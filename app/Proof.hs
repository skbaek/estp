{-# LANGUAGE OverloadedStrings #-}

import Types
import Basic
import PP
import Parse
import Control.Applicative ( Alternative((<|>)) )
import Data.ByteString.Builder as BD (Builder, writeFile, hPutBuilder)
import Data.ByteString.Conversion (toByteString')
import Data.Map (insert)
import Control.Monad (guard, mzero, when)
import System.IO (Handle, openFile, hClose, IOMode(WriteMode))
import Data.ByteString as BS (ByteString, readFile, break, null)

-- Diff display

neqVars :: [BS] -> [BS] -> Builder
neqVars vs ws = ppList ft vs <> "\n!=\n" <> ppList ft ws

neqAppend :: Builder ->  Builder -> Builder
neqAppend b c = b <> "\n------------------------------------------\n" <> c

neqForm :: Form -> Form -> Builder -> Builder
neqForm f g = neqAppend (ppForm f <> "\n!=\n" <> ppForm g) 

neqTerm :: Term -> Term -> Builder -> Builder
neqTerm f g  = neqAppend (ppTerm f <> "\n!=\n" <> ppTerm g) 
diffFunct :: Funct -> Funct -> Maybe Builder
diffFunct (Reg t) (Idx k) = return $ "Reg " <> ft t <> " != Idx " <> ppInt k
diffFunct (Idx k) (Reg t) = return $ "Idx " <> ppInt k <> " != Reg " <> ft t 

diffFunct (Idx k) (Idx m) = do
  guard $ k /= m
  return $ ppInt k <> " != " <> ppInt m
diffFunct (Reg t) (Reg s) = do
  guard $ t /= s
  return $ ft t <> " != " <> ft s

diffTerms :: [Term] -> [Term] -> Maybe Builder
diffTerms [] [] = mzero
diffTerms (x : xs) (y : ys) = diffTerm x y <|> diffTerms xs ys
diffTerms _ _ = Just "unequal number of terms"

diffTerm :: Term -> Term -> Maybe Builder
diffTerm x@(Var _) y@(Fun _ _) = return $ ppTerm x <> " != " <> ppTerm y
diffTerm x@(Fun _ _) y@(Var _) = return $ ppTerm x <> " != " <> ppTerm y
diffTerm x@(Var v) y@(Var w) = do 
  guard $ v /= w 
  return $ ppTerm x <> " != " <> ppTerm y
diffTerm x@(Fun f xs) y@(Fun g ys) = 
  neqTerm x y <$> ( diffFunct f g <|> (zipM xs ys >>= first (uncurry diffTerm)) )

diffJunct :: [Form] -> [Form] -> Maybe Builder
diffJunct [] [] = mzero
diffJunct (f : fs) (g : gs) = diffForm f g <|> diffJunct  fs gs
diffJunct _ _ = Just "unequal number of conjuncts/disjuncts"

diffForm :: Form -> Form -> Maybe Builder
diffForm f@(Eq x y) g@(Eq a b) = neqForm f g <$> (diffTerm x a <|> diffTerm y b)
diffForm f@(Rel r xs) g@(Rel s ys) = neqForm f g <$> (diffFunct r s <|> diffTerms xs ys)
diffForm f@(Iff fl fr) g@(Iff gl gr) = neqForm f g <$> (diffForm fl gl <|> diffForm fr gr)
diffForm (Not f) (Not g) = neqForm (Not f) (Not g) <$> diffForm f g
diffForm (Ex vs f) (Ex ws g) = 
  neqForm (Ex vs f) (Ex ws g) <$> (if vs == ws then diffForm f g else return (neqVars vs ws))
diffForm (Fa vs f) (Fa ws g) = 
  neqForm (Fa vs f) (Fa ws g) <$> (if vs == ws then diffForm f g else return (neqVars vs ws))
diffForm (Imp fa fc) (Imp ga gc) = 
  neqForm (Imp fa fc) (Imp ga gc) <$> (diffForm fa ga <|> diffForm fc gc)
diffForm (And fs) (And gs) = neqForm (And fs) (And gs) <$> diffJunct fs gs
diffForm (Or fs) (Or gs) = neqForm (Or fs) (Or gs) <$> diffJunct fs gs
diffForm f g = Just $ "Default case!\n" <> ppForm f <> "\n!=\n" <> ppForm g 

diffSignForm :: (Bool, Form) -> (Bool, Form) -> Maybe Builder
diffSignForm (True, _) (False, _) = Just "diff sign"
diffSignForm (False, _) (True, _) = Just "diff sign"
diffSignForm (True, f) (True, g) = diffForm f g
diffSignForm (False, f) (False, g) = diffForm f g

diffTrail :: (Bool, Form) -> (Bool, Form) -> BS
diffTrail x y = 
  case diffSignForm x y of 
    Just b -> toByteString' b 
    _ -> error "cannot find diff"

-- Read a ESTP file and stream it out as an ASTP file.
streamProof :: Handle -> [BS] -> BS.ByteString -> IO BS
streamProof h [] bs = return bs
streamProof h (nm : nms) bs = do
  (i, bs') <- cast $ parse elabFormInf bs
  BD.hPutBuilder h (serBS nm)
  let (ibs, pnms) = serInf i
  BD.hPutBuilder h ibs
  streamProof h (pnms ++ nms) bs'

assemblePerm :: Bool -> String -> String -> IO ()
assemblePerm vb enm anm = do
  when vb $ ps "Reading in elaborated formulas...\n"
  estp <- readEstp enm
  when vb $ ps "Asssembling proof...\n"
  prf <- assemble estp "root"
  when vb $ ps "Writing proof to file...\n"
  writeProof anm prf

assembleOrd :: String -> String -> IO ()
assembleOrd enm anm = do
  bs <- BS.readFile enm
  h <- openFile anm WriteMode
  streamProof h ["root"] bs >>= guard . BS.null
  hClose h

stext :: Parser BS
stext = Parser $
  \ tx ->
    case BS.break ((== '.') . w2c) tx of
      (pfx, _ :> sfx) -> Just (pfx, sfx)
      _ -> error "No full stop found"

scheck :: Bool -> Int -> Branch -> Bool -> Form -> Parser ()
scheck vb k bch sgn f = do
  nm <- stext
  let bch' = insert nm (sgn, f) bch
  r <- rule <|> error "cannot read rule"
  scheck'  vb k bch' r

sform :: Parser Form
sform = item >>= sform'

sterm :: Parser Term
sterm = item >>= sterm'

sterm' :: Char -> Parser Term
sterm' '$' = Var <$> stext
sterm' '@' = do
  f <- getFunct
  Fun f <$> slist sterm
sterm' _ = error "cannot get term"

slist :: Parser a -> Parser [a]
slist p = (char '.' >> unit []) <|>
  ( do char ','
       x <- p
       xs <- slist p
       unit (x : xs) )

proof' :: Branch -> Node -> BS -> Parser Proof
proof' bch ni "I" = (Id_ ni <$$> stext) stext
proof' bch ni "C" = do
  f <- sform
  pf <- proof bch False f
  pt <- proof bch True f
  return $ Cut_ ni f pf pt
proof' bch ni "D" = do
  f <- sform
  p <- proof bch True f
  return $ RelD_ ni f p
proof' bch ni "A" = do
  x <- sterm
  f <- sform
  p <- proof bch True f
  return $ AoC_ ni x f p
proof' bch ni "O" = return $ Open_ ni
proof' bch ni "F" = (FunC_ ni <$$> slist stext) stext
proof' bch ni "R" = (RelC_ ni <$$$> slist stext) stext stext
proof' bch ni "=R" = EqR_ ni <$> stext
proof' bch ni "=S" = (EqS_ ni <$$> stext) stext
proof' bch ni "=T" = (EqT_ ni <$$$> stext) stext stext
proof' bch ni "~T" = do
  nm <- stext
  (True, Not f) <- fetch bch nm
  p <- proof bch False f
  return $ NotT_ ni nm p
proof' bch ni "~F" = do
  nm <- stext
  (False, Not f) <- fetch bch nm
  p <- proof bch True f
  return $ NotF_ ni nm p
proof' bch ni "|T" = do
  nm <- stext
  (True, Or fs) <- fetch bch nm
  ps <- mapM (proof bch True) fs
  return $ OrT_ ni nm ps
proof' bch ni "|F" = do
  nm <- stext
  (False, Or fs) <- fetch bch nm
  m <- getInt
  f <- cast $ nth m fs
  p <- proof bch False f
  return $ OrF_ ni nm m p
proof' bch ni "&T" = do
  nm <- stext
  (True, And fs) <- fetch bch nm
  m <- getInt
  f <- cast $ nth m fs
  p <- proof bch True f
  return $ AndT_ ni nm m p
proof' bch ni "&F" = do
  nm <- stext
  (False, And fs) <- fetch bch nm
  ps <- mapM (proof bch False) fs
  return $ AndF_ ni nm ps
proof' bch ni ">T" = do
  nm <- stext
  (True, Imp f g) <- fetch bch nm
  pa <- proof bch False f
  pc <- proof bch True g
  return $ ImpT_ ni nm pa pc
proof' bch ni ">FA" = do
  nm <- stext
  (False, Imp f _) <- fetch bch nm
  p <- proof bch True f
  return $ ImpFA_ ni nm p
proof' bch ni ">FC" = do
  nm <- stext
  (False, Imp _ g) <- fetch bch nm
  p <- proof bch False g
  return $ ImpFC_ ni nm p
proof' bch ni "^TO" = do
  nm <- stext
  (True, Iff f g) <- fetch bch nm
  p <- proof bch True (f ==> g)
  return $ IffTO_ ni nm p
proof' bch ni "^TR" = do
  nm <- stext
  (True, Iff f g) <- fetch bch nm
  p <- proof bch True (g ==> f)
  return $ IffTR_ ni nm p
proof' bch ni "^F" = do
  nm <- stext
  (False, Iff f g) <- fetch bch nm
  po <- proof bch False (f ==> g)
  pr <- proof bch False (g ==> f)
  return $ IffF_ ni nm po pr
proof' bch ni "!T" = do
  nm <- stext
  (True, Fa vs f) <- fetch bch nm
  xs <- slist sterm
  vxs <- zipM vs xs
  let f' = substForm vxs f
  p <- proof bch True f'
  return $ FaT_ ni nm xs p
proof' bch ni "!F" = do
  nm <- stext
  (False, Fa vs f) <- fetch bch nm
  m <- getInt
  let (_, vxs) = varPars m vs
  let f' = substForm vxs f
  p <- proof bch False f'
  return $ FaF_ ni nm m p
proof' bch ni "?T" = do
  nm <- stext
  (True, Ex vs f) <- fetch bch nm
  m <- getInt
  let (_, vxs) = varPars m vs
  let f' = substForm vxs f
  p <- proof bch True f'
  return $ ExT_ ni nm m p
proof' bch ni "?F" = do
  nm <- stext
  (False, Ex vs f) <- fetch bch nm
  xs <- slist sterm
  vxs <- zipM vs xs
  let f' = substForm vxs f
  p <- proof bch False f'
  return $ ExF_ ni nm xs p
proof' bch ni _ = error "invalid rule"

getInt :: Parser Int
getInt = do
  tx <- stext
  cast $ bs2int tx

getFunct :: Parser Funct
getFunct = (char '#' >> (Idx <$> getInt)) <|> (Reg <$> stext)
sform' :: Char -> Parser Form
sform' '=' = do
  x <- sterm
  y <- sterm
  unit $ Eq x y
sform' '@' = do
  r <- getFunct
  Rel r <$> slist sterm
sform' '~' = Not <$> sform
sform' '|' = Or <$> slist sform
sform' '&' = And <$> slist sform
sform' '>' = do
  f <- sform
  g <- sform
  unit $ Imp f g
sform' '^' = do
  f <- sform
  g <- sform
  unit $ Iff f g
sform' '!' = do
  vs <- slist stext
  Fa vs <$> sform
sform' '?' = do
  vs <- slist stext
  Ex vs <$> sform
sform' c = error $ "invalid head character : " <> [c]

scheck'  :: Bool -> Int -> Branch -> BS -> Parser ()
scheck'  vb k bch "I" = do
  nt <- stext
  nf <- stext
  ft <- fetch bch nt
  ff <- fetch bch nf
  pguard "id-fail" $ complementary ft ff
scheck'  vb k bch "C" = do
  f <- sform
  scheck vb k bch False f
  scheck vb k bch True f
scheck'  vb k bch "D" = do
  f <- sform
  k' <- cast $ checkRelD k f
  scheck vb k' bch True f
scheck'  vb k bch "A" = do
  x <- sterm
  f <- sform
  k' <- cast $ checkAoC k x f
  scheck vb k' bch True f
scheck'  vb k bch "O" = skip
scheck'  vb k bch "F" = do
  eqns <- slist stext >>= mapM (fetch bch)
  (False, Eq (Fun f xs) (Fun g ys)) <- stext >>= fetch bch
  pguard "function symbol mismatch" $ f == g
  xys <- cast $ mapM breakTrueEq eqns
  xys' <- zipM xs ys
  pguard "arguments mismatch" $ xys == xys'
scheck'  vb k bch "R" = do
  eqns <- slist stext >>= mapM (fetch bch)
  (True, Rel r xs) <- stext >>= fetch bch
  (False, Rel s ys) <- stext >>= fetch bch
  pguard "relation symbol mismatch" $ r == s
  xys <- cast $ mapM breakTrueEq eqns
  xys' <- zipM xs ys
  pguard "arguments mismatch" $ xys == xys'
scheck'  vb k bch "=R" = do
  (False, Eq x y) <- stext >>= fetch bch
  guard $ x == y
scheck'  vb k bch "=S" = do
  (True, Eq x y) <- stext >>= fetch bch
  (False, Eq y' x') <- stext >>= fetch bch
  guard $ x == x' && y == y'
scheck'  vb k bch "=T" = do
  (True, Eq x y) <- stext >>= fetch bch
  (True, Eq y' z) <- stext >>= fetch bch
  (False, Eq x' z') <- stext >>= fetch bch
  guard $ x == x' && y == y' && z == z'
scheck'  vb k bch "~T" = do
  (True, Not f) <- stext >>= fetch bch
  scheck vb k bch False f
scheck'  vb k bch "~F" = do
  (False, Not f) <- stext >>= fetch bch
  scheck vb k bch True f
scheck'  vb k bch "|T" = do
  nm <- stext
  (True, Or fs) <- fetch bch nm
  mapM_ (scheck vb k bch True) fs
scheck'  vb k bch "|F" = do
  (False, Or fs) <- stext >>= fetch bch
  m <- getInt
  f <- cast $ nth m fs
  scheck vb k bch False f
scheck'  vb k bch "&T" = do
  (True, And fs) <- stext >>= fetch bch
  m <- getInt
  f <- cast $ nth m fs
  scheck vb k bch True f
scheck'  vb k bch "&F" = do
  (False, And fs) <- stext >>= fetch bch
  mapM_ (scheck vb k bch False) fs
scheck'  vb k bch ">T" = do
  (True, Imp f g) <- stext >>= fetch bch
  scheck vb k bch False f
  scheck vb k bch True g
scheck'  vb k bch ">FA" = do
  nm <- stext
  (sgn, fg) <- fetch bch nm
  case (sgn, fg) of
    (False, Imp f g) -> scheck vb k bch True f
    _ -> error "not false imp"
scheck'  vb k bch ">FC" = do
  (False, Imp _ g) <- stext >>= fetch bch
  scheck vb k bch False g
scheck'  vb k bch "^TO" = do
  (True, Iff f g) <- stext >>= fetch bch
  scheck vb k bch True (f ==> g)
scheck'  vb k bch "^TR" = do
  (True, Iff f g) <- stext >>= fetch bch
  scheck vb k bch True (g ==> f)
scheck'  vb k bch "^F" = do
  (False, Iff f g) <- stext >>= fetch bch
  scheck vb k bch False (f ==> g)
  scheck vb k bch False (g ==> f)
scheck'  vb k bch "!T" = do
  (True, Fa vs f) <- stext >>= fetch bch
  xs <- slist sterm
  vxs <- zipM vs xs
  let f' = substForm vxs f
  scheck vb k bch True f'
scheck'  vb k bch "!F" = do
  (False, Fa vs f) <- stext >>= fetch bch
  m <- getInt
  guard $ k <= m
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "!F-fail : cannot zip"
  let f' = substForm vxs f
  scheck vb k' bch False f'
scheck'  vb k bch "?T" = do
  (True, Ex vs f) <- stext >>= fetch bch
  m <- getInt
  pguard "index scheck failed for ?T" (k <= m)
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "?T-fail : cannot zip"
  let f' = substForm vxs f
  scheck vb k' bch True f'
scheck'  vb k bch "?F" = do
  nm <- stext
  (False, Ex vs f) <- fetch bch nm
  xs <- slist sterm
  vxs <- zipM vs xs
  let f' = substForm vxs f
  scheck vb k bch False f'
scheck'  vb k bch _ = error "impossible case"

proof :: Branch -> Bool -> Form -> Parser Proof
proof bch sgn f = do
  nm <- stext
  let bch' = insert nm (sgn, f) bch
  r <- rule <|> error "cannot read rule"
  proof' bch' (nm, sgn, f) r