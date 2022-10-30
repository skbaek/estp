{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Check where

import Types
import Basic
import PP (ppSignForm, ppElab, ppForm, ppTerm, ppFunct)
import Data.Text.Lazy as T (Text, intercalate)
import Data.Map as HM (Map, lookup, insert)
import Data.Set as S (fromList, size)
import Data.List as L (all, map, length)
import Control.Monad as M ( guard, when, mzero )
import Control.Applicative ( Alternative((<|>)) )
import Data.Text.Lazy.Builder as B (Builder)

-- subEPRec :: EP -> EP -> Bool
-- subEPRec (0, []) _ = False
-- subEPRec (0, (_, m) : l) ep = subEP (m, l) ep
-- subEPRec (k, l) ep = subEP (k - 1, l) ep
-- 
-- subEP :: EP -> EP -> Bool
-- subEP ep ep'
--   | ep == ep' = True
--   | otherwise = subEPRec ep ep'
-- 
-- onPath :: EP -> Text -> Bool
-- onPath ep nm =
--   case readEp nm of
--     Just ep' -> subEP ep ep'
--     _ -> True
-- 
-- pathLookup :: EP -> Nodes -> Maybe (Form, Bool, Int)
-- pathLookup = HM.lookup . pathText 
-- 
-- pathText :: EP -> Text
-- pathText = tlt . ppEP

rootName :: Proof -> Text
rootName prf = 
  let (n, _, _) = rootNode prf in 
  n

rootSignForm :: Proof -> (Bool, Form)
rootSignForm prf = 
  let (_, b, f) = rootNode prf in 
  (b, f)

complementary :: SignForm -> SignForm -> Bool
complementary (True, f) (False, g) = f == g 
complementary _ _ = False

neqForm :: Form -> Form -> Builder -> Builder
neqForm f g b = ppForm f <> " != " <> ppForm g <> "\n" <> b

neqTerm :: Term -> Term -> Builder -> Builder
neqTerm f g b = ppTerm f <> " != " <> ppTerm g <> "\n" <> b

diffFunct :: Funct -> Funct -> Maybe Builder
diffFunct (Reg t) (Idx k) = return $ "Reg " <> ft t <> " != Idx " <> ppInt k
diffFunct (Idx k) (Reg t) = return $ "Idx " <> ppInt k <> " != Reg " <> ft t 

diffFunct (Idx k) (Idx m) = do
  guard $ k /= m
  return $ ppInt k <> " != " <> ppInt m
diffFunct (Reg t) (Reg s) = do
  guard $ t /= s
  return $ ft t <> " != " <> ft s

diffTerm :: Term -> Term -> Maybe Builder
diffTerm x@(Var _) y@(Fun _ _) = return $ ppTerm x <> " != " <> ppTerm y
diffTerm x@(Fun _ _) y@(Var _) = return $ ppTerm x <> " != " <> ppTerm y
diffTerm x@(Var v) y@(Var w) = do 
  guard $ v /= w 
  return $ ppTerm x <> " != " <> ppTerm y
diffTerm x@(Fun f xs) y@(Fun g ys) = 
  neqTerm x y <$> ( diffFunct f g <|> (zipM xs ys >>= first (uncurry diffTerm)) )

diffForm :: Form -> Form -> Maybe Builder
diffForm f@(Eq x y) g@(Eq a b) = neqForm f g <$> (diffTerm x a <|> diffTerm y b)
diffForm f@(Iff fl fr) g@(Iff gl gr) = neqForm f g <$> (diffForm fl gl <|> diffForm fr gr)
diffForm (Not f) (Not g) = neqForm (Not f) (Not g) <$> diffForm f g
diffForm f g = error "todo"

diffSignForm :: (Bool, Form) -> (Bool, Form) -> Maybe Builder
diffSignForm (True, _) (False, _) = Just "diff sign"
diffSignForm (False, _) (True, _) = Just "diff sign"
diffSignForm (True, f) (True, g) = diffForm f g
diffSignForm (False, f) (False, g) = diffForm f g

diffTrail :: (Bool, Form) -> (Bool, Form) -> Text
diffTrail x y = 
  case diffSignForm x y of 
    Just b -> tlt b 
    _ -> error "cannot find diff"

check_ :: Bool -> Int -> Branch -> SignForm -> Proof -> IO ()
check_ vb k bch sf prf = do
  let nm = rootName prf 
  when vb (pt $ "Checking node : " <> nm <> "\n")
  let sf' = rootSignForm prf 
  guardMsg ("Actual not as expected, diff trail:\n" <> diffTrail sf sf') $ sf == sf'
  let bch' = HM.insert (rootName prf) sf bch 
  check vb k bch' prf


functIdxLT :: Int -> Funct -> Bool
functIdxLT k (Reg _) = True
functIdxLT k (Idx m) = m < k

termIdxLT :: Int -> Term -> Bool
termIdxLT _ (Var _) = True
termIdxLT k (Fun f xs) = functIdxLT k f && L.all (termIdxLT k) xs 

formIdxLT :: Int -> Form -> Bool
formIdxLT k (Eq x y) = termIdxLT k x && termIdxLT k y
formIdxLT k (Rel r xs) = functIdxLT k r && L.all (termIdxLT k) xs 
formIdxLT k (Not f) = formIdxLT k f
formIdxLT k (Or fs) = L.all (formIdxLT k) fs
formIdxLT k (And fs) = L.all (formIdxLT k) fs
formIdxLT k (Imp f g) = formIdxLT k f && formIdxLT k g
formIdxLT k (Iff f g) = formIdxLT k f && formIdxLT k g
formIdxLT k (Fa _ f) = formIdxLT k f 
formIdxLT k (Ex _ f) = formIdxLT k f 

checkRelD :: Int -> Form -> IO Int
checkRelD k (Fa vs (Iff (Rel (Idx m) xs) f)) = do 
  guard $ k <= m
  ws <- cast $ mapM breakVar xs
  guard $ sublist ws vs
  guard $ isGndForm ws f
  guard $ formIdxLT k f
  return $ m + 1
checkRelD k (Iff (Rel (Idx m) []) f) = do 
  guard $ k <= m
  guard $ isGndForm [] f
  guard $ formIdxLT k f
  return $ m + 1
checkRelD _ _ = mzero

distintList :: (Ord a) => [a] -> Bool
distintList xs = S.size (S.fromList xs) == L.length xs

checkSkolemTerms :: [Text] -> Int -> [Term] -> IO Int
checkSkolemTerms vs k [] = return k
checkSkolemTerms vs k (Var _ : _) = mzero
checkSkolemTerms vs k (Fun (Reg _) _ : _) = mzero
checkSkolemTerms vs k (Fun (Idx m) xs : ys) = do
  guard $ k <= m
  ws <- cast $ mapM breakVar xs
  guard $ sublist ws vs
  checkSkolemTerms vs (m + 1) ys 

checkAoC :: Int -> [Term] -> Form -> IO Int
checkAoC k xs (Fa vs (Imp (Ex ws f) g)) = do
  guard $ distintList (vs ++ ws)
  k' <- checkSkolemTerms vs k xs 
  wxs <- zipM ws xs
  guard $ substForm wxs f == g
  return k'
checkAoC k xs (Imp (Ex ws f) g) = do
  guard $ distintList ws
  k' <- checkSkolemTerms [] k xs 
  wxs <- zipM ws xs
  guard $ substForm wxs f == g
  return k'
checkAoC k xs _ = mzero

-- isAoC' :: [Term] -> Form -> IO ()
-- isAoC' xs (Fa vs (Imp (Ex ws f) g)) = do
--   guard $ L.all (isSkolemTerm vs) xs
--   wxs <- zipM ws xs
--   guard $ substForm wxs f == g
-- isAoC' xs (Imp (Ex ws f) g) = do
--   guard $ L.all isConstant xs
--   wxs <- zipM ws xs
--   guard $ substForm wxs f == g
-- isAoC' _ _ = mzero

breakTrueEq :: SignForm -> IO (Term, Term)
breakTrueEq (True, Eq x y) = return (x, y)
breakTrueEq _ = mzero

check :: Bool -> Int -> Branch -> Proof -> IO ()
check _ _ bch (Id_ _ nt nf) = do 
  tf <- cast $ HM.lookup nt bch
  ff <- cast $ HM.lookup nf bch
  guard $ complementary tf ff
check vb k bch (Cut_ _ pt pf) = do 
  let (_, f) = proofRSF pt 
  check_ vb k bch (False, f) pt
  check_ vb k bch (True, f) pf
check vb k bch (OrT_ _ nm prfs) = do 
  (True, Or fs) <- cast $ HM.lookup nm bch 
  mapM2 (check_ vb k bch . (True,)) fs prfs
  skip
check vb k bch (OrF_ _ nm m prf) = do 
  (False, Or fs) <- cast $ HM.lookup nm bch 
  f <- cast $ nth m fs 
  check_ vb k bch (False, f) prf
check vb k bch (AndT_ _ nm m prf) = do 
  (True, And fs) <- cast $ HM.lookup nm bch 
  f <- cast $ nth m fs 
  check_ vb k bch (True, f) prf
check vb k bch (AndF_ _ nm prfs) = do 
  (False, And fs) <- cast $ HM.lookup nm bch 
  mapM2 (check_ vb k bch . (False,)) fs prfs
  skip
check vb k bch (RelD_ _ prf) = do 
  pb $ "k at Rel-D : " <> ppInt k <> "\n"
  let (True, f) = rootSignForm prf
  k' <- checkRelD k f
  check_ vb k' bch (True, f) prf
check vb k bch (AoC_ _ xs prf) = do 
  let (True, f) = rootSignForm prf
  k' <- checkAoC k xs f
  check_ vb k' bch (True, f) prf
check vb k bch (ImpT_ _ nm pa pc) = do 
  (True, Imp f g) <- cast $ HM.lookup nm bch 
  check_ vb k bch (False, f) pa
  check_ vb k bch (True, g) pc
check vb k bch (ImpFA_ _ nm prf) = do 
  (False, Imp f _) <- cast $ HM.lookup nm bch 
  check_ vb k bch (True, f) prf
check vb k bch (ImpFC_ _ nm prf) = do 
  (False, Imp _ g) <- cast $ HM.lookup nm bch 
  check_ vb k bch (False, g) prf
check vb k bch (IffTO_ _ nm prf) = do 
  (True, Iff f g) <- cast $ HM.lookup nm bch 
  check_ vb k bch (True, Imp f g) prf
check vb k bch (IffTR_ _ nm prf) = do 
  (True, Iff f g) <- cast $ HM.lookup nm bch 
  check_ vb k bch (True, Imp g f) prf
check vb k bch (IffF_ _ nm po pr) = do 
  (False, Iff f g) <- cast $ HM.lookup nm bch 
  check_ vb k bch (False, Imp f g) po
  check_ vb k bch (False, Imp g f) pr
check vb k bch (NotT_ _ nm prf) = do 
  (True, Not f) <- cast $ HM.lookup nm bch 
  check_ vb k bch (False, f) prf
check vb k bch (NotF_ _ nm prf) = do 
  (False, Not f) <- cast $ HM.lookup nm bch 
  check_ vb k bch (True, f) prf
check vb k bch (FunC_  _ nms nm) = do 
  (False, Eq (Fun f xs) (Fun g ys)) <- cast $ HM.lookup nm bch 
  guard $ f == g
  seqs <- cast $ mapM (`HM.lookup` bch) nms
  xys <- mapM breakTrueEq seqs
  xys' <- zipM xs ys
  guard $ xys == xys'
check vb k bch (RelC_  _ nms nt nf) = do 
  (True, Rel r xs) <- cast $ HM.lookup nt bch 
  (False, Rel s ys) <- cast $ HM.lookup nf bch 
  guard $ r == s
  seqs <- cast $ mapM (`HM.lookup` bch) nms
  xys <- mapM breakTrueEq seqs
  xys' <- zipM xs ys
  guard $ xys == xys'
check vb k bch (EqR_ _ nm) = do 
  (False, Eq x y) <- cast $ HM.lookup nm bch
  guard $ x == y
check vb k bch (EqS_ _ nt nf) = do 
  (True, Eq x y) <- cast $ HM.lookup nt bch 
  (False, Eq y' x') <- cast $ HM.lookup nf bch 
  guard $ x == x' && y == y'
check vb k bch (EqT_ _ nxy nyz nxz) = do 
  (True, Eq x y) <- cast $ HM.lookup nxy bch 
  (True, Eq y' z) <- cast $ HM.lookup nyz bch 
  (False, Eq x' z') <- cast $ HM.lookup nxz bch 
  guard $ x == x' && y == y' && z == z'
check vb k bch (FaT_ _ nm xs prf) = do 
  (True, Fa vs f) <- cast $ HM.lookup nm bch 
  vxs <- zipM vs xs 
  let f' = substForm vxs f
  check_ vb k bch (True, f') prf
check vb k bch (FaF_ _ nm m prf) = do 
  pb $ "k = " <> ppInt k <> "\n"
  pb $ "m = " <> ppInt m <> "\n"
  guard $ k <= m
  pb $ "faf-success\n"
  (False, Fa vs f) <- cast $ HM.lookup nm bch 
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "FaF'-fail : cannot zip"
  let f' = substForm vxs f
  check_ vb k' bch (False, f') prf
check vb k bch (ExT_ _ nm m prf) = do 
  guard $ k <= m
  (True, Ex vs f) <- cast $ HM.lookup nm bch 
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "FaF'-fail : cannot zip"
  let f' = substForm vxs f
  check_ vb k' bch (True, f') prf
check vb k bch (ExF_ _ nm xs prf) = do 
  (False, Ex vs f) <- cast $ HM.lookup nm bch 
  vxs <- zipM vs xs 
  let f' = substForm vxs f
  check_ vb k bch (False, f') prf
check _ _ _ (Open_ _) = skip