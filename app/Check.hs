{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Check where

import Types
import Basic
import PP (ppSignForm, ppElab, ppForm, ppTerm, ppFunct, ppList, ppListNl, ppSetNl, ppNL)
import Data.Text.Lazy as T (Text, intercalate)
import Data.Map as HM (Map, lookup, insert)
import Data.Set as S (Set, insert, toList, member, fromList, size)
import Data.List as L (all, map, length, foldl)
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

neqVars :: [Text] -> [Text] -> Builder
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

checkSkolemTerm :: [Text] -> Int -> Term -> IO Int
checkSkolemTerm vs k (Var _) = mzero
checkSkolemTerm vs k (Fun (Reg _) _) = mzero
checkSkolemTerm vs k (Fun (Idx m) xs) = do
  guard $ k <= m
  ws <- cast $ mapM breakVar xs
  guard $ sublist ws vs
  return $ m + 1

checkAoC :: Int -> Term -> Form -> IO Int

checkAoC k x (Fa vs (Imp (Ex [w] f) g)) = do
  guard $ distintList (w : vs)
  k' <- checkSkolemTerm vs k x 
  guard $ substForm [(w, x)] f == g
  return k'

checkAoC k x (Fa vs (Imp (Ex (w : ws) f) g)) = do
  guard $ distintList (w : ws ++ vs)
  k' <- checkSkolemTerm vs k x 
  guard $ substForm [(w, x)] (Ex ws f) == g
  return k'

checkAoC k x (Imp (Ex [w] f) g) = do
  k' <- checkSkolemTerm [] k x
  guard $ substForm [(w, x)] f == g
  return k'

checkAoC k x (Imp (Ex (w : ws) f) g) = do
  guard $ distintList (w : ws)
  k' <- checkSkolemTerm [] k x
  guard $ substForm [(w, x)] (Ex ws f) == g
  return k'
checkAoC k xs _ = mzero

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
check vb k bch (AoC_ _ x prf) = do 
  let (True, f) = rootSignForm prf
  k' <- checkAoC k x f
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
  guard $ k <= m
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

{- Verification -}

isRelD :: Form -> Bool
isRelD (Fa vs (Iff (Rel s xs) f)) = L.map Var vs == xs && isGndForm vs f
isRelD (Iff (Rel s []) f) = isGndForm [] f
isRelD _ = False

verifyEqGoal :: Int -> Set Form -> Set Form -> (Term, Term, Prf) -> IO ()
verifyEqGoal k lft rgt (x, y, p) = verify k lft (S.insert (Eq x y) rgt) p

verifyRgtGoal :: Int -> Set Form -> Set Form -> (Form, Prf) -> IO ()
verifyRgtGoal k lft rgt (f, p) = verify k lft (S.insert f rgt) p

verifyLftGoal :: Int -> Set Form -> Set Form -> (Form, Prf) -> IO ()
verifyLftGoal k lft rgt (f, p) = verify k (S.insert f lft) rgt p

ev :: Text -> Form -> Set Form -> Set Form -> IO ()
ev t f lhs rhs = eb $ ft t <> ppForm f <> "\nLHS :\n" <> ppListNl (ppNL ppForm) (S.toList lhs) <> "\nRHS :\n" <> ppListNl (ppNL ppForm) (S.toList rhs) <> "\n"

verify :: Int -> Set Form -> Set Form -> Prf -> IO ()
verify _ _ _ Open' = return ()
verify _ lft rgt (Id' f) = do
  let lhs_text = ppListNl ppForm (S.toList lft)
  let rhs_text = ppListNl ppForm (S.toList rgt)
  guard (S.member f lft) <|> eb ("Id' fail, LHS missing : " <> ppForm f <> "\nLHS = " <> lhs_text)
  guard (S.member f rgt) <|> eb ("Id' fail, RHS missing : " <> ppForm f <> "\nRHS = " <> rhs_text)
verify _ lft rgt (EqR' x) = guard (S.member (Eq x x) rgt) <|> error "EqR'-fail"
verify k lft rgt (EqS' x y) =
  guard (S.member (Eq x y) lft && S.member (Eq y x) rgt) <|> error "EqS'-fail"
verify k lft rgt (EqT' x y z) =
  guard (S.member (Eq x y) lft && S.member (Eq y z) lft && S.member (Eq x z) rgt) <|> error "EqT'-fail"
verify k lft rgt (FunC' f xs ys) = do
  xys <- zipM xs ys
  guardMsg "Fun-C : premise missing" $ L.all (\ (x_, y_) -> S.member (x_ === y_) lft) xys
  guardMsg "Fun-C : conclusion missing" $ S.member (Fun f xs === Fun f ys) rgt
verify k lft rgt (RelC' r xs ys) = do
  xys <- zipM xs ys
  guardMsg "Fun-C : eq-premise missing" $ L.all (\ (x_, y_) -> S.member (x_ === y_) lft) xys
  guardMsg "Fun-C : premise missing" $ S.member (Rel r xs) lft
  guardMsg "Fun-C : conclusion missing" $ S.member (Rel r ys) rgt
verify k lft rgt (NotT' f p) = do
  guard (S.member (Not f) lft) <|> error "NotT'-fail"
  verify k lft (S.insert f rgt) p
verify k lft rgt (NotF' f p) = do
  guard (S.member (Not f) rgt) <|> eb ("NotF'-fail\nCannot find hyp : " <> ppSignForm (False, Not f) <> "\nFrom :\n" <> ppSetNl ppForm rgt)
  verify k (S.insert f lft) rgt p
verify k lft rgt (OrT' gls) = do
  let fs = L.map fst gls
  guard (S.member (Or fs) lft) <|> eb ("OrT'-fail : " <> ppForm (Or fs))
  mapM_ (verifyLftGoal k lft rgt) gls
verify k lft rgt (OrF' fs gs p) = do
  guard (sublist gs fs && S.member (Or fs) rgt) <|> error "OrF'-fail"
  verify k lft (L.foldl (flip S.insert) rgt gs) p
verify k lft rgt (AndT' fs gs p) = do
  guard (sublist gs fs) <|> error "AndT'-fail : not subset"
  guard (S.member (And fs) lft) <|> ev "AndT'-fail : " (And fs) lft rgt
  verify k (L.foldl (flip S.insert) lft gs) rgt p
verify k lft rgt (AndF' gls) = do
  let fs = L.map fst gls
  guard (S.member (And fs) rgt) <|> ev "AndF'-fail" (And fs) lft rgt
  mapM_ (verifyRgtGoal k lft rgt) gls
verify k lft rgt (ImpT' f g p q) = do
  guard (S.member (Imp f g) lft) <|> ev "ImpT'-fail" (f ==> g) lft rgt
  verify k lft (S.insert f rgt) p
  verify k (S.insert g lft) rgt q

verify k lft rgt (ImpFA' f g p) = do
  guard (S.member (Imp f g) rgt) <|> error "ImpFA'-fail"
  verify k (S.insert f lft) rgt p

verify k lft rgt (ImpFC' f g p) = do
  guard (S.member (Imp f g) rgt) <|> error "ImpFC'-fail"
  verify k lft (S.insert g rgt) p

verify k lft rgt (IffF' f g p q) = do
  guard (S.member (Iff f g) rgt) <|> ev "IffF'-fail" (f <=> g) lft rgt
  verify k lft (S.insert (Imp f g) rgt) p
  verify k lft (S.insert (Imp g f) rgt) q
verify k lft rgt (IffTO' f g p) = do
  guard (S.member (Iff f g) lft) <|> ev "IffTO'-fail : " (f <=> g) lft rgt
  verify k (S.insert (Imp f g) lft) rgt p
verify k lft rgt (IffTR' f g p) = do
  guard (S.member (f <=> g) lft) <|> ev "IffTR'-fail : " (f <=> g) lft rgt
  verify k (S.insert (Imp g f) lft) rgt p
verify k lft rgt (FaT' vxs f p) = do
  let vs = L.map fst vxs
  guard (S.member (Fa vs f) lft) <|> ev "FaT'-fail : " (Fa vs f) lft rgt
  verify k (S.insert (substForm vxs f) lft) rgt p
verify k lft rgt (FaF' vs m f p) = do
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "FaF'-fail : cannot zip"
  guard (k <= m && S.member (Fa vs f) rgt) <|> ev "FaF'-fail" (Fa vs f) lft rgt
  verify k' lft (S.insert (substForm vxs f) rgt) p
verify k lft rgt (ExT' vs m f p) = do
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "ExT'-fail"
  guard (k <= m && S.member (Ex vs f) lft) <|> error "ExT'-fail"
  verify k' (S.insert (substForm vxs f) lft) rgt p
verify k lft rgt (ExF' vxs f p) = do
  let vs = L.map fst vxs
  guard (S.member (Ex vs f) rgt) <|> error "ExF'-fail"
  verify k lft (S.insert (substForm vxs f) rgt) p
verify k lft rgt (Cut' f p0 p1) = do
  verify k lft (S.insert f rgt) p0
  verify k (S.insert f lft) rgt p1
verify k lft rgt (Mrk s p) = verify k lft rgt p