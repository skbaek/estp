{-# LANGUAGE OverloadedStrings #-}

module Elab where

import Types
import Basic 
import PP (ppForm, ppListNl, ppEP)
import Prove
import Sat (sat)
import Data.List as L (all, map, foldl)
import Data.Map as HM (insert, lookup)
import Data.Set as S (insert, singleton, toList, member)
import Data.Text.Lazy as T (Text)
import Data.Text.Lazy.Builder (Builder)
import Control.Monad as M ( guard, MonadPlus(mzero), foldM_, when )
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative ( Alternative((<|>)) )
import Data.Bifunctor as DBF (first, second, bimap)


rfsj :: Form -> Form
rfsj (Not f) = Not $ rfsj f
rfsj (Imp f g) = Imp (rfsj f) (rfsj g)
rfsj (Iff f g) = Iff (rfsj f) (rfsj g)
rfsj (Or fs) =
  case L.map rfsj fs of
    [f] -> f
    fs' -> Or fs'
rfsj (And fs) =
  case L.map rfsj fs of
    [f] -> f
    fs' -> And fs'
rfsj (Fa vs f) = Fa vs $ rfsj f
rfsj (Ex vs f) = Ex vs $ rfsj f
rfsj f@(Rel _ _) = f
rfsj f@(Eq _ _) = f

rpsj :: Prf -> Prf
rpsj (Id' f) = Id' $ rfsj f
rpsj p@(EqR' _) = p
rpsj p@(EqS' _ _) = p
rpsj p@EqT' {} = p
rpsj p@FunC' {} = p
rpsj p@RelC' {} = p
rpsj (NotT' f p) = NotT' (rfsj f) (rpsj p)
rpsj (NotF' f p) = NotF' (rfsj f) (rpsj p)

rpsj (OrT' [(f, p)]) = rpsj p
rpsj (OrT' ps) = OrT' $ L.map (bimap rfsj rpsj) ps

rpsj (AndF' [(f, p)]) = rpsj p
rpsj (AndF' ps) = AndF' $ L.map (bimap rfsj rpsj) ps

rpsj (AndT' [f] gs p) =
  case gs of
    [g] -> if f == g then rpsj p else et "rpsj-and-lft-0"
    _ -> et "rpsj-and-lft-1"
rpsj (AndT' fs gs p) = AndT' (L.map rfsj fs) (L.map rfsj gs) (rpsj p)

rpsj (OrF' [f] gs p) =
  case gs of
    [g] -> if f == g then rpsj p else et "rpsj-or-rgt-0"
    _ -> et "rpsj-or-rgt-1"
rpsj (OrF' fs gs p) = OrF' (L.map rfsj fs) (L.map rfsj gs) (rpsj p)

rpsj (ImpFA' f g p) = ImpFA' (rfsj f) (rfsj g) (rpsj p)
rpsj (ImpFC' f g p) = ImpFC' (rfsj f) (rfsj g) (rpsj p)
rpsj (ImpT' f g pf pg) = ImpT' (rfsj f) (rfsj g) (rpsj pf) (rpsj pg)
rpsj (IffTO' f g p) = IffTO' (rfsj f) (rfsj g) (rpsj p)
rpsj (IffTR' f g p) = IffTR' (rfsj f) (rfsj g) (rpsj p)
rpsj (IffF' f g pf pg) = IffF' (rfsj f) (rfsj g) (rpsj pf) (rpsj pg)
rpsj (FaT' vxs f p) = FaT' vxs (rfsj f) (rpsj p)
rpsj (ExF' vxs f p) = ExF' vxs (rfsj f) (rpsj p)
rpsj (FaF' xs k f p) = FaF' xs k (rfsj f) (rpsj p)
rpsj (ExT' xs k f p) = ExT' xs k (rfsj f) (rpsj p)
rpsj (Cut' f pl pr) = Cut' (rfsj f) (rpsj pl) (rpsj pr)
rpsj (Mrk t p) = Mrk t $ rpsj p
rpsj Asm = Asm

resj :: Stelab -> Stelab
resj (Plab n p t) = Plab n (rpsj p) t
resj (AoC' xs f g p t) = AoC' xs (rfsj f) (rfsj g) (rpsj p) t
resj (RelD' f g p t) = RelD' (rfsj f) (rfsj g) (rpsj p) t

detectSJ :: EF -> IO ()
detectSJ (ep, _, f, _, _, _)
  | formSJ f = eb $ "single junct at EP : " <> ppEP ep
  | otherwise = return ()
{- Verification -}

isRelD :: Form -> Bool
isRelD (Fa vs (Iff (Rel s xs) f)) = L.map Var vs == xs && isGndForm vs f
isRelD (Iff (Rel s []) f) = isGndForm [] f
isRelD _ = False

verifyEqGoal :: Int -> Seq -> Seq -> (Term, Term, Prf) -> IO ()
verifyEqGoal k lft rgt (x, y, p) = verify k lft (S.insert (Eq x y) rgt) p

verifyRgtGoal :: Int -> Seq -> Seq -> (Form, Prf) -> IO ()
verifyRgtGoal k lft rgt (f, p) = verify k lft (S.insert f rgt) p

verifyLftGoal :: Int -> Seq -> Seq -> (Form, Prf) -> IO ()
verifyLftGoal k lft rgt (f, p) = verify k (S.insert f lft) rgt p

ev :: Text -> Form -> Seq -> Seq -> IO ()
ev t f lhs rhs = eb $ ft t <> ppForm f <> "\nLHS :\n" <> ppListNl ppForm (S.toList lhs) <> "\nRHS :\n" <> ppListNl ppForm (S.toList rhs) <> "\n"

verify :: Int -> Seq -> Seq -> Prf -> IO ()
verify _ _ _ Asm = return ()
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
  guard (S.member (Not f) rgt) <|> error "NotF'-fail"
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
checkStelab :: Seq -> Form -> Stelab -> IO ()
checkStelab sq g (Plab g' p _) = do
  guard (g == g')
  verify 0 sq (S.singleton g) p
checkStelab sq g (RelD' f g' p _) = do
  guard (g == g')
  guard $ isRelD f
  verify 0 (S.singleton f) (S.singleton g) p
checkStelab sq g (AoC' xs f g' p _) = do
  guard $ g == g'
  isAoC' xs f
  verify 0 (S.singleton f) (S.singleton g) p

infer :: Text -> [Form] -> Form -> IO Prf
infer "superposition" [f, g] h         = superpose f g h
infer "forward_demodulation" [f, g] h  = superpose f g h
infer "backward_demodulation" [f, g] h = superpose f g h
infer "negated_conjecture" [f] g = guard (f == g) >> return (Id' f)
infer "factoring" [f] g = efactor (Just True) f g
infer "nnf_transformation" [f] g = nnfTrans False f g
infer "ennf_transformation" [f] g = nnfTrans True f g
infer "true_and_false_elimination" [f] g = trueFalseElim f g
infer "duplicate_literal_removal" [f] g = efactor (Just True) f g
infer "equality_resolution" [f] g = efactor (Just False) f g
infer "trivial_inequality_removal" [f] g = efactor nt f g
infer "subsumption_resolution" [f, g] h = resolve f g h
infer "resolution" [f, g] h = resolve f g h
infer "definition_folding" (f : fs) g = definFold fs f g
infer "definition_unfolding" (f : fs) g = dunfold fs f g
infer "pure_predicate_removal" [f] g = removePure f g
infer "flattening" [f] g = flattening f g
infer "equality_factoring" [f] g = eqfactor f g
infer "rectify" [f] g            = rectify f g
infer "avatar_component_clause" [f] g = avatarComp f g
infer "cnf_transformation" [f] g = cnfTrans f g
infer "avatar_split_clause" (f : fs) g    = avatarSplit fs f g
infer "unused_predicate_definition_removal" [f] g = updr 0 f g
infer "avatar_contradiction_clause" [f] g = efactor (Just True) f g
infer "skolemisation" (f : fs) g = skolemize fs 0 f g
infer r fs g = et $ "No inference : " <> r

getHyp :: Text -> NSeq -> IO Form
getHyp n c =
  case HM.lookup n c of
    Just f -> return f
    _ -> MF.fail $ "Hypothesis does not exist : " ++ show n

elab :: NSeq -> Step -> IO Stelab
elab s (n, "file", [m], g) = do
  f <- getHyp m s
  p <- orig f g
  return $ Plab g p n
elab _ (n, "predicate_definition_introduction", [], g) = relDef n g
elab _ (n, "avatar_definition", [], g) = relDef n g
elab s (n, "choice_axiom", [], g) = do
  (xs, f) <- normalizeAoC g
  p <- orig f g
  return $ AoC' xs f g p n
elab s (n, "avatar_sat_refutation", ns, g) = do
  fs <- mapM (`lookupM` s) ns
  p <- sat fs
  return $ Plab g p n
elab s (n, r, ns, g) = do
  fs <- mapM (`lookupM` s) ns
  p <- infer r fs g
  return $ Plab g p n
stelabIO :: Bool -> Hyps -> Step -> IO (Hyps, Stelab) -- todo : eliminate checking during Stelab-IO
stelabIO vb (nsq, sq) af@(n, _, _, f) = do
  when vb $ print $ "Elaborating step = " <> n
  e <- elab nsq af
  checkStelab sq f e <|> et ("precheck fail : " <> n)
  let e' = resj e
  checkStelab sq f e' <|> et "postcheck fail"
  return ((HM.insert n f nsq, S.insert f sq), e')

stepsToStelabs :: Bool -> Hyps -> [Step] -> IO [Stelab]
stepsToStelabs vb hs stps = do
  (_, es) <- mapAccumM (stelabIO vb) hs stps
  return es