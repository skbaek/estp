{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Elab where

import Types
import Basic
import PP 
import Prove
import Sat (sat)
import Expand (stelabsToElabs)

import Data.List as L (all, map, foldl, length, reverse, findIndex, concatMap, mapAccumL, elemIndex)
import Data.Map as HM (insert, lookup, Map, empty, toList)
import Data.Set as S (Set, insert, singleton, toList, member)
import Data.Text.Lazy as T (Text, intercalate)
import Data.Text.Lazy.Builder (Builder)
import Control.Monad as M ( guard, MonadPlus(mzero), foldM_, when )
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative ( Alternative((<|>)) )
import Data.Bifunctor as DBF (first, second, bimap)
import Norm (fltn)
import GHC.Stack (pushCallStack)


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
rpsj Open' = Open'

removeMultiStepPrf :: Prf -> Prf
removeMultiStepPrf p@(Id' f) = p
removeMultiStepPrf p@(EqR' _) = p
removeMultiStepPrf p@(EqS' _ _) = p
removeMultiStepPrf p@EqT' {} = p
removeMultiStepPrf p@FunC' {} = p
removeMultiStepPrf p@RelC' {} = p
removeMultiStepPrf (NotT' f p) = NotT' f (removeMultiStepPrf p)
removeMultiStepPrf (NotF' f p) = NotF' f (removeMultiStepPrf p)

removeMultiStepPrf (OrT' ps) = OrT' $ L.map (DBF.second removeMultiStepPrf) ps
removeMultiStepPrf (AndF' ps) = AndF' $ L.map (DBF.second removeMultiStepPrf) ps

removeMultiStepPrf (AndT' _ [] p) = removeMultiStepPrf p
removeMultiStepPrf (OrF'  _ [] p) = removeMultiStepPrf p

removeMultiStepPrf (AndT' fs [g] p) = AndT' fs [g] $ removeMultiStepPrf p
removeMultiStepPrf (OrF'  fs [g] p) = OrF'  fs [g] $ removeMultiStepPrf p

removeMultiStepPrf (AndT' fs (g : gs) p) = AndT' fs [g] $ removeMultiStepPrf (AndT' fs gs p) 
removeMultiStepPrf (OrF' fs  (g : gs) p) = OrF'  fs [g] $ removeMultiStepPrf (OrF'  fs gs p) 

removeMultiStepPrf (ImpFA' f g p) = ImpFA' f g (removeMultiStepPrf p)
removeMultiStepPrf (ImpFC' f g p) = ImpFC' f g (removeMultiStepPrf p)
removeMultiStepPrf (ImpT' f g pf pg) = ImpT' f g (removeMultiStepPrf pf) (removeMultiStepPrf pg)
removeMultiStepPrf (IffTO' f g p) = IffTO' f g (removeMultiStepPrf p)
removeMultiStepPrf (IffTR' f g p) = IffTR' f g (removeMultiStepPrf p)
removeMultiStepPrf (IffF' f g pf pg) = IffF' f g (removeMultiStepPrf pf) (removeMultiStepPrf pg)
removeMultiStepPrf (FaT' vxs f p) = FaT' vxs f (removeMultiStepPrf p)
removeMultiStepPrf (ExF' vxs f p) = ExF' vxs f (removeMultiStepPrf p)
removeMultiStepPrf (FaF' xs k f p) = FaF' xs k f (removeMultiStepPrf p)
removeMultiStepPrf (ExT' xs k f p) = ExT' xs k f (removeMultiStepPrf p)
removeMultiStepPrf (Cut' f pl pr) = Cut' f (removeMultiStepPrf pl) (removeMultiStepPrf pr)
removeMultiStepPrf (Mrk t p) = Mrk t $ removeMultiStepPrf p
removeMultiStepPrf Open' = Open'

-- resj :: Stelab -> Stelab
-- resj (InfStep f p t) = InfStep f (rpsj p) t
-- resj (SkmStep xs f g p t) = SkmStep xs (rfsj f) (rfsj g) (rpsj p) t
-- resj (DefStep f g p t) = DefStep (rfsj f) (rfsj g) (rpsj p) t

-- detectSJ :: Elab -> IO ()
-- detectSJ ((ep, _, f), _, _)
--   | formSJ f = eb "single junct at EP : " 
--   | otherwise = return ()

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
ev t f lhs rhs = eb $ ft t <> ppForm f <> "\nLHS :\n" <> ppListNl ppForm (S.toList lhs) <> "\nRHS :\n" <> ppListNl ppForm (S.toList rhs) <> "\n"

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

checkStelab :: Set Form -> Stelab -> IO Form
checkStelab sq (InfStep g p nm) = do
  pb $ "Checking stelab : " <> ft nm <> "\n"
  verify 0 sq (S.singleton g) p
  return g
checkStelab sq (DefStep f g p nm) = do
  pb $ "Checking stelab : " <> ft nm <> "\n"
  guard $ isRelD f
  verify 0 (S.singleton f) (S.singleton g) p
  return g
checkStelab sq (SkmStep xs f g p nm) = do
  pb $ "Checking stelab : " <> ft nm <> "\n"
  isAoC' xs f
  verify 0 (S.singleton f) (S.singleton g) p
  return g

-- stelabConc :: Stelab -> Form
-- stelabConc (InfStep g p _) = g
-- stelabConc (DefStep f g p _) = g
-- stelabConc (SkmStep xs f g p _) = g

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


elab :: NTF -> Step -> IO Stelab
elab s (n, "file", [m], g) = do
  f <- cast (HM.lookup m s)
  p <- orig f g
  return $ InfStep g p n
elab _ (n, "predicate_definition_introduction", [], g) = relDef n g
elab _ (n, "avatar_definition", [], g) = relDef n g
elab s (n, "choice_axiom", [], g) = do
  (xs, f) <- normalizeAoC g
  p <- orig f g
  return $ SkmStep xs f g p n
elab s (n, "avatar_sat_refutation", ns, g) = do
  fs <- mapM (`lookupM` s) ns
  p <- sat fs
  return $ InfStep g p n
elab s (n, r, ns, g) = do
  fs <- mapM (`lookupM` s) ns
  p <- infer r fs g
  return $ InfStep g p n

stelabIO :: Bool -> (NTF, Set Form) -> Step -> IO ((NTF, Set Form), Stelab) -- todo : eliminate checking during Stelab-IO
stelabIO vb (nsq, sq) af@(n, _, _, f) = do
  when vb $ print $ "Elaborating step = " <> n
  e <- elab nsq af
  return ((HM.insert n f nsq, S.insert f sq), e)

stepsToStelabs :: Bool -> NTF -> Set Form -> [Step] -> IO [Stelab]
stepsToStelabs vb ntf sf stps = do
  (_, es) <- mapAccumM (stelabIO vb) (ntf, sf) stps
  return es

-- elaborate :: [String] -> IO ()
-- elaborate (tptp : tstp : estp : flags) = do
--   let vb = "silent" `notElem` flags
--   (hs, stps) <- hypsSteps vb tptp tstp
--   -- -- (_, es) <- mapAccumM (elabIO verbose) hs stps
--   slbs <- stepsToStelabs vb hs stps
--   elbs <- stelabsToElabs (fst hs) slbs
--   writeElab estp elbs


-- expand :: Invranch -> Form -> EP -> [Stelab] -> IO [Elab]
-- expand _ (Or []) ep [] = return [(ep, bt, Or [], 0, OrT (tlt $ ppEP ep), Just "'EOP'")]
-- expand _ f ep [] = et "last added formula must be bot\n"
-- expand br f ep (InfStep g p tx : els) = do
--   let br' = HM.insert (f, bt) (tlt $ppEP ep) br
--   efs0 <- expand' br' g (epFork 0 ep) els
--   efs1 <- addExp br' g bf (epFork 1 ep) 0 p
--   return $ (ep, bt, f, 0, Cut, Just tx) : efs0 ++ efs1
-- expand br f ep (RelD' g h p tx : els) = do
--   let br' = HM.insert (f, bt) (tlt $ ppEP ep) br
--   let ep' = epIncr ep
--   let br'' = HM.insert (g, bt) (tlt $ ppEP ep') br'
--   let ep'' = epIncr ep'
--   efs0 <- addExp br'' h bf (epFork 1 ep') 0 p
--   efs1 <- expand' br'' h ep'' els
--   return $ (ep, bt, f, 0, RelD, Just tx) : (ep', bt, g, 0, Cut, Just "'rel-def-cut'") : efs0 ++ efs1
-- expand br f ep (AoC' xs g h p tx : els) = do
--   let br' = HM.insert (f, bt) (tlt $ppEP ep) br
--   let ep' = epIncr ep
--   let br'' = HM.insert (g, bt) (tlt $ ppEP ep') br'
--   let ep'' = epIncr ep'
--   efs0 <- expand' br'' h ep'' els
--   efs1 <- addExp br'' h bf (epFork 1 ep') 0 p
--   return $ (ep, bt, f, 0, AoC xs, Just tx) : (ep', bt, g, 0, Cut, Just "'aoc-cut'") : efs0 ++ efs1
-- 

desingle :: Stelab -> Stelab
desingle (InfStep f p tx) = InfStep (rfsj f) (rpsj p) tx
desingle (DefStep f g p tx) = DefStep (rfsj f) (rfsj g) (rpsj p) tx
desingle (SkmStep xs f g p tx) = SkmStep xs (rfsj f) (rfsj g) (rpsj p) tx

removeMultiStep :: Stelab -> Stelab
removeMultiStep (InfStep f p tx) = InfStep f (removeMultiStepPrf p) tx
removeMultiStep (DefStep f g p tx) = DefStep f g (removeMultiStepPrf p) tx
removeMultiStep (SkmStep xs f g p tx) = SkmStep xs f g (removeMultiStepPrf p) tx

indexStelabs :: Int -> HM.Map Funct Int -> [Stelab] -> IO [Stelab]
indexStelabs k mp [] = return []
indexStelabs k mp (InfStep f p tx : slbs) = do
  slbs' <- indexStelabs k mp slbs
  return $ InfStep (indexForm mp f) (indexPrf k mp p) tx : slbs'
indexStelabs k mp (DefStep f g p tx : slbs) = do
  let r = definedRel f 
  let mp' = HM.insert r k mp 
  let f' = indexForm mp' f 
  let g' = indexForm mp' g 
  let p' = indexPrf (k + 1) mp' p 
  pt "New map :\n"
  pb $ ppHM ppFunct ppInt mp'
  pt "\n\n\n"
  slbs' <- indexStelabs (k + 1) mp' slbs 
  return $ DefStep f' g' p' tx : slbs'
indexStelabs k mp (SkmStep xs f g p tx : slbs) = do
  let ((k', mp'), xs') = L.mapAccumL indexAoCStep (k, mp) xs 
  let f' = indexForm mp' f
  let g' = indexForm mp' g
  let p' = indexPrf k' mp' p
  slbs' <- indexStelabs k' mp' slbs 
  return $ SkmStep xs' f' g' p' tx : slbs'

indexAoCStep :: (Int, HM.Map Funct Int) -> Term -> ((Int, HM.Map Funct Int), Term)
indexAoCStep (k, mp) (Fun (Reg tx) xs) = ((k + 1, HM.insert (Reg tx) k mp), Fun (Idx k) xs)
indexAoCStep (k, mp) x =  error "cannot get functor text"

-- relabel k (AoC' xs f p) = 
--   let len = length xs in
--   let k' = k + len in
--   let p' = relabel k' p in
--   let fks = relabelPairsAoC xs k in 
--   AoC' (L.map (indexTerm fks) xs) (indexForm fks f) $ relabel' fks p'
-- relabel k (RelD' f p) = 
--   let r = definedRel f in
--   let p' = relabel (k + 1) p in
--   RelD' (indexForm [(r, k)] f) $ relabel' [(r, k)] p'

indexPrf :: Int -> HM.Map Funct Int -> Prf -> Prf
indexPrf k mp Open' = Open'
indexPrf k mp (Id' f) = Id' (indexForm mp f)
indexPrf k mp (EqR' x) = EqR' (indexTerm mp x)
indexPrf k mp (EqS' x y) = EqS' (indexTerm mp x) (indexTerm mp y)
indexPrf k mp (EqT' x y z) = EqT' (indexTerm mp x) (indexTerm mp y) (indexTerm mp z)
indexPrf k mp (FunC' f xs ys) = FunC' (indexFunctor mp f) (L.map (indexTerm mp) xs) (L.map (indexTerm mp) ys)
indexPrf k mp (RelC' r xs ys) = RelC' (indexFunctor mp r) (L.map (indexTerm mp) xs) (L.map (indexTerm mp) ys)
indexPrf k mp (NotT' f p) = NotT' (indexForm mp f) $ indexPrf k mp p
indexPrf k mp (NotF' f p) = NotF' (indexForm mp f) $ indexPrf k mp p

indexPrf k mp (AndT' fs gs p) = AndT' (L.map (indexForm mp) fs) (L.map (indexForm mp) gs) $ indexPrf k mp p
indexPrf k mp (OrF' fs gs p)  = OrF'  (L.map (indexForm mp) fs) (L.map (indexForm mp) gs) $ indexPrf k mp p

indexPrf k mp (OrT' fps) =  OrT'  $ L.map (bimap (indexForm mp) (indexPrf k mp)) fps
indexPrf k mp (AndF' fps) = AndF' $ L.map (bimap (indexForm mp) (indexPrf k mp)) fps
indexPrf k mp (ImpT' f g p q) = ImpT' (indexForm mp f) (indexForm mp g) (indexPrf k mp p) (indexPrf k mp q)
indexPrf k mp (ImpFA' f g p) = ImpFA' (indexForm mp f) (indexForm mp g) $ indexPrf k mp p
indexPrf k mp (ImpFC' f g p) = ImpFC' (indexForm mp f) (indexForm mp g) $ indexPrf k mp p
indexPrf k mp (IffF' f g p q) = IffF' (indexForm mp f) (indexForm mp g) (indexPrf k mp p) (indexPrf k mp q)
indexPrf k mp (IffTO' f g p) = IffTO' (indexForm mp f) (indexForm mp g) $ indexPrf k mp p
indexPrf k mp (IffTR' f g p) = IffTR' (indexForm mp f) (indexForm mp g) $ indexPrf k mp p

indexPrf k mp (FaT' vxs f p) = FaT' (L.map (DBF.second $ indexTerm mp) vxs) (indexForm mp f) $ indexPrf k mp p
indexPrf k mp (ExF' vxs f p) = ExF' (L.map (DBF.second $ indexTerm mp) vxs) (indexForm mp f) $ indexPrf k mp p

indexPrf k mp (FaF' vs m f p) =
  let (_, k', mp') = L.foldl (\ (m_, k_, mp_) _ -> (m_ + 1, k_ + 1, HM.insert (Idx m_) k_ mp_)) (m, k, mp) vs in
  FaF' vs k (indexForm mp f) $ indexPrf k' mp' p
indexPrf k mp (ExT' vs m f p) =
  let (_, k', mp') = L.foldl (\ (m_, k_, mp_) _ -> (m_ + 1, k_ + 1, HM.insert (Idx m_) k_ mp_)) (m, k, mp) vs in
  ExT' vs k (indexForm mp f) $ indexPrf k' mp' p

indexPrf k mp (Cut' f p q) = Cut' (indexForm mp f) (indexPrf k mp p) (indexPrf k mp q)
indexPrf k mp (Mrk s p) = Mrk s $ indexPrf k mp p

relabelPairsAoC :: [Term] -> Int -> [(Funct, Int)]
relabelPairsAoC [] _ = []
relabelPairsAoC (Fun f _ : xs) k = (f, k) : relabelPairsAoC xs (k + 1)
relabelPairsAoC _ _ = et "relabel-pairs-aoc"


definedRel :: Form -> Funct
definedRel (Fa _ f) = definedRel f
definedRel (Iff (Rel (Reg tx) _) _) = Reg tx
definedRel _ = et "Not a relation definition"


relabelPairs :: Int -> Int -> Int -> [(Funct, Int)]
relabelPairs 0 _ _ = []
relabelPairs l k m = (Idx k, m) : relabelPairs (l - 1) (k + 1) (m + 1)

-- relabel' :: [(Funct, Int)] -> Prf -> Prf
-- relabel' kms Open' = Open'
-- relabel' kms (Id' f) = Id' $ indexForm kms f
-- relabel' kms (EqR' x) = EqR' x
-- relabel' kms (EqS' x y) = EqS' x y
-- relabel' kms (EqT' x y z) = EqT' x y z
-- relabel' kms (FunC' f xs ys) = FunC' (indexFunctor kms f) (L.map (indexTerm kms) xs) (L.map (indexTerm kms) ys)
-- relabel' kms (RelC' r xs ys) = RelC' (indexFunctor kms r) (L.map (indexTerm kms) xs) (L.map (indexTerm kms) ys)
-- relabel' kms (NotT' f p) = NotT' (indexForm kms f) $ relabel' kms p
-- relabel' kms (NotF' f p) = NotT' (indexForm kms f) $ relabel' kms p
-- relabel' kms (OrT' gps) = OrT' $ L.map (bimap (indexForm kms) (relabel' kms)) gps
-- relabel' kms (OrF' fs gs p) = OrF'   (L.map (indexForm kms) fs) (L.map (indexForm kms) gs) $ relabel' kms p
-- relabel' kms (AndT' fs gs p) = AndT' (L.map (indexForm kms) fs) (L.map (indexForm kms) gs) $ relabel' kms p
-- relabel' kms (AndF' fps) = AndF' $ L.map (bimap (indexForm kms) (relabel' kms)) fps
-- relabel' kms (ImpT' f g p q) = ImpT' (indexForm kms f) (indexForm kms g) (relabel' kms p) (relabel' kms q)
-- relabel' kms (ImpFA' f g p) = ImpFA' (indexForm kms f) (indexForm kms g) $ relabel' kms p
-- relabel' kms (ImpFC' f g p) = ImpFC' (indexForm kms f) (indexForm kms g) $ relabel' kms p
-- relabel' kms (IffF' f g p q) = IffF' (indexForm kms f) (indexForm kms g) (relabel' kms p) (relabel' kms q)
-- relabel' kms (IffTO' f g p) = IffTO' (indexForm kms f) (indexForm kms g) $ relabel' kms p
-- relabel' kms (IffTR' f g p) = IffTR' (indexForm kms f) (indexForm kms g) $ relabel' kms p
-- relabel' kms (FaT' vxs f p) = FaT' (L.map (DBF.second $ indexTerm kms) vxs) (indexForm kms f) $ relabel' kms p
-- relabel' kms (FaF' vs m f p) = FaF' vs m (indexForm kms f) (relabel' kms p)
-- relabel' kms (ExT' vs m f p) = ExT' vs m (indexForm kms f) (relabel' kms p)
-- relabel' kms (ExF' vxs f p) = ExF' vxs (indexForm kms f) $ relabel' kms p
-- -- relabel' kms (AoC' xs f p) = AoC' (L.map (indexTerm kms) xs) (indexForm kms f) (relabel' kms p)
-- -- relabel' kms (RelD' f p) = RelD' (indexForm kms f) (relabel' kms p)
-- relabel' kms (Cut' f p0 p1) = Cut' (indexForm kms f) (relabel' kms p0) (relabel' kms p1)
-- relabel' kms (Mrk s p) = Mrk s $ relabel' kms p

indexForm :: HM.Map Funct Int -> Form -> Form
indexForm kms (Eq x y) = Eq (indexTerm kms x) (indexTerm kms y)
indexForm kms (Rel r xs) = Rel (indexFunctor kms r) $ L.map (indexTerm kms) xs
indexForm kms (Not f) = Not $ indexForm kms f
indexForm kms (Or fs) = Or $ L.map (indexForm kms) fs
indexForm kms (And fs) = And $ L.map (indexForm kms) fs
indexForm kms (Imp f g) = Imp (indexForm kms f) (indexForm kms g)
indexForm kms (Iff f g) = Iff (indexForm kms f) (indexForm kms g)
indexForm kms (Fa vs f) = Fa vs $ indexForm kms f
indexForm kms (Ex vs f) = Ex vs $ indexForm kms f

indexTerm :: HM.Map Funct Int -> Term -> Term
indexTerm _ v@(Var _) = v
indexTerm kms (Fun f xs) = Fun (indexFunctor kms f) $ L.map (indexTerm kms) xs

indexFunctor :: HM.Map Funct Int -> Funct -> Funct
indexFunctor mp f = 
  case HM.lookup f mp of 
    Just k -> Idx k 
    _ -> f

type Loc = (Text, [Int], Int)

locText :: Loc -> Text
locText (tx, ks, k) = tx <> "_" <> T.intercalate "_" (L.map (tlt . ppInt) $ L.reverse ks) <> "_" <> tlt (ppInt k)

extLoc :: Loc -> Loc
extLoc (tx, ks, k) = (tx, ks, k + 1)

forkLoc :: Loc -> Int -> Loc
forkLoc (tx, ks, k) m = (tx, m : ks, 0)

range :: Int -> Int -> [Int]
range _ 0 = []
range k m = k : range (k + 1) (m - 1)

stitch :: SFTN -> NodeInfo -> [Stelab] -> IO Proof
stitch sftn ni@(nm, True, Or []) [] = return (OrT_ ni nm [])
stitch _ _ [] = error "Last signed formula of branch is not a T-bot"
stitch sftn (nm, b, f) (InfStep g prf cmt : slbs) = do 
  pt $ "Stitching inf step : " <> cmt <> "\n"
  pt "Proof:\n"
  pb $ ppPrf 1000 prf

  let loc = (cmt, [], 0)
  proofL <- deliteral' sftn loc (False, g) prf
  pt "Left stitched!\n"
  proofR <- stitch (HM.insert (True, g) cmt sftn) (cmt, True, g) slbs
  pt "Right stitched!\n"
  return $ Cut_ (nm, b, f) proofL proofR
stitch sftn (nm, b, h) (DefStep f g p cmt : slbs) = do 
  pt $ "Stitching def step : " <> cmt <> "\n"
  let loc = (cmt, [], 0)
  let loc' = extLoc loc
  let sftn' = HM.insert (True, f) (locText loc) sftn
  pf <- deliteral' sftn' loc' (False, g) p

  pt <- stitch (HM.insert (True, g) cmt sftn') (cmt, True, g) slbs

  return $ RelD_ (nm, b, h) $ Cut_ (locText loc, True, f) pf pt
stitch sftn (nm, b, h) (SkmStep xs f g p cmt : slbs) = do 
  pt $ "Stitching AoC step : " <> cmt <> "\n"
  let loc = (cmt, [], 0)
  let loc' = extLoc loc
  let sftn' = HM.insert (True, f) (locText loc) sftn

  pf <- deliteral' sftn' loc' (False, g) p
  pt <- stitch (HM.insert (True, g) cmt sftn') (cmt, True, g) slbs

  return $ AoC_ (nm, b, h) xs $ Cut_ (locText loc, True, f) pf pt

-- stitch :: [Stelab] -> IO Prf
-- stitch [] = return $ OrT' []
-- stitch (InfStep f prf cmt : slbs) = do
--   prf' <- stitch slbs
--   return $ Cut' f prf (Mrk cmt prf')
-- stitch (DefStep f g prf cmt : slbs) = do
--   prf' <- stitch slbs
--   return $ RelD' f $ Mrk (cmt <> " : pre-def") $
--     Cut' g prf $ Mrk (cmt <> " : post-def") prf'
-- stitch (SkmStep xs f g prf cmt : slbs) = do
--   prf' <- stitch slbs
--   return $ AoC' xs f $ Mrk (cmt <> " : pre-skm") $
--     Cut' g prf $ Mrk (cmt <> " : post-stm") prf'
deliteral' :: SFTN -> Loc -> (Bool, Form) -> Prf -> IO Proof
deliteral' sftn loc (b, f) prf = do
  let sftn' = HM.insert (b, f) (locText loc) sftn 
  -- pt "\n-----------------------------------------------------------\n"
  -- pt "Branch after insert :\n"
  -- pb $ ppInter "\n" $ L.map (\ (sf_, nm_) ->  ppSignForm sf_ <> " ----- " <> ft nm_ <> "\n") $ HM.toList sftn'
  pt "\n-----------------------------------------------------------\n"
  pb "Proof :\n"
  pb $ ppPrf 5 prf
  pt "\n-----------------------------------------------------------\n\n\n\n"
  deliteral sftn' loc (b, f) prf

deliteral :: SFTN -> Loc -> (Bool, Form) -> Prf -> IO Proof
deliteral sftn loc (b, h) Open' = return $ Open_ (locText loc, b, h) 

deliteral sftn loc (b, h) (Id' f) = do
  nt <- cast $ HM.lookup (True, f)  sftn 
  nf <- cast $ HM.lookup (False, f) sftn 
  return $ Id_ (locText loc, b, h) nt nf

deliteral sftn loc (b, h) (EqR' x) = do
  nm <- cast $ HM.lookup (False, Eq x x) sftn 
  return $ EqR_ (locText loc, b, h) nm
deliteral sftn loc (b, h) (EqS' x y) = do
  nt <- cast $ HM.lookup (True, Eq x y) sftn 
  nf <- cast $ HM.lookup (False, Eq y x) sftn 
  return $ EqS_ (locText loc, b, h) nt nf
deliteral sftn loc (b, h) (EqT' x y z) = do
  nxy <- cast $ HM.lookup (True, Eq x y) sftn 
  nyz <- cast $ HM.lookup (True, Eq y z) sftn 
  nxz <- cast $ HM.lookup (False, Eq x z) sftn 
  return $ EqT_ (locText loc, b, h) nxy nyz nxz
deliteral sftn loc (b, h) (FunC' f xs ys) = do
  xys <- zipM xs ys
  let eqns = L.map ((True,) . uncurry Eq) xys
  nms <- cast $ mapM (`HM.lookup` sftn) eqns
  nm <- cast $ HM.lookup (False, Fun f xs === Fun f ys) sftn 
  return $ FunC_ (locText loc, b, h) nms nm
deliteral sftn loc (b, h) (RelC' r xs ys) = do
  xys <- zipM xs ys
  let eqns = L.map ((True,) . uncurry Eq) xys
  nms <- cast $ mapM (`HM.lookup` sftn) eqns
  nt <- cast $ HM.lookup (True, Rel r xs) sftn 
  nf <- cast $ HM.lookup (False, Rel r ys) sftn 
  return $ RelC_ (locText loc, b, h) nms nt nf

deliteral sftn loc (b, h) (NotT' f p) = do
  let loc' = extLoc loc
  nm <- cast $ HM.lookup (True, Not f) sftn 
  p' <- deliteral' sftn loc' (False, f) p 
  return $ NotT_ (locText loc, b, h) nm p'

deliteral sftn loc (b, h) (NotF' f p) = do
  let loc' = extLoc loc
  nm <- cast $ HM.lookup (False, Not f) sftn 
  p' <- deliteral' sftn loc' (True, f) p 
  return $ NotF_ (locText loc, b, h) nm p'
  
deliteral sftn loc (b, h) (OrT' fps) = do
  let (fs, _) = unzip fps
  nm <- cast $ HM.lookup  (True, Or fs) sftn 
  let ks = range 0 $ L.length fps
  ps' <- mapM2 (\ k_ (f_, p_) -> deliteral' sftn (forkLoc loc k_) (True, f_) p_) ks fps
  return $ OrT_ (locText loc, b, h) nm ps'

deliteral sftn loc (b, h) (OrF' fs [f] p) = do
  let loc' = extLoc loc
  nm <- cast $ HM.lookup (False, Or fs) sftn 
  p' <- deliteral' sftn loc' (False, f) p 
  k <- cast $ elemIndex f fs
  return $ OrF_ (locText loc, b, h) nm k p'
deliteral sftn loc (b, h) (OrF' fs gs p) = eb $ "single residue : " <> ppForm (Or gs)

deliteral sftn loc (b, h) (AndT' fs [f] p) = do
  let loc' = extLoc loc
  nm <- cast $ HM.lookup (True, And fs) sftn 
  p' <- deliteral' sftn loc' (True, f) p 
  k <- cast $ elemIndex f fs
  return $ AndT_ (locText loc, b, h) nm k p'
deliteral sftn loc (b, h) (AndT' fs gs p) = eb $ "single residue : " <> ppForm (And gs)

deliteral sftn loc (b, h) (AndF' fps) = do
  let (fs, _) = unzip fps
  let ks = range 0 $ L.length fps
  nm <- cast $ HM.lookup  (False, And fs) sftn 
  ps' <- mapM2 (\ k_ (f_, p_) -> deliteral' sftn (forkLoc loc k_) (False, f_) p_) ks fps
  return $ AndF_ (locText loc, b, h) nm ps'

deliteral sftn loc (b, h) (ImpT' f g p q) = do
  nm <- cast $ HM.lookup (True, Imp f g) sftn 
  p' <- deliteral' sftn (forkLoc loc 0) (False, f) p 
  q' <- deliteral' sftn (forkLoc loc 1) (True, g) q 
  return $ ImpT_ (locText loc, b, h) nm p' q'

deliteral sftn loc (b, h) (ImpFA' f g p) = do
  nm <- cast $ HM.lookup  (False, Imp f g) sftn 
  p' <- deliteral' sftn (extLoc loc) (True, f) p 
  return $ ImpFA_ (locText loc, b, h) nm p' 

deliteral sftn loc (b, h) (ImpFC' f g p) = do
  nm <- cast $ HM.lookup  (False, Imp f g) sftn 
  p' <- deliteral' sftn (extLoc loc) (False, g) p 
  return $ ImpFC_ (locText loc, b, h) nm p' 

deliteral sftn loc (b, h) (IffF' f g p q) = do
  nm <- cast $ HM.lookup (False, Iff f g) sftn 
  p' <- deliteral' sftn (forkLoc loc 0) (False, Imp f g) p 
  q' <- deliteral' sftn (forkLoc loc 1) (False, Imp g f) q 
  return $ IffF_ (locText loc, b, h) nm p' q'

deliteral sftn loc (b, h) (IffTO' f g p) = do
  nm <- cast $ HM.lookup (True, Iff f g) sftn 
  p' <- deliteral' sftn (extLoc loc) (True, Imp f g) p 
  return $ IffTO_ (locText loc, b, h) nm p' 

deliteral sftn loc (b, h) (IffTR' f g p) = do
  nm <- cast $ HM.lookup (True, Iff f g) sftn 
  p' <- deliteral' sftn (extLoc loc) (True, Imp g f) p 
  return $ IffTR_ (locText loc, b, h) nm p' 

deliteral sftn loc (b, h) (FaT' vxs f p) = do
  let (vs, xs) = unzip vxs 
  nm <- cast $ HM.lookup (True, Fa vs f) sftn 
  let f' = substForm vxs f
  p' <- deliteral' sftn (extLoc loc) (True, f') p 
  return $ FaT_ (locText loc, b, h) nm xs p'

deliteral sftn loc (b, h) (FaF' vs m f p) = do
  nm <- cast $ HM.lookup (False, Fa vs f) sftn -- <> eb ("Cannot find hyp:\n" <> ppSignForm (False, Fa vs f)) 
  let (_, vxs) = varPars m vs 
  let f' = substForm vxs f
  p' <- deliteral' sftn (extLoc loc) (False, f') p 
  return $ FaF_ (locText loc, b, h) nm m p'

deliteral sftn loc (b, h) (ExT' vs m f p) = do
  nm <- cast $ HM.lookup (True, Ex vs f) sftn 
  let (_, vxs) = varPars m vs 
  let f' = substForm vxs f
  p' <- deliteral' sftn (extLoc loc) (True, f') p 
  return $ ExT_ (locText loc, b, h) nm m p'

deliteral sftn loc (b, h) (ExF' vxs f p) = do
  let (vs, xs) = unzip vxs 
  nm <- cast $ HM.lookup (False, Ex vs f) sftn 
  let f' = substForm vxs f
  p' <- deliteral' sftn (extLoc loc) (False, f') p 
  return $ ExF_ (locText loc, b, h) nm xs p'

deliteral sftn loc (b, h) (Cut' f p q) = do
  p' <- deliteral' sftn (forkLoc loc 0) (False, f) p 
  q' <- deliteral' sftn (forkLoc loc 1) (True, f) q 
  return $ Cut_ (locText loc, b, h) p' q'
  
deliteral sftn loc (b, h) (Mrk s p) = deliteral sftn loc (b, h) p

linearize :: Proof -> [Elab]
linearize (Id_ ni nt nf) = [(ni, Id nt nf, Nothing)]
linearize (Cut_ ni p q) = (ni, Cut (proofRN p) (proofRN q), Nothing) : linearize p ++ linearize q
linearize (FunC_ ni xs nm) = [(ni, FunC xs nm, Nothing)]
linearize (RelC_ ni xs nt nf) = [(ni, RelC xs nt nf, Nothing)]
linearize (EqR_ ni nm) = [(ni, EqR nm, Nothing)]
linearize (EqS_ ni nt nf) = [(ni, EqS nt nf, Nothing)]
linearize (EqT_ ni nxy nyz nxz) = [(ni, EqT nxy nyz nxz, Nothing)]
linearize (NotT_ ni nm p) = (ni, NotT nm (proofRN p), Nothing) : linearize p
linearize (NotF_ ni nm p) = (ni, NotF nm (proofRN p), Nothing) : linearize p
linearize (OrT_ ni nm ps) = (ni, OrT nm (L.map proofRN ps), Nothing) : L.concatMap linearize ps
linearize (OrF_ ni nm k p) = (ni, OrF nm k (proofRN p), Nothing) : linearize p
linearize (AndT_ ni nm k p) = (ni, AndT nm k (proofRN p), Nothing) : linearize p
linearize (AndF_ ni nm ps) = (ni, AndF nm (L.map proofRN ps), Nothing) : L.concatMap linearize ps
linearize (ImpT_ ni nm p q) = (ni, ImpT nm (proofRN p) (proofRN q), Nothing) : linearize p ++ linearize q
linearize (ImpFA_ ni nm p) = (ni, ImpFA nm (proofRN p), Nothing) : linearize p
linearize (ImpFC_ ni nm p) = (ni, ImpFC nm (proofRN p), Nothing) : linearize p
linearize (IffTO_ ni nm p) = (ni, IffTO nm (proofRN p), Nothing) : linearize p
linearize (IffTR_ ni nm p) = (ni, IffTR nm (proofRN p), Nothing) : linearize p
linearize (IffF_ ni nm p q) = (ni, IffF nm (proofRN p) (proofRN q), Nothing) : linearize p ++ linearize q
linearize (FaT_ ni nm xs p) = (ni, FaT nm xs (proofRN p), Nothing) : linearize p
linearize (FaF_ ni nm k p) = (ni, FaF nm k (proofRN p), Nothing) : linearize p
linearize (ExT_ ni nm k p) = (ni, ExT nm k (proofRN p), Nothing) : linearize p
linearize (ExF_ ni nm xs p) = (ni, ExF nm xs (proofRN p), Nothing) : linearize p
linearize (RelD_ ni p) = (ni, RelD (proofRN p), Nothing) : linearize p
linearize (AoC_ ni xs p) = (ni, AoC xs (proofRN p), Nothing) : linearize p
linearize (Open_ ni) = [(ni, Open, Nothing)]

elaborate :: Bool -> NTF -> Set Form -> SFTN -> [Step] -> IO [Elab]
elaborate vb ntf sf ftn stps = do
   slbs <- stepsToStelabs vb ntf sf stps
   checkStelabs sf slbs
   let slbs' = L.map (removeMultiStep . desingle) slbs
   checkStelabs sf slbs'
   slbs'' <- indexStelabs 0 HM.empty slbs'
   checkStelabs sf slbs''
   proof <- stitch ftn ("root", True, top) slbs''
   return $ linearize proof

checkStelabs :: Set Form -> [Stelab] -> IO ()
checkStelabs sf [] = return ()
checkStelabs sf (slb : slbs) = do 
  g <- checkStelab sf slb 
  let sf' = S.insert g sf 
  checkStelabs sf' slbs

-- checkStelab sq f e <|> et ("precheck fail : " <> n)
   -- stelabsToElabs ntf slbs