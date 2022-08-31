{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Types
import Basic
import PP
import Parse
import Sat
import Lem
import Norm
import Prove

import Control.Monad as M (guard, MonadPlus, foldM, foldM_, (>=>), mzero, when )
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative ( Alternative((<|>)) )
import System.Environment
import Data.List as L
    (filter, null, find, map, length, foldl, elem, partition, all, any, concat, (\\),
    elemIndex, insert, sortBy, concatMap, unzip, nub, splitAt, delete, reverse )
import Data.Text as T ( uncons, unpack, pack, Text, take, length )
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList, delete,
  union, unions, difference, disjoint, (\\), null, lookupMin, isSubsetOf, intersection, size )
import qualified Data.Text.Lazy as TL (toStrict, intercalate)
import Data.Text.Read as TR ( decimal )
import Data.Functor ((<&>))
import Data.Map as HM ( Map, insert, lookup, empty, map, member, mapMaybe, mapKeys, toList,
  fromListWithKey, mapWithKey, delete, notMember, findWithDefault, partitionWithKey, isSubmapOf,
  filterWithKey, update, fromList, alter, foldrWithKey, foldr )
import Debug.Trace (trace)
import Data.Maybe as MB ( isNothing, fromMaybe, mapMaybe )

putAnForm :: AnForm -> IO ()
putAnForm i = Prelude.putStr $ ppAnForm i ++ "\n"

addHyp :: Hyps -> AnForm -> Hyps
addHyp (nsq, sq) (Af n f _) = (HM.insert n f nsq, S.insert f sq)

sortAfs :: [AnForm] -> [AnForm]
sortAfs = sortBy compareAfs

compareAfs :: AnForm -> AnForm -> Ordering
compareAfs (Af (m :> ms) _ _) (Af (n :> ns) _ _) =
  case compare m n of
    EQ -> compare (TR.decimal ms) (TR.decimal ns)
    other -> other
compareAfs _ _ = LT

addIf :: Bool -> NSeq-> Text -> Form -> [NSeq]
addIf True c n f = return $ HM.insert n f c
addIf False _ n _ = error $ "Cannot add formula : " ++ unpack n

getHyp :: Text -> NSeq -> IO Form
getHyp n c =
  case HM.lookup n c of
    Just f -> return f
    _ -> MF.fail $ "Hypothesis does not exist : " ++ show n

isRdef :: Text -> Form -> Bool
isRdef r (Fa _ (Iff (Rel s _) _)) = r == s
isRdef r (Iff (Rel s _) _) = r == s
isRdef _ _ = False

checkElab :: Seq -> Form -> Elab -> IO ()
checkElab sq g (Plab p) =
  verify 0 sq (S.singleton g) p
checkElab sq g (Rdef r f p) = do
  guard $ isRdef r f
  verify 0 (S.singleton f) (S.singleton g) p
checkElab sq g (AOC xs f p) = isAOC xs f
checkElab sq g (Lrats fs lrs) = do
  guard (L.all (`S.member` sq) fs)
  checkLrats (lratCtx 1 fs) lrs

lratCtx :: Int -> [Form] -> Map Int Form
lratCtx _ [] = HM.empty
lratCtx k (f : fs) = HM.insert k f $ lratCtx (k + 1) fs

negLit :: Form -> Form
negLit (Not f) = f
negLit f = Not f

negated :: Set Form -> Form -> Bool
negated fs (Not f) = f `elem` fs
negated fs f = Not f `elem` fs

checkLrat :: Map Int Form -> Set Form -> [Int] -> IO ()
checkLrat _ _ [] = mzero
checkLrat fs fxs [k] = do
  ls <- cast $ HM.lookup k fs >>= formToLits
  guard $ L.all (negated fxs) ls
checkLrat fs fxs (k : ks) = do
  ls <- cast $ HM.lookup k fs >>= formToLits
  fx <- cast $ breakSingleton $ nub $ L.filter (not . negated fxs) ls
  checkLrat fs (S.insert fx fxs) ks

checkLrats :: Map Int Form -> [Lrat] -> IO ()
checkLrats _ [] = mzero
checkLrats fs [Add _ [] hs] = checkLrat fs S.empty hs
checkLrats _ [_] = mzero
checkLrats fs (Add k ls hs : lrs) = do
  let ns = S.fromList $ L.map negLit ls
  checkLrat fs ns hs
  checkLrats (HM.insert k (Or ls) fs) lrs
checkLrats fs (Del _ ks : lrs) =
  checkLrats (L.foldl (flip HM.delete) fs ks) lrs

isSkolemTerm :: [Text] -> Term -> Bool
isSkolemTerm vs (Fun _ xs) =
  case mapM breakVar xs of
    Just ws -> sublist vs ws && sublist ws vs
    _ -> False
isSkolemTerm _ _ = False

isAOC :: [Term] -> Form -> IO ()
isAOC xs (Fa vs (Imp (Ex ws f) g)) = do
  guard $ L.all (isSkolemTerm vs) xs
  wxs <- zipM ws xs
  guard $ substForm wxs f == g
isAOC xs (Imp (Ex ws f) g) = do
  guard $ L.all isConstant xs
  wxs <- zipM ws xs
  guard $ substForm wxs f == g
isAOC _ _ = mzero

elabIO :: Hyps -> AnForm -> IO Hyps
elabIO (nsq, sq) (Af n f a) = do
  print $ "Elaborating step = " <> n
  e <- elab nsq (Af n f a)
  checkElab sq f e
  return (HM.insert n f nsq, S.insert f sq)

gFunFunctor :: Gterm -> Maybe Text
gFunFunctor (Gfun t []) = return t
gFunFunctor _ = Nothing

infer :: Text -> [Form] -> Form -> IO Prf
infer "superposition" [f, g] h         = superpose f g h
infer "forward_demodulation" [f, g] h  = superpose f g h
infer "backward_demodulation" [f, g] h = superpose f g h
infer "negated_conjecture" [f] g = guard (f == g) >> return (Ax f)
infer "factoring" [f] g = efactor (Just True) f g
infer "nnf_transformation" [f] g = nnfTrans False f g -- efactor (Just True) f g
infer "ennf_transformation" [f] g = nnfTrans True f g -- efactor (Just True) f g
infer "true_and_false_elimination" [f] g = trueFalseElim f g -- efactor (Just True) f g
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
infer "skolemisation" (f : fs) g = skolemize fs 0 f g  -- et "todo : skolemisation" -- skolemize fs (f, g, 0) blank
infer r fs g = et $ "No inference : " <> r


elab :: NSeq -> AnForm -> IO Elab
elab s (Af n h (Just (Gfun "file" [_, Gfun m []]))) = do
  f <- getHyp m s
  Plab <$> orig f h
elab _ (Af n g (Just (Gfun "introduced" [Gfun "predicate_definition_introduction" [],Glist [Gfun "new_symbols" [Gfun "naming" [],Glist [Gfun r []]]]]))) = relDef r g
elab _ (Af n g (Just (Gfun "introduced" [Gfun "avatar_definition" [], Glist [Gfun "new_symbols" [Gfun "naming" [], Glist [Gfun r []]]]]))) = relDef r g
elab s (Af n g (Just (Gfun "introduced" [Gfun "choice_axiom" [], Glist []]))) = do
  (xs, f) <- normalizeAOC g
  p <- orig f g
  return $ AOC xs f p
elab s (Af _ g (Just (Gfun "inference" [Gfun "avatar_sat_refutation" [], _, Glist l]))) = do
  fs <- cast (mapM gFunFunctor l) >>= mapM (`lookupM` s)
  -- sat fs <&> Lrats fs
  Lrats fs <$> sat fs 
elab s (Af _ g (Just (Gfun "inference" [Gfun r [], _, Glist l]))) = do
  fs <- cast (mapM gFunFunctor l) >>= mapM (`lookupM` s)
  p <- infer r fs g
  return $ Plab p
elab _ (Af _ _ a) = error $ "Unimplemented inference : " ++ show a

main :: IO ()
main = do
  (tptp : tstp : flags) <- getArgs
  tptp_afs <- parseName tptp
  tstp_afs <- sortAfs <$> parseName tstp
  let hs = L.foldl addHyp (HM.empty, S.empty) tptp_afs
  Prelude.putStr $ tptp ++ "\n"
  if "silent" `elem` flags
    then return ()
    else mapM_ putAnForm tptp_afs
  Prelude.putStr $ tstp ++ "\n"
  if "silent" `elem` flags
    then return ()
    else mapM_ putAnForm tstp_afs
  if tooHard tptp
  then return ()
  else foldM_ elabIO hs tstp_afs
  Prelude.putStr "Elab complete.\n\n"

tooHard :: String -> Bool
tooHard n = False -- n `elem` ["ALG038+1", "ALG016+1", "HAL004+1", "ALG114+1", "ALG111+1", "ALG121+1"]

  -- Prelude.putStr $ tstp ++ "\n"
  -- tstp_afs <- sortAfs <$> parseName tstp
  -- foldM_ elabIO' HM.empty tstp_afs
  -- Prelude.putStr "Elab complete.\n\n"



{- Verification -}

verifyEqGoal :: Int -> Seq -> Seq -> (Term, Term, Prf) -> IO ()
verifyEqGoal k lft rgt (x, y, p) = verify k lft (S.insert (Eq x y) rgt) p

verifyRgtGoal :: Int -> Seq -> Seq -> (Form, Prf) -> IO ()
verifyRgtGoal k lft rgt (f, p) = verify k lft (S.insert f rgt) p

verifyLftGoal :: Int -> Seq -> Seq -> (Form, Prf) -> IO ()
verifyLftGoal k lft rgt (f, p) = verify k (S.insert f lft) rgt p

ev :: Text -> Form -> Seq -> Seq -> IO ()
ev t f lhs rhs = et $ t <> ppForm f <> "\nLHS :\n" <> ppListNl ppForm (S.toList lhs) <> "\nRHS :\n" <> ppListNl ppForm (S.toList rhs) <> "\n"

verify :: Int -> Seq -> Seq -> Prf -> IO ()
verify _ _ _ Asm = return ()
verify _ lft rgt (Ax f) = do
  let lhs_text = ppListNl ppForm (S.toList lft)
  let rhs_text = ppListNl ppForm (S.toList rgt)
  guard (S.member f lft) <|> et ("Ax fail, LHS missing : " <> ppForm f <> "\nLHS = " <> lhs_text)
  guard (S.member f rgt) <|> et ("Ax fail, RHS missing : " <> ppForm f <> "\nRHS = " <> rhs_text)
verify _ lft rgt (EqR x) = guard (S.member (Eq x x) rgt) <|> error "EqR-fail"
verify k lft rgt (EqC (x0, y0, p0) (x1, y1, p1)) = do
  guard (S.member (Eq x0 x1) lft && S.member (Eq y0 y1) rgt) <|> error "EqC-fail"
  verifyEqGoal k lft rgt (x0, y0, p0) 
  verifyEqGoal k lft rgt (x1, y1, p1)
verify k lft rgt (EqS x y) = 
  guard (S.member (Eq x y) lft && S.member (Eq y x) rgt) <|> error "EqS-fail"
verify k lft rgt (EqT x y z) = 
  guard (S.member (Eq x y) lft && S.member (Eq y z) lft && S.member (Eq x z) rgt) <|> error "EqT-fail"
verify k lft rgt (FunC f egs) = do
  let xs = L.map (\ (x, _, _) -> x) egs 
  let ys = L.map (\ (_, y, _) -> y) egs 
  guard (S.member (Eq (Fun f xs) (Fun f ys)) rgt) <|> error "FunC-fail"
  mapM_ (verifyEqGoal k lft rgt) egs
verify k lft rgt (RelC r egs) = do
  let xs = L.map (\ (x, _, _) -> x) egs
  let ys = L.map (\ (_, y, _) -> y) egs
  guard (S.member (Rel r xs) lft && S.member (Rel r ys) rgt) <|> error "RelC-fail"
  mapM_ (verifyEqGoal k lft rgt) egs
verify k lft rgt (NotL f p) = do
  guard (S.member (Not f) lft) <|> error "NotL-fail"
  verify k lft (S.insert f rgt) p
verify k lft rgt (NotR f p) = do
  guard (S.member (Not f) rgt) <|> error "NotR-fail"
  verify k (S.insert f lft) rgt p
verify k lft rgt (OrL gls) = do
  let fs = L.map fst gls 
  guard (S.member (Or fs) lft) <|> error ("OrL-fail : " ++ unpack (ppForm (Or fs)))
  mapM_ (verifyLftGoal k lft rgt) gls
verify k lft rgt (OrR fs gs p) = do
  guard (sublist gs fs && S.member (Or fs) rgt) <|> error "OrR-fail"
  verify k lft (foldl (flip S.insert) rgt gs) p
verify k lft rgt (AndL fs gs p) = do
  guard (sublist gs fs) <|> error "AndL-fail : not subset"
  guard (S.member (And fs) lft) <|> ev "AndL-fail : " (And fs) lft rgt
  verify k (foldl (flip S.insert) lft gs) rgt p
verify k lft rgt (AndR gls) = do
  let fs = L.map fst gls 
  guard (S.member (And fs) rgt) <|> error "AndR-fail"
  mapM_ (verifyRgtGoal k lft rgt) gls
verify k lft rgt (ImpL f g p q) = do
  guard (S.member (Imp f g) lft) <|> ev "ImpL-fail" (f ==> g) lft rgt 
  verify k lft (S.insert f rgt) p
  verify k (S.insert g lft) rgt q
verify k lft rgt (ImpRA f g p) = do
  guard (S.member (Imp f g) rgt) <|> error "ImpRA-fail"
  verify k (S.insert f lft) rgt p
verify k lft rgt (ImpRC f g p) = do
  guard (S.member (Imp f g) rgt) <|> error "ImpRC-fail"
  verify k lft (S.insert g rgt) p
verify k lft rgt (IffR f g p q) = do
  guard (S.member (Iff f g) rgt) <|> ev "IffR-fail" (f <=> g) lft rgt 
  verify k lft (S.insert (Imp f g) rgt) p
  verify k lft (S.insert (Imp g f) rgt) q
verify k lft rgt (IffLO f g p) = do 
  guard (S.member (Iff f g) lft) <|> ev "IffLO-fail : " (f <=> g) lft rgt 
  verify k (S.insert (Imp f g) lft) rgt p
verify k lft rgt (IffLR f g p) = do 
  guard (S.member (f <=> g) lft) <|> ev "IffLR-fail : " (f <=> g) lft rgt 
  verify k (S.insert (Imp g f) lft) rgt p
verify k lft rgt (FaL vxs f p) = do
  let vs = L.map fst vxs 
  guard (S.member (Fa vs f) lft) <|> ev "FaL-fail : " (Fa vs f) lft rgt
  verify k (S.insert (substForm vxs f) lft) rgt p
verify k lft rgt (FaR vs m f p) = do
  let (k', xs) = listPars m vs 
  vxs <- zipM vs xs <|> error "FaR-fail : cannot zip"
  guard (k <= m && S.member (Fa vs f) rgt) <|> ev "FaR-fail" (Fa vs f) lft rgt
  verify k' lft (S.insert (substForm vxs f) rgt) p
verify k lft rgt (ExL vs m f p) = do
  let (k', xs) = listPars m vs 
  vxs <- zipM vs xs <|> error "ExL-fail"
  guard (k <= m && S.member (Ex vs f) lft) <|> error "ExL-fail"
  verify k' (S.insert (substForm vxs f) lft) rgt p
verify k lft rgt (ExR vxs f p) = do
  let vs = L.map fst vxs 
  guard (S.member (Ex vs f) rgt) <|> error "ExR-fail"
  verify k lft (S.insert (substForm vxs f) rgt) p
verify k lft rgt (Cut f p0 p1) = do
  verify k lft (S.insert f rgt) p0
  verify k (S.insert f lft) rgt p1
verify k lft rgt (Mrk s p) = verify k lft rgt p 

nnfTrans :: Bool -> Form -> Form -> IO Prf
nnfTrans b f g = do 
  p <- pnnf b 0 f g 
  return $ Cut (f <=> g) p $ iffMP f g

trueFalseElim :: Form -> Form -> IO Prf
trueFalseElim f g = do 
  let f' = boolSimp f
  let f'' = uws f'
  guard $ f'' == g
  p0 <- pbs 0 f f'
  p1 <- puw 0 f' f''
  return $ cutIff f f' p0 $ cutIff f' f'' p1 $ Ax g

pnnf :: Bool -> Int -> Form -> Form -> IO Prf
pnnf b k (Not (Not f)) g = do 
  p <- pnnf b k f g 
  return $ iffsTrans [(Not (Not f), notNotIff f), (f, p)] g
pnnf b k (Not (Or fs)) (And gs) = do
  let nfs = L.map Not fs 
  nfgps <- mapM2 (\ nf_ g_ -> (nf_ <=> g_,) <$> pnnf b k nf_ g_) nfs gs
  p <- cuts nfgps <$> cast (iffsToAndIffAnd nfs gs)
  return $ iffsTrans [(Not (Or fs), notOrIffAndNots fs), (And nfs, p)] (And gs)
pnnf b k (Not (And fs)) (Or gs) = do
  let nfs = L.map Not fs 
  nfgps <- mapM2 (\ nf_ g_ -> (nf_ <=> g_,) <$> pnnf b k nf_ g_) nfs gs
  p <- cuts nfgps <$> cast (iffsToOrIffOr nfs gs)
  return $ iffsTrans [(Not (And fs), notAndIffOrNots fs), (Or nfs, p)] (Or gs)
pnnf b k (Not (Imp f g)) h = do
  p <- pnnf b k (And [Not g, f]) h
  return $ iffsTrans [(Not (Imp f g), notImpIffNotAnd f g), (And [Not g, f], p)] h
pnnf True k (Not (Iff f g)) (Not fg) = do
  p <- pnnf True k (f <=> g) fg
  return $ Cut ((f <=> g) <=> fg) p $ iffToNotIffNot (f <=> g) fg
pnnf False k (Not (Iff f g)) (And [Or [ng', nf'], Or [g', f']]) = do 
  png <- pnnf False k (Not g) ng'
  pnf <- pnnf False k (Not f) nf'
  pf <- pnnf False k f f'
  pg <- pnnf False k g g'
  _px <- cast $ iffsToOrIffOr [Not g, Not f] [ng', nf']
  let px = Cut (Not g <=> ng') png $ Cut (Not f <=> nf') pnf _px -- px |- Or [Not g, Not f] <=> Or [ng', nf']
  _py <- cast $ iffsToOrIffOr [g, f] [g', f']
  let py = Cut (g <=> g') pg $ Cut (f <=> f') pf _py -- py |- Or [g, f] <=> Or [g', f']
  _pz <- cast $ iffsToAndIffAnd [Or [Not g, Not f], Or [g, f]] [Or [ng', nf'], Or [g', f']]
  let pz = Cut (Or [Not g, Not f] <=> Or [ng', nf']) px $ Cut (Or [g, f] <=> Or [g', f']) py _pz -- pz : |- (And [Or [Not g, Not f], Or [g, f]], pz) <=> (And [Or [ng', nf'], Or [g', f']]) 
  return $ 
    iffsTrans 
      [ (Not (Iff f g), notIffIffAnd f g), 
        (And [Or [Not g, Not f], Or [g, f]], pz) ] 
    (And [Or [ng', nf'], Or [g', f']]) 

pnnf b k (Not (Fa vs f)) (Ex ws nf) = do 
  guard $ vs == ws
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let nf' = substForm vxs nf
  pnf <- pnnf b k' (Not f') nf'
  let p = Cut (Fa vs $ Not f <=> nf) (FaR vs k (Not f <=> nf) pnf) $ faIffToExIffEx vs k (Not f) nf
  return $ iffsTrans [(Not (Fa vs f), notFaIffExNot k vs f), (Ex vs (Not f), p)] (Ex ws nf)

pnnf b k (Not (Ex vs f)) (Fa ws nf) = do 
  guard $ vs == ws
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let nf' = substForm vxs nf
  pnf <- pnnf b k' (Not f') nf'
  let p = Cut (Fa vs $ Not f <=> nf) (FaR vs k (Not f <=> nf) pnf) $ faIffToFaIffFa vs k (Not f) nf
  return $ iffsTrans [(Not (Ex vs f), notExIffFaNot k vs f), (Fa vs (Not f), p)] (Fa ws nf)

pnnf b k (Fa vs f) (Fa ws g) = do 
  guard $ vs == ws
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- pnnf b k' f' g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToFaIffFa vs k f g

pnnf b k (Ex vs f) (Ex ws g) = do 
  guard $ vs == ws
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- pnnf b k' f' g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToExIffEx vs k f g

pnnf b k (Imp f g) (Or [g', nf']) = do 
  pf <- pnnf b k (Not f) nf'
  pg <- pnnf b k g g'
  _p <- cast $ iffsToOrIffOr [g, Not f] [g', nf']
  let p = cuts [(Not f <=> nf', pf), (g <=> g', pg)] _p
  return $ iffsTrans [(f ==> g, impIffOrNot f g), (Or [g, Not f], p)] (Or [g', nf'])

pnnf b k (And fs) (And gs) = do
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> pnnf b k f_ g_) fs gs
  cuts fgps <$> cast (iffsToAndIffAnd fs gs)
pnnf b k (Or fs) (Or gs) = do
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> pnnf b k f_ g_) fs gs
  cuts fgps <$> cast (iffsToOrIffOr fs gs)
pnnf True k (Iff f g) (Iff f' g') = do
  pf <- pnnf True k f f'
  pg <- pnnf True k g g'
  return $ Cut (f <=> f') pf $ Cut (g <=> g') pg $ iffCong f g f' g' 
pnnf False k (Iff f g) (And [Or [f', ng'], Or [g', nf']]) = do 
  png <- pnnf False k (Not g) ng'
  pnf <- pnnf False k (Not f) nf'
  pf <- pnnf False k f f'
  pg <- pnnf False k g g'
  _px <- cast $ iffsToOrIffOr [f, Not g] [f', ng']
  let px = Cut (f <=> f') pf $ Cut (Not g <=> ng') png _px -- px |- Or [f, Not g] <=> Or [f', ng']
  _py <- cast $ iffsToOrIffOr [g, Not f] [g', nf']
  let py = Cut (g <=> g') pg $ Cut (Not f <=> nf') pnf _py -- py |- Or [g, Not f] <=> Or [g', nf']
  _pz <- cast $ iffsToAndIffAnd [Or [f, Not g], Or [g, Not f]] [Or [f', ng'], Or [g', nf']]
  let pz = Cut (Or [f, Not g] <=> Or [f', ng']) px $ Cut (Or [g, Not f] <=> Or [g', nf']) py _pz 
  return $ 
    iffsTrans 
      [ (Iff f g, iffIffAnd f g), 
        (And [Or [f, Not g], Or [g, Not f]], pz) ] 
    (And [Or [f', ng'], Or [g', nf']]) 
pnnf _ _ f@(Not (Rel _ _)) g
  | f == g = return $ iffRefl f
pnnf _ _ f@(Not (Eq _ _)) g
  | f == g = return $ iffRefl f
pnnf _ _ f@(Rel _ _) g
  | f == g = return $ iffRefl f
pnnf _ _ f@(Eq _ _) g
  | f == g = return $ iffRefl f
pnnf _ _ f g = et $ "prove-nnf\nf : " <> ppForm f <> "\ng : " <> ppForm g <> "\n"
