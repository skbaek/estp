{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use list comprehension" #-}

module Main where

import Types
import Basic
import PP
import Parse ( parseName )
import Sat ( sat )
import Lem
import Norm
import Prove

import Control.Monad as M ( guard, MonadPlus(mzero), foldM_, when ) 
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative ( Alternative((<|>)) )
import System.Environment ( getArgs )
import Data.List as L ( map, foldl, all, sortBy ) 
import Data.Text as T ( Text, unpack )
import Data.Set as S ( empty, insert, member, singleton, toList )  
import Data.Text.Read as TR ( decimal )
import Data.Map as HM ( empty, insert, lookup )  

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
  Plab <$> sat fs
elab s (Af _ g (Just (Gfun "inference" [Gfun r [], _, Glist l]))) = do
  fs <- cast (mapM gFunFunctor l) >>= mapM (`lookupM` s)
  p <- infer r fs g
  return $ Plab p
elab _ (Af _ _ a) = error $ "Unimplemented inference : " ++ show a



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
  verify k lft (L.foldl (flip S.insert) rgt gs) p
verify k lft rgt (AndL fs gs p) = do
  guard (sublist gs fs) <|> error "AndL-fail : not subset"
  guard (S.member (And fs) lft) <|> ev "AndL-fail : " (And fs) lft rgt
  verify k (L.foldl (flip S.insert) lft gs) rgt p
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



{- Main -}

skipList :: String -> Bool
skipList n = False 

elaborate :: [String] -> IO ()
elaborate (tptp : tstp : flags) = do
  tptp_afs <- parseName tptp
  tstp_afs <- sortAfs <$> parseName tstp
  let hs = L.foldl addHyp (HM.empty, S.empty) tptp_afs
  Prelude.putStr $ tptp ++ "\n"
  when ("silent" `elem` flags) $ mapM_ putAnForm tptp_afs
  Prelude.putStr $ tstp ++ "\n"
  when ("silent" `elem` flags) $ mapM_ putAnForm tstp_afs
  foldM_ elabIO hs tstp_afs
  Prelude.putStr "Elab complete.\n\n"

check :: [String] -> IO ()
check (tptp : estp : flags) = do _

main :: IO ()
main = do
  (cmd : args) <- getArgs
  case cmd of  
    "elab" -> elaborate args
    "check" -> check args
    _ -> et "undefined command"