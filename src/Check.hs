module Check where

import Types
import Basic
import Data.List (map, foldl)
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList )
import Control.Monad as M (guard, foldM, foldM_ ,(>=>))

sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _ = True
sublist (x : xs) ys = elem x ys && sublist xs ys

verifyEqGoal :: Int -> Seq -> Seq -> (Term, Term, Prf) -> IO ()
verifyEqGoal k lft rgt (x, y, p) = verify k lft (S.insert (Eq x y) rgt) p

verifyRgtGoal :: Int -> Seq -> Seq -> (Form, Prf) -> IO ()
verifyRgtGoal k lft rgt (f, p) = verify k lft (S.insert f rgt) p

verifyLftGoal :: Int -> Seq -> Seq -> (Form, Prf) -> IO ()
verifyLftGoal k lft rgt (f, p) = verify k (S.insert f lft) rgt p

verify :: Int -> Seq -> Seq -> Prf -> IO ()
verify _ lft rgt (Ax f) = do
  guard $ S.member f lft 
  guard $ S.member f rgt
verify _ lft rgt (EqR x) = guard $ S.member (Eq x x) rgt
verify k lft rgt (EqC (x0, y0, p0) (x1, y1, p1)) = do
  guard $ S.member (Eq x0 x1) lft 
  guard $ S.member (Eq y0 y1) rgt 
  verifyEqGoal k lft rgt (x0, y0, p0) 
  verifyEqGoal k lft rgt (x1, y1, p1)
verify k lft rgt (EqSL x y p) = do
  guard $ S.member (Eq x y) lft
  verify k (S.insert (Eq y x) lft) rgt p
verify k lft rgt (FunC f egs) =
  let xs = map (\ (x, _, _) -> x) egs in
  let ys = map (\ (_, y, _) -> y) egs in
  do guard $ S.member (Eq (Fun f xs) (Fun f ys)) rgt
     mapM_ (verifyEqGoal k lft rgt) egs
verify k lft rgt (RelC r egs) =
  let xs = map (\ (x, _, _) -> x) egs in
  let ys = map (\ (_, y, _) -> y) egs in
  do guard $ S.member (Rel r xs) lft
     guard $ S.member (Rel r ys) rgt
     mapM_ (verifyEqGoal k lft rgt) egs
verify k lft rgt (NotL f p) = do
  guard $ S.member (Not f) lft
  verify k lft (S.insert f rgt) p
verify k lft rgt (NotR f p) = do
  guard $ S.member (Not f) rgt
  verify k (S.insert f lft) rgt p
verify k lft rgt (OrL gls) =
  let fs = map fst gls in
  do guard $ S.member (Or fs) lft
     mapM_ (verifyLftGoal k lft rgt) gls
verify k lft rgt (OrR fs gs p) = do
  guard $ sublist gs fs
  guard $ S.member (Or fs) rgt
  verify k lft (foldl (flip S.insert) rgt gs) p
verify k lft rgt (AndL fs gs p) = do
  guard $ sublist gs fs
  guard $ S.member (And fs) lft
  verify k (foldl (flip S.insert) lft gs) rgt p
verify k lft rgt (AndR gls) =
  let fs = map fst gls in
  do guard $ S.member (And fs) rgt
     mapM_ (verifyRgtGoal k lft rgt) gls
verify k lft rgt (ImpL f g p q) = do
  guard $ S.member (Imp f g) lft
  verify k lft (S.insert f rgt) p
  verify k (S.insert g lft) rgt q
verify k lft rgt (ImpRA f g p) = do
  guard $ S.member (Imp f g) rgt
  verify k (S.insert f lft) rgt p
verify k lft rgt (ImpRC f g p) = do
  guard $ S.member (Imp f g) rgt
  verify k lft (S.insert g lft) p
verify k lft rgt (IffR f g p q) = do
  guard (S.member (Iff f g) rgt)
  verify k lft (S.insert (Imp f g) rgt) p
  verify k lft (S.insert (Imp g f) rgt) q
verify k lft rgt (IffLO f g p) = guard (S.member (Iff f g) lft) >> verify k (S.insert (Imp f g) lft) rgt p
verify k lft rgt (IffLR f g p) = guard (S.member (Iff f g) lft) >> verify k (S.insert (Imp g f) lft) rgt p
verify k lft rgt (FaL vxs f p) =
  let vs = map fst vxs in
  do guard $ S.member (Fa vs f) lft
     verify k (S.insert (substForm vxs f) lft) rgt p
verify k lft rgt (FaR vs m f p) =
  let (k', xs) = listPars m vs in
  do vxs <- zipM vs xs
     guard $ k <= m
     guard $ S.member (Fa vs f) rgt
     verify k' lft (S.insert (substForm vxs f) rgt) p
verify k lft rgt (ExL vs m f p) =
  let (k', xs) = listPars m vs in
  do vxs <- zipM vs xs
     guard $ k <= m && S.member (Ex vs f) lft
     verify k' (S.insert (substForm vxs f) lft) rgt p
verify k lft rgt (ExR vxs f p) =
  let vs = map fst vxs in
  do guard $ S.member (Ex vs f) rgt
     verify k lft (S.insert (substForm vxs f) rgt) p
verify k lft rgt (Cut f p0 p1) = do
  verify k lft (S.insert f rgt) p0
  verify k (S.insert f lft) rgt p1