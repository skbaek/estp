{-# LANGUAGE OverloadedStrings #-}

module Check where

import Types
import Basic
import PP
import Data.List (map, foldl)
import Data.Text as T (unpack, pack)
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList )
import Control.Monad as M (guard, foldM, foldM_ ,(>=>))
import Control.Applicative ( Alternative((<|>)) )

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
verify _ _ _ Sorry = return ()
verify _ lft rgt (Ax f) = do
  let rhs_text = ppListNl ppForm (S.toList rgt)
  guard (S.member f lft) <|> error "Ax-fail-1"
  guard (S.member f rgt) <|> error (unpack $ "RHS missing : " <> ppForm f <> "\nRHS = " <> rhs_text)
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
  let xs = map (\ (x, _, _) -> x) egs 
  let ys = map (\ (_, y, _) -> y) egs 
  guard (S.member (Eq (Fun f xs) (Fun f ys)) rgt) <|> error "FunC-fail"
  mapM_ (verifyEqGoal k lft rgt) egs
verify k lft rgt (RelC r egs) = do
  let xs = map (\ (x, _, _) -> x) egs
  let ys = map (\ (_, y, _) -> y) egs
  guard (S.member (Rel r xs) lft && S.member (Rel r ys) rgt) <|> error "RelC-fail"
  mapM_ (verifyEqGoal k lft rgt) egs
verify k lft rgt (NotL f p) = do
  guard (S.member (Not f) lft) <|> error "NotL-fail"

  verify k lft (S.insert f rgt) p
verify k lft rgt (NotR f p) = do
  guard (S.member (Not f) rgt) <|> error "NotR-fail"
  verify k (S.insert f lft) rgt p
verify k lft rgt (OrL gls) = do
  let fs = map fst gls 
  guard (S.member (Or fs) lft) <|> error "OrL-fail"
  mapM_ (verifyLftGoal k lft rgt) gls
verify k lft rgt (OrR fs gs p) = do
  guard (sublist gs fs && S.member (Or fs) rgt) <|> error "OrR-fail"
  verify k lft (foldl (flip S.insert) rgt gs) p
verify k lft rgt (AndL fs gs p) = do
  guard (sublist gs fs && S.member (And fs) lft) <|> error "AndL-fail"
  verify k (foldl (flip S.insert) lft gs) rgt p
verify k lft rgt (AndR gls) = do
  let fs = map fst gls 
  guard (S.member (And fs) rgt) <|> error "AndR-fail"
  mapM_ (verifyRgtGoal k lft rgt) gls
verify k lft rgt (ImpL f g p q) = do
  guard (S.member (Imp f g) lft) <|> error "ImpL-fail"
  verify k lft (S.insert f rgt) p
  verify k (S.insert g lft) rgt q
verify k lft rgt (ImpRA f g p) = do
  guard (S.member (Imp f g) rgt) <|> error "ImpRA-fail"
  verify k (S.insert f lft) rgt p
verify k lft rgt (ImpRC f g p) = do
  guard (S.member (Imp f g) rgt) <|> error "ImpRC-fail"
  verify k lft (S.insert g rgt) p
verify k lft rgt (IffR f g p q) = do
  guard (S.member (Iff f g) rgt) <|> error "IffR-fail"
  verify k lft (S.insert (Imp f g) rgt) p
  verify k lft (S.insert (Imp g f) rgt) q
verify k lft rgt (IffLO f g p) = do 
  guard (S.member (Iff f g) lft) <|> error (unpack $ "IffLO-fail" <> ppForm (f <=> g))
  verify k (S.insert (Imp f g) lft) rgt p
verify k lft rgt (IffLR f g p) = do 
  guard (S.member (Iff f g) lft) <|> error "IffLR-fail"
  verify k (S.insert (Imp g f) lft) rgt p
verify k lft rgt (FaL vxs f p) = do
  let vs = map fst vxs 
  guard (S.member (Fa vs f) lft) <|> error "FaL-fail"
  verify k (S.insert (substForm vxs f) lft) rgt p
verify k lft rgt (FaR vs m f p) = do
  let (k', xs) = listPars m vs 
  vxs <- zipM vs xs <|> error "FaR-fail"
  guard (k <= m && S.member (Fa vs f) rgt) <|> error "FaR-fail"
  verify k' lft (S.insert (substForm vxs f) rgt) p
verify k lft rgt (ExL vs m f p) = do
  let (k', xs) = listPars m vs 
  vxs <- zipM vs xs <|> error "ExL-fail"
  guard (k <= m && S.member (Ex vs f) lft) <|> error "ExL-fail"
  verify k' (S.insert (substForm vxs f) lft) rgt p
verify k lft rgt (ExR vxs f p) = do
  let vs = map fst vxs 
  guard (S.member (Ex vs f) rgt) <|> error "ExR-fail"
  verify k lft (S.insert (substForm vxs f) rgt) p
verify k lft rgt (Cut f p0 p1) = do
  verify k lft (S.insert f rgt) p0
  verify k (S.insert f lft) rgt p1