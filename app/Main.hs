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
import Parse ( parseName, parsePreName, decimal, parseForm, univClose, conjecturize, readEp, estpToElabs, tstpToSteps )
import Sat ( sat )
import Lem
import Norm
import Prove
import Elab (stepsToStelabs)
import Expand (stelabsToElabs)
import Check (check)

import Control.Monad as M ( guard, MonadPlus(mzero), foldM_, when )
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative ( Alternative((<|>)) )
import System.Environment ( getArgs )
import Data.List as L ( map, foldl, all, concat, reverse, length, any, filter, delete )
import Data.Text.Lazy as T ( Text, unpack, intercalate, pack, null, splitOn, unsnoc )
import Data.Text.Lazy.Builder (Builder)
import Data.Set as S ( empty, insert, member, singleton, toList, Set )
import Data.Map as HM ( Map, empty, insert, lookup, toList, foldrWithKey, size )
import Data.Text.Lazy.IO as TIO ( hPutStrLn, hPutStr, writeFile )
import Data.Bifunctor as DBF (first, second, bimap)
import System.IO as SIO ( openFile, hClose, IOMode(WriteMode) )

putAF :: AF -> IO ()
putAF af = pb $ fmtAF af <> "\n"

addHyp :: Hyps -> AF -> Hyps
addHyp (nsq, sq) (n, _, f, _) = (HM.insert n f nsq, S.insert f sq)



addToNodes :: Set Text -> Nodes -> PreAF -> Nodes
addToNodes ahns nds (CnfAF nm rl tx) 
  | S.member nm ahns = 
    let f = (conjecturize rl $ univClose $ parseForm tx) in
    HM.insert nm (f, True, 0) nds
  | otherwise = nds
addToNodes ahns nds (FofAF nm rl tx) 
  | S.member nm ahns = 
    let f = (conjecturize rl $ parseForm tx) in
    HM.insert nm (f, True, 0) nds
  | otherwise = nds

addToHyps :: Set Text -> Hyps -> PreAF -> Hyps
addToHyps ahns hyp@(nsq, sq) (CnfAF n r tx)  
  | S.member n ahns =
      let f = (conjecturize r $ univClose $ parseForm tx) in
      (HM.insert n f nsq, S.insert f sq)
  | otherwise = hyp
addToHyps ahns hyp@(nsq, sq) (FofAF n r tx) 
  | S.member n ahns =
      let f = (conjecturize r $ parseForm tx) in
      (HM.insert n f nsq, S.insert f sq)
  | otherwise = hyp




{- Main -}

skipList :: String -> Bool
skipList n = False

ppInvranch :: Invranch -> Builder
ppInvranch br = ppListNl ppSignForm $ L.map fst (HM.toList br)

elabNote :: Stelab -> Text
elabNote (Plab _ _ n) = n
elabNote (RelD' _ _ _ n) = n
elabNote (AoC' _ _ _ _ n) = n
-- elabNote (ElabFail _ n) = n

prfHasAsm :: Prf -> Bool
prfHasAsm (Id' _) = False
prfHasAsm (EqR' _) = False
prfHasAsm (EqS' _ _) = False
prfHasAsm EqT'  {} = False
prfHasAsm FunC' {} = False
prfHasAsm RelC' {} = False
prfHasAsm (Cut' f p0 p1) = prfHasAsm p0 || prfHasAsm p1
prfHasAsm (ImpFA' _ _ p) = prfHasAsm p
prfHasAsm (ImpFC' _ _ p) = prfHasAsm p
prfHasAsm (IffTO' _ _ p) = prfHasAsm p
prfHasAsm (IffTR' _ _ p) = prfHasAsm p
prfHasAsm (ImpT' _ _ p0 p1) = prfHasAsm p0 || prfHasAsm p1
prfHasAsm (IffF' _ _ p0 p1) = prfHasAsm p0 || prfHasAsm p1
prfHasAsm (OrT' fps) = L.any (prfHasAsm . snd) fps
prfHasAsm (AndF' fps) = L.any (prfHasAsm . snd) fps
prfHasAsm (OrF' _ _ p) = prfHasAsm p
prfHasAsm (AndT' _ _ p) = prfHasAsm p
prfHasAsm (NotT' _ p) = prfHasAsm p
prfHasAsm (NotF' _ p) = prfHasAsm p
prfHasAsm (FaT' _ _ p) = prfHasAsm p
prfHasAsm (FaF' _ _ _ p) = prfHasAsm p
prfHasAsm (ExT' _ _ _ p) = prfHasAsm p
prfHasAsm (ExF' _ _ p) = prfHasAsm p
prfHasAsm (Mrk _ p) = prfHasAsm p
prfHasAsm Asm = True

elabHasAsm :: Stelab -> Bool
elabHasAsm (Plab _ p _) = prfHasAsm p
elabHasAsm (RelD' _ _ p _) = prfHasAsm p
elabHasAsm (AoC' _ _ _ p _) = prfHasAsm p

-- elabHasSjt :: Stelab -> Bool
-- elabHasSjt (Plab _ p _)     = prfHasSjt p
-- elabHasSjt (RelD' _ _ _ p _) = prfHasSjt p
-- elabHasSjt (AoC' _ _ _ p _)  = prfHasSjt p
-- 
-- prfHasSjt :: Prf -> Bool
-- prfHasSjt (Id' _) = False
-- prfHasSjt (EqR' _) = False
-- prfHasSjt (EqS' _ _) = False
-- prfHasSjt EqT'  {} = False
-- prfHasSjt FunC' {} = False
-- prfHasSjt RelC' {} = False
-- prfHasSjt (Cut f p0 p1) = prfHasSjt p0 || prfHasSjt p1
-- prfHasSjt (ImpFA' _ _ p) = prfHasSjt p
-- prfHasSjt (ImpFC' _ _ p) = prfHasSjt p
-- prfHasSjt (IffTO' _ _ p) = prfHasSjt p
-- prfHasSjt (IffTR' _ _ p) = prfHasSjt p
-- prfHasSjt (ImpT' _ _ p0 p1) = prfHasSjt p0 || prfHasSjt p1
-- prfHasSjt (IffF' _ _ p0 p1) = prfHasSjt p0 || prfHasSjt p1
-- prfHasSjt (OrT' fps) = L.any (prfHasSjt . snd) fps
-- prfHasSjt (AndF' fps) = L.any (prfHasSjt . snd) fps
-- prfHasSjt (OrF' _ _ p) = prfHasSjt p
-- prfHasSjt (AndT' _ _ p) = prfHasSjt p
-- prfHasSjt (NotT' _ p) = prfHasSjt p
-- prfHasSjt (NotF' _ p) = prfHasSjt p
-- prfHasSjt (FaT' _ _ p) = prfHasSjt p
-- prfHasSjt (FaF' _ _ _ p) = prfHasSjt p
-- prfHasSjt (ExT' _ _ _ p) = prfHasSjt p
-- prfHasSjt (ExF' _ _ p) = prfHasSjt p
-- prfHasSjt (Mrk _ p) = prfHasSjt p
-- prfHasSjt Asm = True
-- 

efForm :: EF -> Form
efForm (_, _, f, _, _, _) = f

efEP :: EF -> EP
efEP (ep, _, _, _, _, _) = ep

stepHyps :: Step -> [Text]
stepHyps (_, _, ns, _) = ns

efHyps :: EF -> [Text]
efHyps (_, _, _, _, i, _) = infHyps i

infHyps :: Inf -> [Text]
infHyps (Id n m) = [n, m]
infHyps Cut = []
infHyps (FunC ns n) = n : ns
infHyps (RelC ns n m) = n : m : ns
infHyps (EqR n) = [n]
infHyps (EqS n m) = [n, m]
infHyps (EqT n m k) = [n, m, k]
infHyps (NotT n) = [n]
infHyps (NotF n) = [n]
infHyps (OrT n) = [n]
infHyps (OrF n) = [n]
infHyps (AndT n) = [n]
infHyps (AndF n) = [n]
infHyps (ImpT n) = [n]
infHyps (ImpFA n) = [n]
infHyps (ImpFC n) = [n]
infHyps (IffTO n) = [n]
infHyps (IffTR n) = [n]
infHyps (IffF n) = [n]
infHyps (FaT n _) = [n]
infHyps (FaF n _) = [n]
infHyps (ExT n _) = [n]
infHyps (ExF n _) = [n]
infHyps RelD = []
infHyps (AoC _) = []
infHyps Open = []
  
nodeEFs :: Bool -> String -> String -> IO (Nodes, [EF])
nodeEFs vb tptp estp = do
  pt $ "TPTP : " <> pack tptp <> "\n"
  pafs <- parsePreName tptp
  pt $ "ESTP : " <> pack estp <> "\n"
  efs <- estpToElabs estp 
  let ahns = foldl (\ ns_ -> foldl (flip S.insert) ns_ . efHyps) S.empty efs
  let _nds = L.foldl (addToNodes ahns) HM.empty pafs
  let nds = L.foldl (\ mp_ (ep_, pl_, f_, k_, _, _) -> HM.insert (tlt $ ppEP ep_) (f_, pl_, k_) mp_) _nds efs
  return (nds, efs)

hypsSteps :: Bool -> String -> String -> IO (Hyps, [Step])
hypsSteps verbose tptp tstp = do
  pafs <- parsePreName tptp
  pb $ "Total hyps count =  " <> ppInt (L.length pafs) <> "\n"
  -- stps <- parseName tstp >>= mapM afToStep . sortAfs
  stps <- tstpToSteps tstp -- >>= mapM afToStep . sortAfs
  let ahns = L.foldl (\ ns_ -> foldl (flip S.insert) ns_ . stepHyps) S.empty stps
  let hs = L.foldl (addToHyps ahns) (HM.empty, S.empty) pafs
  pb $ "Active hyps count = " <> ppInt (HM.size $ fst hs) <> "\n"
  Prelude.putStr $ tptp ++ "\n"
  when verbose $ mapM_ (\ (nm_, f_) -> pb (ft nm_ <> " :: " <> ppForm f_ <> "\n")) (HM.toList $ fst hs)
  Prelude.putStr $ tstp ++ "\n"
  when verbose $ mapM_ (pb . ppStep) stps
  return (hs, stps)


writeElab :: String -> [EF] -> IO ()
writeElab nm efs = do
  let output = tlt $ ppInter "\n" $ L.map writeEF efs
  Prelude.putStrLn $ "Writing EF : " <> nm
  TIO.writeFile nm output

isVar :: Term -> Bool
isVar (Var _) = True
isVar _ = False

dev :: [String] -> IO ()
-- dev (tptp : flags)
--   | "quick" `elem` flags = do
--      afs <- parsePreName tptp
--      let k = L.length afs
--      pb $ ppInt k <> " annotated pre-formulas parsed.\n"
--   | otherwise = do
--      afs <- parseName tptp
--      let k = L.length afs
--      pb $ ppInt k <> " annotated formulas parsed.\n"
dev _ = et "invalid args"

ppFM :: HM.Map EP (Form, Bool, Int) -> Builder
ppFM fm = ppListNl (\ (ep_, (f_, pl_, _)) -> ppEP ep_ <> " : " <> ppSignForm (f_, pl_)) $ HM.toList fm

ppHM :: (a -> Builder) -> (b -> Builder) -> HM.Map a b -> Builder
ppHM f g m = ppListNl (\ (x_, y_) -> f x_ <> " : " <> g y_) $ HM.toList m

ps :: String -> IO ()
ps = Prelude.putStr

elaborate :: [String] -> IO ()
elaborate (tptp : tstp : estp : flags) = do
  let vb = "silent" `notElem` flags
  (hs, stps) <- hypsSteps vb tptp tstp
  -- -- (_, es) <- mapAccumM (elabIO verbose) hs stps
  slbs <- stepsToStelabs vb hs stps
  elbs <- stelabsToElabs (fst hs) slbs
  writeElab estp elbs
  
elaborate args = error $ "invalid arguments : " ++ unwords args
mainArgs :: [String] -> IO ()
mainArgs ("elab" : args) = elaborate args
mainArgs ("check" : tptp : estp : flags) = do
  let vb = "silent" `notElem` flags
  (nds, efs) <- nodeEFs vb tptp estp
  check vb nds efs 
mainArgs _ = et "Invalid main args"

main :: IO ()
main = getArgs >>= mainArgs
  -- case cmd of
  --   "elab" -> elaborate args
  --   "check" -> check args
  --   "dev" -> dev args
  --   _ -> et "undefined command"