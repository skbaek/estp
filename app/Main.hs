{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use infix" #-}

module Main where

import Types
import Basic
import PP
import Parse -- ( parseName, parsePreName, decimal, parseForm, univClose, proof, gTermToInf, gTermsToMaybeBS, proofCheck, conjecturize, functor, parse, getBS, getList )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BD
import Control.Monad as M ( guard, MonadPlus(mzero), foldM_, when )
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative ( Alternative((<|>)) )
import System.Environment ( getArgs )
import Data.List as L 
  ( map, foldl, all, concat, reverse, length, any, filter, delete, concatMap )
import Data.Set as S ( empty, insert, member, singleton, toList, fromList, Set, union, unions )
import Data.Map as HM (Map, map, empty, insert, lookup, toList, foldrWithKey, size, fromList)
import Data.Bifunctor as DBF (first, second, bimap)
import System.IO as SIO ( openFile, hClose, IOMode(WriteMode), writeFile, Handle )
import System.Timeout (timeout)

assemble' :: Sol -> Node -> Inf -> IO Proof
assemble' mp ni (Id nt nf) = return $ Id_ ni nt nf
assemble' mp ni (FunC nms nm) = return $ FunC_ ni nms nm
assemble' mp ni (RelC nms nt nf) = return $ RelC_ ni nms nt nf
assemble' mp ni (EqR nm) = return $ EqR_ ni nm
assemble' mp ni (EqS nt nf) = return $ EqS_ ni nt nf
assemble' mp ni (EqT nxy nyz nxz) = return $ EqT_ ni nxy nyz nxz
assemble' mp ni (Cut f nf nt) = do
  pf <- assemble mp nf
  pt <- assemble mp nt
  return $ Cut_ ni f pf pt
assemble' mp ni (NotT nh nc) = NotT_ ni nh <$> assemble mp nc
assemble' mp ni (NotF nh nc) = NotF_ ni nh <$> assemble mp nc
assemble' mp ni (OrT nh ncs) = do
  ps <- mapM (assemble mp) ncs
  return $ OrT_ ni nh ps
assemble' mp ni (OrF nh k nc) = OrF_ ni nh k <$> assemble mp nc
assemble' mp ni (AndT nh k nc) = AndT_ ni nh k <$> assemble mp nc
assemble' mp ni (AndF nh ncs) = do
  ps <- mapM (assemble mp) ncs
  return $ AndF_ ni nh ps
assemble' mp ni (ImpT nh na nc) = do
  pa <- assemble mp na
  pc <- assemble mp nc
  return $ ImpT_ ni nh pa pc
assemble' mp ni (ImpFA nh nc) = ImpFA_ ni nh <$> assemble mp nc
assemble' mp ni (ImpFC nh nc) = ImpFC_ ni nh <$> assemble mp nc
assemble' mp ni (IffTO nh nc) = IffTO_ ni nh <$> assemble mp nc
assemble' mp ni (IffTR nh nc) = IffTR_ ni nh <$> assemble mp nc
assemble' mp ni (IffF nh no nr) = do
  po <- assemble mp no
  pr <- assemble mp nr
  return $ IffF_ ni nh po pr
assemble' mp ni (FaT nh xs nc) = FaT_ ni nh xs <$> assemble mp nc
assemble' mp ni (FaF nh k nc) = FaF_ ni nh k <$> assemble mp nc
assemble' mp ni (ExT nh k nc) = ExT_ ni nh k <$> assemble mp nc
assemble' mp ni (ExF nh xs nc) = ExF_ ni nh xs <$> assemble mp nc
assemble' mp ni (RelD f nc) = RelD_ ni f <$> assemble mp nc
assemble' mp ni (AoC x f nc) = AoC_ ni x f <$> assemble mp nc
assemble' mp ni Open = return $ Open_ ni

assemble :: Sol -> BS -> IO Proof
assemble mp nm = do 
  (b, f, i) <- cast (HM.lookup nm mp) 
  assemble' mp (nm, b, f) i

linearize :: Proof -> [Elab]
linearize (Id_ ni nt nf) = [(ni, Id nt nf)]
linearize (Cut_ ni f p q) = (ni, Cut f (proofRN p) (proofRN q)) : linearize p ++ linearize q
linearize (FunC_ ni xs nm) = [(ni, FunC xs nm)]
linearize (RelC_ ni xs nt nf) = [(ni, RelC xs nt nf)]
linearize (EqR_ ni nm) = [(ni, EqR nm)]
linearize (EqS_ ni nt nf) = [(ni, EqS nt nf)]
linearize (EqT_ ni nxy nyz nxz) = [(ni, EqT nxy nyz nxz)]
linearize (NotT_ ni nm p) = (ni, NotT nm (proofRN p)) : linearize p
linearize (NotF_ ni nm p) = (ni, NotF nm (proofRN p)) : linearize p
linearize (OrT_ ni nm ps) = (ni, OrT nm (L.map proofRN ps)) : L.concatMap linearize ps
linearize (OrF_ ni nm k p) = (ni, OrF nm k (proofRN p)) : linearize p
linearize (AndT_ ni nm k p) = (ni, AndT nm k (proofRN p)) : linearize p
linearize (AndF_ ni nm ps) = (ni, AndF nm (L.map proofRN ps)) : L.concatMap linearize ps
linearize (ImpT_ ni nm p q) = (ni, ImpT nm (proofRN p) (proofRN q)) : linearize p ++ linearize q
linearize (ImpFA_ ni nm p) = (ni, ImpFA nm (proofRN p)) : linearize p
linearize (ImpFC_ ni nm p) = (ni, ImpFC nm (proofRN p)) : linearize p
linearize (IffTO_ ni nm p) = (ni, IffTO nm (proofRN p)) : linearize p
linearize (IffTR_ ni nm p) = (ni, IffTR nm (proofRN p)) : linearize p
linearize (IffF_ ni nm p q) = (ni, IffF nm (proofRN p) (proofRN q)) : linearize p ++ linearize q
linearize (FaT_ ni nm xs p) = (ni, FaT nm xs (proofRN p)) : linearize p
linearize (FaF_ ni nm k p) = (ni, FaF nm k (proofRN p)) : linearize p
linearize (ExT_ ni nm k p) = (ni, ExT nm k (proofRN p)) : linearize p
linearize (ExF_ ni nm xs p) = (ni, ExF nm xs (proofRN p)) : linearize p
linearize (RelD_ ni f p) = (ni, RelD f (proofRN p)) : linearize p
linearize (AoC_ ni x f p) = (ni, AoC x f (proofRN p)) : linearize p
linearize (Open_ ni) = [(ni, Open)]

mainArgs :: [String] -> IO ()
mainArgs ("assemble" : enm : anm : flags) = do
  let vb = "silent" `notElem` flags
  ps "Reading in elaborated formulas...\n"
  estp <- readEstp enm
  ps "Asssembling proof...\n"
  prf <- assemble estp "root"
  ps "Writing proof to file...\n"
  writeProof anm prf
mainArgs ("check" : pnm : anm : flags) = do
  let vb = "silent" `notElem` flags
  when vb $ ps $ "Reading TPTP : " ++ pnm ++ " ...\n"
  tptp <- readTptp pnm HM.empty
  let bch = HM.map (True,) tptp -- readBranch pnm HM.empty
  when vb $ ps $ "Reading ASTP : " ++ anm ++ " ...\n"
  bs <- BS.readFile anm
  runParser (proofCheck vb 0 bch True (And [])) bs
  ps "Proof checked.\n"
mainArgs ("extract" : pnm : anm : enm : flags) = do
  let vb = "silent" `notElem` flags
  when vb $ ps $ "Reading TPTP : " ++ pnm ++ " ...\n"
  tptp <- readTptp pnm HM.empty
  let bch = HM.map (True,) tptp -- readBranch pnm HM.empty
  when vb $ ps $ "Reading ASTP : " ++ anm ++ " ...\n"
  bs <- BS.readFile anm
  prf <- runParser (proof bch True (And [])) bs 
  BD.writeFile enm $ ppListNl ppElab $ linearize prf
mainArgs _ = error "Invalid main args"

main :: IO ()
main = getArgs >>= mainArgs