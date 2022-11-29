{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Parse ( parseName, parsePreName, decimal, parseForm, univClose, proof, proofCheck,
  afToEf, conjecturize, estpToElabs, functor, parse, getText, getList )

import System.Timeout (timeout)
import Control.Monad as M ( guard, MonadPlus(mzero), foldM_, when )
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative ( Alternative((<|>)) )
import System.Environment ( getArgs )
import Data.List as L 
  ( map, foldl, all, concat, reverse, length, any, filter, delete, concatMap )
import Data.Text.Lazy as T ( Text, unpack, intercalate, pack, null, splitOn, unsnoc )
import Data.Text.Lazy.IO as TIO
    ( readFile, hPutStrLn, hPutStr, writeFile )
import Data.Text.Lazy.Builder (Builder)
import Data.Set as S ( empty, insert, member, singleton, toList, fromList, Set, union, unions )
import Data.Map as HM (Map, map, empty, insert, lookup, toList, foldrWithKey, size, fromList)
import Data.Bifunctor as DBF (first, second, bimap)
import System.IO as SIO ( openFile, hClose, IOMode(WriteMode), writeFile, Handle )

addToBranch :: Set Text -> Branch -> PreAF -> Branch
addToBranch ahns bch (CnfAF n r tx)
  | S.member n ahns =
      let f = (conjecturize r $ univClose $ parseForm tx) in
      HM.insert n (True, f) bch
  | otherwise = bch
addToBranch ahns bch (FofAF n r tx)
  | S.member n ahns =
      let f = (conjecturize r $ parseForm tx) in
      HM.insert n (True, f) bch
  | otherwise = bch

addToHyps :: Set Text -> (NTF, Set Form, SFTN) -> PreAF -> (NTF, Set Form, SFTN)
addToHyps ahns hyp@(ntf, sf, ftn) (CnfAF n r tx)
  | S.member n ahns =
      let f = (conjecturize r $ univClose $ parseForm tx) in
      (HM.insert n f ntf, S.insert f sf, HM.insert (True, f) n ftn)
  | otherwise = hyp
addToHyps ahns hyp@(ntf, sf, ftn) (FofAF n r tx)
  | S.member n ahns =
      let f = (conjecturize r $ parseForm tx) in
      (HM.insert n f ntf, S.insert f sf, HM.insert (True, f) n ftn)
  | otherwise = hyp

{- Main -}

skipList :: String -> Bool
skipList n = False

elabHyps :: Elab -> [Text]
elabHyps (_, i, _) = infHyps i

infHyps :: Inf -> [Text]
infHyps (Id n m) = [n, m]
infHyps (Cut _ _) = []
infHyps (FunC ns n) = n : ns
infHyps (RelC ns n m) = n : m : ns
infHyps (EqR n) = [n]
infHyps (EqS n m) = [n, m]
infHyps (EqT n m k) = [n, m, k]
infHyps (NotT n _) = [n]
infHyps (NotF n _) = [n]
infHyps (OrT n _) = [n]
infHyps (OrF n _ _) = [n]
infHyps (AndT n _ _) = [n]
infHyps (AndF n _) = [n]
infHyps (ImpT n _ _) = [n]
infHyps (ImpFA n _) = [n]
infHyps (ImpFC n _) = [n]
infHyps (IffTO n _) = [n]
infHyps (IffTR n _) = [n]
infHyps (IffF n _ _) = [n]
infHyps (FaT n _ _) = [n]
infHyps (FaF n _ _) = [n]
infHyps (ExT n _ _) = [n]
infHyps (ExF n _ _) = [n]
infHyps (RelD _) = []
infHyps (AoC _ _) = []
infHyps Open = []

assemble' :: HM.Map Text Elab -> Elab -> IO Proof
assemble' mp (ni, Id nt nf, _) = return $ Id_ ni nt nf
assemble' mp (ni, FunC nms nm, _) = return $ FunC_ ni nms nm
assemble' mp (ni, RelC nms nt nf, _) = return $ RelC_ ni nms nt nf
assemble' mp (ni, EqR nm, _) = return $ EqR_ ni nm
assemble' mp (ni, EqS nt nf, _) = return $ EqS_ ni nt nf
assemble' mp (ni, EqT nxy nyz nxz, _) = return $ EqT_ ni nxy nyz nxz
assemble' mp (ni, Cut nf nt, _) = do
  pf <- assemble mp nf
  pt <- assemble mp nt
  return $ Cut_ ni pf pt
assemble' mp (ni, NotT nh nc, _) = NotT_ ni nh <$> assemble mp nc
assemble' mp (ni, NotF nh nc, _) = NotF_ ni nh <$> assemble mp nc
assemble' mp (ni, OrT nh ncs, _) = do
  ps <- mapM (assemble mp) ncs
  return $ OrT_ ni nh ps
assemble' mp (ni, OrF nh k nc, _) = OrF_ ni nh k <$> assemble mp nc
assemble' mp (ni, AndT nh k nc, _) = AndT_ ni nh k <$> assemble mp nc
assemble' mp (ni, AndF nh ncs, _) = do
  ps <- mapM (assemble mp) ncs
  return $ AndF_ ni nh ps
assemble' mp (ni, ImpT nh na nc, _) = do
  pa <- assemble mp na
  pc <- assemble mp nc
  return $ ImpT_ ni nh pa pc
assemble' mp (ni, ImpFA nh nc, _) = ImpFA_ ni nh <$> assemble mp nc
assemble' mp (ni, ImpFC nh nc, _) = ImpFC_ ni nh <$> assemble mp nc
assemble' mp (ni, IffTO nh nc, _) = IffTO_ ni nh <$> assemble mp nc
assemble' mp (ni, IffTR nh nc, _) = IffTR_ ni nh <$> assemble mp nc
assemble' mp (ni, IffF nh no nr, _) = do
  po <- assemble mp no
  pr <- assemble mp nr
  return $ IffF_ ni nh po pr
assemble' mp (ni, FaT nh xs nc, _) = FaT_ ni nh xs <$> assemble mp nc
assemble' mp (ni, FaF nh k nc, _) = FaF_ ni nh k <$> assemble mp nc
assemble' mp (ni, ExT nh k nc, _) = ExT_ ni nh k <$> assemble mp nc
assemble' mp (ni, ExF nh xs nc, _) = ExF_ ni nh xs <$> assemble mp nc
assemble' mp (ni, RelD nc, _) = RelD_ ni <$> assemble mp nc
assemble' mp (ni, AoC xs nc, _) = AoC_ ni xs <$> assemble mp nc
assemble' mp (ni, Open, _) = return $ Open_ ni

assemble :: HM.Map Text Elab -> Text -> IO Proof
assemble mp nm = cast (HM.lookup nm mp) >>= assemble' mp

elabName :: Elab -> Text
elabName ((nm, _, _), _, _) = nm

readCstp :: String -> IO ([Text], Text)
readCstp cstp = do
  tx <- TIO.readFile cstp
  cast $ parse (getList getText) tx

readTptp :: [Text] -> String -> IO Branch
readTptp nms tptp = do
  pafs <- parsePreName tptp
  return $ L.foldl (addToBranch $ S.fromList nms) HM.empty pafs

writeElab :: String -> [Elab] -> IO ()
writeElab nm efs = do
  Prelude.putStrLn $ "Writing Elab : " <> nm
  let output = tlt $ ppInter "\n" $ L.map ppElab efs
  TIO.writeFile nm output

proofNames :: Proof -> Set Text
proofNames (Id_ _ nt nf) = S.fromList [nt, nf]
proofNames (Cut_ _ pf pt) = S.union (proofNames pf) (proofNames pt)
proofNames (FunC_ _ nts nf) = S.fromList (nf : nts)
proofNames (RelC_ _ nts nt nf) = S.fromList (nt : nf : nts)
proofNames (EqR_ _ nf) = S.fromList [nf]
proofNames (EqS_ _ nt nf) = S.fromList [nt, nf]
proofNames (EqT_ _ nxy nyz nxz) = S.fromList [nxy, nyz, nxz]
proofNames (NotT_ _ nm p) = S.insert nm $ proofNames p
proofNames (NotF_ _ nm p) = S.insert nm $ proofNames p
proofNames (OrT_ _ nm ps) = S.insert nm $ S.unions $ L.map proofNames ps
proofNames (OrF_ _ nm _ p) = S.insert nm $ proofNames p
proofNames (AndT_ _ nm _ p) = S.insert nm $ proofNames p
proofNames (AndF_ _ nm ps) = S.insert nm $ S.unions $ L.map proofNames ps
proofNames (ImpT_ _ nm pa pc) = S.insert nm $ S.union (proofNames pa) (proofNames pc)
proofNames (ImpFA_ _ nm p) = S.insert nm $ proofNames p
proofNames (ImpFC_ _ nm p) = S.insert nm $ proofNames p
proofNames (IffTO_ _ nm p) = S.insert nm $ proofNames p
proofNames (IffTR_ _ nm p) = S.insert nm $ proofNames p
proofNames (IffF_ _ nm po pr) = S.insert nm $ S.union (proofNames po) (proofNames pr)
proofNames (FaT_ _ nm _ p) = S.insert nm $ proofNames p
proofNames (FaF_ _ nm _ p) = S.insert nm $ proofNames p
proofNames (ExT_ _ nm _ p) = S.insert nm $ proofNames p
proofNames (ExF_ _ nm _ p) = S.insert nm $ proofNames p
proofNames (RelD_ _ p) = proofNames p
proofNames (AoC_ _ _ p) = proofNames p
proofNames (Open_ _) = S.empty

isVar :: Term -> Bool
isVar (Var _) = True
isVar _ = False

ps :: String -> IO ()
ps = Prelude.putStr

elabInf :: Elab -> Inf
elabInf (_, i, _) = i

mainArgs ("check" : tptp : astp : flags) = do
  let vb = "silent" `notElem` flags
  when vb $ ps $ "Reading ASTP : " ++ astp ++ " ...\n"
  (nms, prfTx) <- readCstp astp
  when vb $ ps $ "Reading TPTP : " ++ tptp ++ " ...\n"
  bch <- readTptp nms tptp
  ((), rem) <- cast $ parse (proofCheck vb 0 bch True (And [])) prfTx 
  guard (T.null rem)
  pt "Proof checked.\n"

mainArgs ("extract" : tptp : astp : estp : flags) = do
  let vb = "silent" `notElem` flags
  when vb $ ps $ "Reading ASTP : " ++ astp ++ " ...\n"
  (nms, prfTx) <- readCstp astp
  when vb $ ps $ "Reading TPTP : " ++ tptp ++ " ...\n"
  bch <- readTptp nms tptp
  (prf, rem) <- cast $ parse (proof bch True (And [])) prfTx 
  guard (T.null rem)
  TIO.writeFile estp $ tlt $ ppListNl ppElab $ linearize prf

mainArgs ("assemble" : estp : astp : flags) = do
  elbs <- parseName estp >>= mapM afToEf 
  let nms = L.foldl S.union S.empty (L.map (S.fromList . infHyps . elabInf) elbs)
  let m = L.foldl (\ m_ elb_ -> HM.insert (elabName elb_) elb_ m_) HM.empty elbs
  prf <- assemble m "root"
  writeProof astp (S.toList nms) prf
mainArgs _ = et "Invalid main args"

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

main :: IO ()
main = getArgs >>= mainArgs