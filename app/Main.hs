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

-- addToBranch :: Set BS -> Branch -> PreAF -> Branch
-- addToBranch ahns bch (n, r, bs, _)
--   | S.member n ahns =
--       let f = (conjecturize r $ univClose $ parseForm bs) in
--       HM.insert n (True, f) bch
--   | otherwise = bch

-- addToBranch ahns bch (FofAF n r tx _)
--   | S.member n ahns =
--       let f = (conjecturize r $ parseForm tx) in
--       HM.insert n (True, f) bch
--   | otherwise = bch

-- addToHyps :: Set BS -> (NTF, Set Form, SFTN) -> PreAF -> (NTF, Set Form, SFTN)
-- addToHyps ahns hyp@(ntf, sf, ftn) (CnfAF n r tx)
--   | S.member n ahns =
--       let f = (conjecturize r $ univClose $ parseForm tx) in
--       (HM.insert n f ntf, S.insert f sf, HM.insert (True, f) n ftn)
--   | otherwise = hyp
-- addToHyps ahns hyp@(ntf, sf, ftn) (FofAF n r tx)
--   | S.member n ahns =
--       let f = (conjecturize r $ parseForm tx) in
--       (HM.insert n f ntf, S.insert f sf, HM.insert (True, f) n ftn)
--   | otherwise = hyp

{- Main -}


-- pafToEf :: PreAF -> IO Elab'
-- pafToEf (nm, sgn, bs, Just (Gfun "inference" [gt], gts)) = do
--   pl <- textToBool sgn
--   i <- gTermToInf gt
--   mtx <- gTermsToMaybeBS gts
--   return ((nm, pl, bs), i, mtx)
-- pafToEf af = error "cannot read AF into Elab" -- <> tlt (fmtAF af)

-- afToEf :: Anf -> IO Elab
-- afToEf (nm, sgn, f, Just (Gfun "inference" [gt], gts)) = do
--   pl <- textToBool sgn
--   i <- gTermToInf gt
--   mtx <- gTermsToMaybeBS gts
--   return ((nm, pl, f), i, mtx)
-- afToEf af = error "cannot read AF into Elab" -- <> tlt (fmtAF af)
-- 
-- estpToElabs :: String -> IO [Elab]
-- estpToElabs estp = do
--   ps "Reading ESTP file...\n"
--   xs <- parseName estp
--   ps "Transcribing AFs to EFs...\n"
--   mapM afToEf xs
-- 
skipList :: String -> Bool
skipList n = False

elabHyps :: Elab -> [BS]
elabHyps (_, i, _) = infHyps i

infHyps :: Inf -> [BS]
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

-- nip :: Node' -> Node
-- nip (x, y, bs) = (x, y, parseForm bs)

-- pssemble' :: HM.Map BS Elab' -> Elab' -> IO Proof
-- pssemble' mp (ni, Id nt nf, _) = return $ Id_ (nip ni) nt nf
-- pssemble' mp (ni, FunC nms nm, _) = return $ FunC_ (nip ni) nms nm
-- pssemble' mp (ni, RelC nms nt nf, _) = return $ RelC_ (nip ni) nms nt nf
-- pssemble' mp (ni, EqR nm, _) = return $ EqR_ (nip ni) nm
-- pssemble' mp (ni, EqS nt nf, _) = return $ EqS_ (nip ni) nt nf
-- pssemble' mp (ni, EqT nxy nyz nxz, _) = return $ EqT_ (nip ni) nxy nyz nxz
-- pssemble' mp (ni, Cut nf nt, _) = do
--   pf <- pssemble mp nf
--   pt <- pssemble mp nt
--   return $ Cut_ (nip ni) pf pt
-- pssemble' mp (ni, NotT nh nc, _) = NotT_ (nip ni) nh <$> pssemble mp nc
-- pssemble' mp (ni, NotF nh nc, _) = NotF_ (nip ni) nh <$> pssemble mp nc
-- pssemble' mp (ni, OrT nh ncs, _) = do
--   ps <- mapM (pssemble mp) ncs
--   return $ OrT_ (nip ni) nh ps
-- pssemble' mp (ni, OrF nh k nc, _) = OrF_ (nip ni) nh k <$> pssemble mp nc
-- pssemble' mp (ni, AndT nh k nc, _) = AndT_ (nip ni) nh k <$> pssemble mp nc
-- pssemble' mp (ni, AndF nh ncs, _) = do
--   ps <- mapM (pssemble mp) ncs
--   return $ AndF_ (nip ni) nh ps
-- pssemble' mp (ni, ImpT nh na nc, _) = do
--   pa <- pssemble mp na
--   pc <- pssemble mp nc
--   return $ ImpT_ (nip ni) nh pa pc
-- pssemble' mp (ni, ImpFA nh nc, _) = ImpFA_ (nip ni) nh <$> pssemble mp nc
-- pssemble' mp (ni, ImpFC nh nc, _) = ImpFC_ (nip ni) nh <$> pssemble mp nc
-- pssemble' mp (ni, IffTO nh nc, _) = IffTO_ (nip ni) nh <$> pssemble mp nc
-- pssemble' mp (ni, IffTR nh nc, _) = IffTR_ (nip ni) nh <$> pssemble mp nc
-- pssemble' mp (ni, IffF nh no nr, _) = do
--   po <- pssemble mp no
--   pr <- pssemble mp nr
--   return $ IffF_ (nip ni) nh po pr
-- pssemble' mp (ni, FaT nh xs nc, _) = FaT_ (nip ni) nh xs <$> pssemble mp nc
-- pssemble' mp (ni, FaF nh k nc, _) = FaF_ (nip ni) nh k <$> pssemble mp nc
-- pssemble' mp (ni, ExT nh k nc, _) = ExT_ (nip ni) nh k <$> pssemble mp nc
-- pssemble' mp (ni, ExF nh xs nc, _) = ExF_ (nip ni) nh xs <$> pssemble mp nc
-- pssemble' mp (ni, RelD nc, _) = RelD_ (nip ni) <$> pssemble mp nc
-- pssemble' mp (ni, AoC xs nc, _) = AoC_ (nip ni) xs <$> pssemble mp nc
-- pssemble' mp (ni, Open, _) = return $ Open_ (nip ni)

-- pssemble :: HM.Map BS Elab' -> BS -> IO Proof
-- pssemble mp nm = cast (HM.lookup nm mp) >>= pssemble' mp

assemble' :: ESTP -> Node -> Inf -> IO Proof
assemble' mp ni (Id nt nf) = return $ Id_ ni nt nf
assemble' mp ni (FunC nms nm) = return $ FunC_ ni nms nm
assemble' mp ni (RelC nms nt nf) = return $ RelC_ ni nms nt nf
assemble' mp ni (EqR nm) = return $ EqR_ ni nm
assemble' mp ni (EqS nt nf) = return $ EqS_ ni nt nf
assemble' mp ni (EqT nxy nyz nxz) = return $ EqT_ ni nxy nyz nxz
assemble' mp ni (Cut nf nt) = do
  pf <- assemble mp nf
  pt <- assemble mp nt
  return $ Cut_ ni pf pt
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
assemble' mp ni (RelD nc) = RelD_ ni <$> assemble mp nc
assemble' mp ni (AoC xs nc) = AoC_ ni xs <$> assemble mp nc
assemble' mp ni Open = return $ Open_ ni

assemble :: ESTP -> BS -> IO Proof
assemble mp nm = do 
  (b, f, i) <- cast (HM.lookup nm mp) 
  assemble' mp (nm, b, f) i

elabName :: Elab -> BS
elabName ((nm, _, _), _, _) = nm

-- readAstp :: String -> IO ([BS], BS)
-- readAstp cstp = do
--   tx <- BS.readFile cstp
--   cast $ parse (getList getBS) tx
-- 
-- readTptp :: [BS] -> String -> IO Branch
-- readTptp nms tptp = do
--   pafs <- parsePreName tptp
--   return $ L.foldl (addToBranch $ S.fromList nms) HM.empty pafs
-- 
-- readTptp nms tptp = do
--   pafs <- parsePreName tptp
--   return $ L.foldl (addToBranch $ S.fromList nms) HM.empty pafs
-- 
afName :: Anf -> BS
afName (nm, _, _, _) = nm

afSignForm :: Anf -> SignForm
afSignForm (_, _, f, _) = (True, f)

writeElab :: String -> [Elab] -> IO ()
writeElab nm efs = do
  Prelude.putStrLn $ "Writing Elab : " <> nm
  let output = ppInter "\n" $ L.map ppElab efs
  BD.writeFile nm output

proofNames :: Proof -> Set BS
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

triFst :: (a, b, c) -> a
triFst (x, _, _) = x

triSnd :: (a, b, c) -> b
triSnd (_, x, _) = x

-- asmWrite' :: Bool -> String -> String -> IO ()
-- asmWrite' vb estp astp = do 
--   when vb $ ps $ "Reading ESTP : " <> estp <> " ...\n"
--   plbs <- parsePreName estp >>= mapM pafToEf 
--   when vb $ ps "Collecting hypothesis names...\n"
--   let nms = L.foldl S.union S.empty (L.map (S.fromList . infHyps . triSnd) plbs)
--   when vb $ ps "Constructing node map...\n"
--   let m = L.foldl (\ m_ elb_ -> HM.insert (triFst $ triFst elb_) elb_ m_) HM.empty plbs
--   when vb $ ps "Assembling proof...\n"
--   prf <- pssemble m "root"
--   when vb $ ps "Writing proof...\n"
--   writeProof astp (S.toList nms) prf

-- asmWrite :: Bool -> String -> String -> IO ()
-- asmWrite vb enm anm = do 
--   estp <- nameParseEstp enm
--   prf <- assemble estp "root"
--   writeProof anm prf

-- asmWrite vb estp astp = do 
--   when vb $ ps $ "Reading ESTP : " <> estp <> " ...\n"
--   elbs <- parseName estp >>= mapM afToEf 
--   when vb $ ps "Collecting hypothesis names...\n"
--   let nms = L.foldl S.union S.empty (L.map (S.fromList . infHyps . triSnd) elbs)
--   when vb $ ps "Constructing node map...\n"
--   let m = L.foldl (\ m_ elb_ -> HM.insert (elabName elb_) elb_ m_) HM.empty elbs
--   when vb $ ps "Assembling proof...\n"
--   prf <- assemble m "root"
--   when vb $ ps "Writing proof...\n"
--   writeProof astp (S.toList nms) prf

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

mainArgs :: [String] -> IO ()
mainArgs ("assemble" : enm : anm : flags) = do
  let vb = "silent" `notElem` flags
  estp <- readEstp enm
  prf <- assemble estp "root"
  writeProof anm prf
mainArgs ("check" : pnm : anm : flags) = do
  let vb = "silent" `notElem` flags
  when vb $ ps $ "Reading TPTP : " ++ pnm ++ " ...\n"
  bch <- readBranch pnm HM.empty
  when vb $ ps $ "Reading ASTP : " ++ anm ++ " ...\n"
  bs <- BS.readFile anm
  runParser (proofCheck vb 0 bch True (And [])) bs
  ps "Proof checked.\n"
-- mainArgs ("extract" : tptp : astp : estp : flags) = do
--   let vb = "silent" `notElem` flags
--   when vb $ ps $ "Reading ASTP : " ++ astp ++ " ...\n"
--   (nms, prfTx) <- readAstp astp
--   when vb $ ps $ "Reading TPTP : " ++ tptp ++ " ...\n"
--   bch <- readTptp nms tptp
--   (prf, rem) <- cast $ parse (proof bch True (And [])) prfTx 
--   guard (BS.null rem)
--   (Just ()) <- timeout 120000000 $ BD.writeFile estp $ ppListNl ppElab $ linearize prf
--   skip
-- 

mainArgs _ = error "Invalid main args"

main :: IO ()
main = getArgs >>= mainArgs