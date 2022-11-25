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
import Parse ( parseName, parsePreName, decimal, parseForm, univClose,
  conjecturize, estpToElabs, tstpToSteps, functor, parse )
import Sat ( sat )
import Lem
import Norm
import Prove
import Elab (elaborate, linearize)
-- import Expand (stelabsToElabs)
import Check (check, getText, getList, pcheck)

import Control.Monad as M ( guard, MonadPlus(mzero), foldM_, when )
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative ( Alternative((<|>)) )
import System.Environment ( getArgs )
import Data.List as L ( map, foldl, all, concat, reverse, length, any, filter, delete, isPrefixOf, stripPrefix )
import Data.Text.Lazy as T ( Text, unpack, intercalate, pack, null, splitOn, unsnoc )
import Data.Text.Lazy.IO as TIO
    ( readFile, hPutStrLn, hPutStr, writeFile )
import Data.Text.Lazy.Builder (Builder)
import Data.Set as S ( empty, insert, member, singleton, toList, fromList, Set, union, unions )
import Data.Map as HM (Map, map, empty, insert, lookup, toList, foldrWithKey, size, fromList)
import Data.Bifunctor as DBF (first, second, bimap)
import System.IO as SIO ( openFile, hClose, IOMode(WriteMode), writeFile, Handle )
-- import Data.Strings (strStartsWith) 

addHyp :: (NTF, Set Form) -> AF -> (NTF, Set Form)
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

ppInvranch :: Invranch -> Builder
ppInvranch br = ppListNl (\ (f_, b_) -> ppSignForm (b_, f_)) $ L.map fst (HM.toList br)

-- elabNote :: Stelab -> Text
-- elabNote (InfStep _ _ n) = n
-- elabNote (DefStep _ _ _ n) = n
-- elabNote (AoCStep _ _ _ _ n) = n

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
prfHasAsm Open' = True

stepHyps :: Step -> [Text]
stepHyps (_, _, ns, _) = ns

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

branchProof :: Bool -> String -> String -> IO (Branch, Proof)
branchProof vb tptp estp = do
  pt "Perusing TPTP file...\n"
  pafs <- parsePreName tptp
  elbs <- estpToElabs estp
  let ahns = L.foldl (\ ns_ -> foldl (flip S.insert) ns_ . elabHyps) S.empty elbs
  let bch = L.foldl (addToBranch ahns) HM.empty pafs
  let elbsMap = HM.fromList $ L.map (\ elb_ -> (elabName elb_, elb_)) elbs
  pt "Constructing proof tree...\n"
  prf <- assemble elbsMap "root"
  return (bch, prf)

hypsSteps :: Bool -> String -> String -> IO (NTF, Set Form, SFTN, [Step])
hypsSteps verbose tptp tstp = do
  pafs <- parsePreName tptp
  pb $ "Total hyps count =  " <> ppInt (L.length pafs) <> "\n"
  -- stps <- parseName tstp >>= mapM afToStep . sortAfs
  stps <- tstpToSteps tstp -- >>= mapM afToStep . sortAfs
  let ahns = L.foldl (\ ns_ -> foldl (flip S.insert) ns_ . stepHyps) S.empty stps
  let (ntf, sf, ftn) = L.foldl (addToHyps ahns) (HM.empty, S.empty, HM.empty) pafs
  pb $ "Active hyps count = " <> ppInt (HM.size ntf) <> "\n"
  Prelude.putStr $ tptp ++ "\n"
  when verbose $ mapM_ (\ (nm_, f_) -> pb (ft nm_ <> " :: " <> ppForm f_ <> "\n")) (HM.toList ntf)
  Prelude.putStr $ tstp ++ "\n"
  when verbose $ mapM_ (pb . ppStep) stps
  return (ntf, sf, ftn, stps)

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

writeProof :: String -> Proof -> IO ()
writeProof nm prf = do
  Prelude.putStrLn $ "Writing proof : " <> nm
  TIO.writeFile nm $ tlt $ serList serText (S.toList $ proofNames prf) <> serProof prf

isVar :: Term -> Bool
isVar (Var _) = True
isVar _ = False

ps :: String -> IO ()
ps = Prelude.putStr

afName :: AF -> Text
afName (n, _, _, _) = n

isExtraAF :: Text -> Bool
isExtraAF ('e' :> _) = True
isExtraAF _ = False

breakExtra :: Text -> IO Int
breakExtra ('e' :> tx) = cast $ readInt tx
breakExtra _ = mzero

redefine :: [Int] -> AF -> IO AF
redefine ks ('f' :> tx, lang, g, Just (Gfun "introduced" [Gfun "predicate_definition_introduction" [],
  Glist [Gfun "new_symbols" [Gfun "naming" [],Glist [Gfun r []]]]], _)) = do
    k <- cast $ readInt tx
    guard (k `elem` ks)
    return ("f" <> tx, lang, g, Just (Gfun "inference" [Gfun "usedef" [], Glist [], Glist [Gfun ("e" <> tlt (ppInt k)) []]], Nothing))
redefine ks af@('f' :> tx, lang, g, Just (Gfun "introduced" [Gfun "avatar_definition" [],
  Glist [Gfun "new_symbols" [Gfun "naming" [], Glist [Gfun r []]]]], _)) = do
    k <- cast $ readInt tx
    -- pb $ "Is it in list? : " <> ppInt k <> "\n"
    -- guard (k `elem` ks)
    -- return ("f" <> tx, lang, g, Just (Gfun "inference" [Gfun "usedef" [], Glist [], Glist [Gfun ("e" <> tlt (ppInt k)) []]], Nothing)) 
    guard (k `notElem` ks)
    return af
redefine _ af = return af

mainArgs :: [String] -> IO ()
mainArgs ("elab" : tptp : tstp : estp : flags) = do
  let vb = "silent" `notElem` flags
  when vb $ pt "Reading problem and solution...\n"
  (ntf, sf, ftn, stps) <- hypsSteps vb tptp tstp
  when vb $ pt "Elaborating solution...\n"
  -- elbs <- elaborate vb ntf sf ftn stps
  prf <- elaborate vb ntf sf ftn stps
  when vb $ pt "Writing proof : \n"
  -- pb $ ppListNl ppElab (linearize prf) <> "\n"
  -- pb "checking before output...\n"
  -- check vb 0 (HM.map (True,) ntf) prf
  writeProof estp prf

mainArgs ("check" : tptp : cstp : flags) = do
  let vb = "silent" `notElem` flags
  when vb $ ps $ "Reading ESTP : " ++ cstp ++ " ...\n"
  (nms, prfTx) <- readCstp cstp
  when vb $ ps $ "Reading TPTP : " ++ tptp ++ " ...\n"
  bch <- readTptp nms tptp
  case parse (pcheck vb 0 bch True (And [])) prfTx of
    Just ((), rem) -> guard (T.null rem)
    _ -> et "check failure" 

  -- -- when vb $ pt "Reading TPTP and ESTP files...\n"
  -- (bch, prf) <- branchProof vb tptp estp
  -- -- when vb $ pt "Checking ESTP solution...\n"
  -- pt "Checking ESTP solution...\n"
  -- check vb 0 bch prf

mainArgs ("dev" : tstp : _) =
  if isPrefixOf "/home/sk/tstp/alt/" tstp
    then do
      probName <- cast $ stripPrefix "/home/sk/tstp/alt/" tstp
      ps $ "Alt solution : " <> tstp <> "\n"
      afs <- parseName tstp
      let nms = L.filter isExtraAF (L.map afName afs)
      ks <- mapM breakExtra nms
      pb $ ppList ppInt ks
      afs' <- mapM (redefine ks) afs
      let output = tlt $ ppInter "\n" $ L.map ((<> ".") . fmtAF) afs'
      TIO.writeFile ("./newalt/" ++ probName)  output
    else skip
mainArgs _ = et "Invalid main args"

main :: IO ()
main = getArgs >>= mainArgs

