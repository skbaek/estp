{-# LANGUAGE OverloadedStrings #-}

module Expand where

import Types 
import Basic
import Data.List as L (concat)
import Data.Text.Lazy as T (Text)
import PP 
import Data.Map as HM (insert, lookup, foldrWithKey, empty)

{-
addExp :: Invranch -> Form -> Bool -> EP -> Int -> Prf -> IO [Elab]
addExp br f pl ep k p = do
  let br' = HM.insert (f, pl) (tlt $ ppEP ep) br
  -- pt "\nWorking on proof :\n"
  -- pt $ T.intercalate "\n" $ ppPrf 100 p
  -- pt "\nContext:\n"
  -- pt $ ppListNl ppSignForm $ L.map fst (HM.toList br')
  -- pt "\n\n"
  expp br' f pl ep k p

expp :: Invranch -> Form -> Bool -> EP -> Int -> Prf -> IO [Elab]
expp br f sd ep k (Cut'  g p0 p1) = do
  let ep0 = epFork 0 ep
  let ep1 = epFork 1 ep
  efs0 <- addExp br g bt ep0 k p1
  efs1 <- addExp br g bf ep1 k p0
  return $ (ep, sd, f, k, Cut, nt) : efs0 ++ efs1
expp br f sd ep k (IffTO' g h p) = do
  epgh <- cast $ HM.lookup (g <=> h, bt) br
  efs <- addExp br (g ==> h) bt (epIncr ep) k p
  return $ (ep, sd, f, k, IffTO epgh, nt) : efs
expp br f sd ep k (IffTR' g h p) = do
  epgh <- cast $ HM.lookup (g <=> h, bt) br
  efs <- addExp br (h ==> g) bt (epIncr ep) k p
  return $ (ep, sd, f, k, IffTR epgh, nt) : efs
expp br f sd ep k (IffF' g h p0 p1) = do
  epgh <- cast $ HM.lookup (g <=> h, bf) br
  efs0 <- addExp br (g ==> h) bf (epFork 0 ep) k p0
  efs1 <- addExp br (h ==> g) bf (epFork 1 ep) k p1
  return $ (ep, sd, f, k, IffF epgh, nt) : efs0 ++ efs1

expp br f sd ep k (ImpT' g h p0 p1) = do
  epgh <- cast $ HM.lookup (g ==> h, bt) br
  efs0 <- addExp br g bf (epFork 0 ep) k p0
  efs1 <- addExp br h bt (epFork 1 ep) k p1
  return $ (ep, sd, f, k, ImpT epgh, nt) : efs0 ++ efs1

expp br f sd ep k (NotT' g p) = do
  epg <- cast $ HM.lookup (Not g, bt) br
  efs <- addExp br g bf (epIncr ep) k p
  return $ (ep, sd, f, k, NotT epg, nt) : efs

expp br f sd ep k (NotF' g p) = do
  epg <- cast $ HM.lookup (Not g, bf) br
  efs <- addExp br g bt (epIncr ep) k p
  return $ (ep, sd, f, k, NotF epg, nt) : efs

expp br f sd ep k (ImpFA' g h p) = do
  epgh <- cast $ HM.lookup (g ==> h, bf) br
  efs <- addExp br g bt (epIncr ep) k p
  return $ (ep, sd, f, k, ImpFA epgh, nt) : efs

expp br f sd ep k (ImpFC' g h p) = do
  epgh <- cast $ HM.lookup (g ==> h, bf) br
  efs <- addExp br h bf (epIncr ep) k p
  return $ (ep, sd, f, k, ImpFC epgh, nt) : efs

expp br f sd ep k (ExT' vs m g p) = do
  epg <- cast $ HM.lookup (Ex vs g, bt) br
  let (k', vxs) = varPars k vs
  let g' = substForm vxs g
  efs <- addExp br g' bt (epIncr ep) k' p
  return $ (ep, sd, f, k, ExT epg k, nt) : efs

expp br f sd ep k (ExF' vxs g p) = do
  let (vs, xs) = unzip vxs
  epg <- cast $ HM.lookup (Ex vs g, bf) br
  let g' = substForm vxs g
  efs <- addExp br g' bf (epIncr ep) k p
  return $ (ep, sd, f, k, ExF epg xs, nt) : efs

expp br f sd ep k (FaF' vs m g p) = do
  epg <- cast $ HM.lookup (Fa vs g, bf) br
  let (k', vxs) = varPars k vs
  let g' = substForm vxs g
  efs <- addExp br g' bf (epIncr ep) k' p
  return $ (ep, sd, f, k, FaF epg k, nt) : efs

expp br f sd ep k (FaT' vxs g p) = do
  let (vs, xs) = unzip vxs
  epg <- cast $ HM.lookup (Fa vs g, bt) br
  let g' = substForm vxs g
  efs <- addExp br g' bt (epIncr ep) k p
  return $ (ep, sd, f, k, FaT epg xs, nt) : efs

expp br f pl ep k (OrF' gs gs' p) = do
  epg <- cast $ HM.lookup (Or gs, bf) br
  expOr br k epg f pl ep gs' p

expp br f pl ep k (AndT' gs gs' p) = do
  epg <- cast $ HM.lookup (And gs, bt) br
  expAnd br k epg f pl ep gs' p

expp br f pl ep k (OrT' fps) = do
  let (fs, ps) = unzip fps
  epg <- cast $ HM.lookup (Or fs, bt) br
  (_, efss) <- mapAccumM (\ m_ (f_, p_) -> (m_ + 1,) <$> addExp br f_ bt (epFork m_ ep) k p_) 0 fps
  let efs = L.concat efss
  return $ (ep, pl, f, k, OrT epg, nt) : efs

expp br f pl ep k (AndF' fps) = do
  let (fs, ps) = unzip fps
  epg <- cast $ HM.lookup (And fs, bf) br
  (_, efss) <- mapAccumM (\ m_ (f_, p_) -> (m_ + 1,) <$> addExp br f_ bf (epFork m_ ep) k p_) 0 fps
  let efs = L.concat efss
  return $ (ep, pl, f, k, AndF epg, nt) : efs

expp br f sd ep k (Id' g) = do
  epgl <- cast $ HM.lookup (g, bt) br
  epgr <- cast $ HM.lookup (g, bf) br
  return [(ep, sd, f, k, Id epgl epgr, nt)]

expp br f sd ep k (EqR' x) = do
  epg <- cast $ HM.lookup (Eq x x, bf) br
  return [(ep, sd, f, k, EqR epg, nt)]

expp br f sd ep k (EqS' x y) = do
  epf <- cast $ HM.lookup (Eq x y, bt) br
  epg <- cast $ HM.lookup (Eq y x, bf) br
  return [(ep, sd, f, k, EqS epf epg, nt)]

expp br f sd ep k (EqT' x y z) = do
  epf <- cast $ HM.lookup (Eq x y, bt) br
  epg <- cast $ HM.lookup (Eq y z, bt) br
  eph <- cast $ HM.lookup (Eq x z, bf) br
  return [(ep, sd, f, k, EqT epf epg eph, nt)]

expp br f sd ep k (FunC' g xs ys) = do
  xys <- zipM xs ys
  eps <- cast $ mapM (\ (x_, y_) -> HM.lookup (x_ === y_, bt) br) xys
  epg <- cast $ HM.lookup (Fun g xs === Fun g ys, bf) br
  return [(ep, sd, f, k, FunC eps epg, nt)]

expp br f sd ep k (RelC' r xs ys) = do
  xys <- zipM xs ys
  eps <- cast $ mapM (\ (x_, y_) -> HM.lookup (x_ === y_, bt) br) xys
  epf <- cast $ HM.lookup (Rel r xs, bt) br
  epg <- cast $ HM.lookup (Rel r ys, bf) br
  return [(ep, sd, f, k, RelC eps epf epg, nt)]

expp br f sd ep k Open' = return [(ep, sd, f, k, Open, nt)]

expp _ f b ep k p = eb $ ppInter "\n" $ "expansion not implemented" : ppPrf 10 p

expOr :: Invranch -> Int -> Text ->  Form -> Bool -> EP -> [Form] -> Prf -> IO [Elab]
expOr br k epg f pl ep [] p = expp br f pl ep k p
expOr br k epg f pl ep (g : gs) p = do
  let ep' = epIncr ep
  let br' = HM.insert (g, bf) (tlt $ ppEP ep') br
  efs <- expOr br' k epg g bf ep' gs p
  return $ (ep, pl, f, k, OrF epg, nt) : efs

expAnd :: Invranch -> Int -> Text -> Form -> Bool -> EP -> [Form] -> Prf -> IO [Elab]
expAnd br k epg f pl ep [] p = expp br f pl ep k p
expAnd br k epg f pl ep (g : gs) p = do
  let ep' = epIncr ep
  let br' = HM.insert (g, bt) (tlt $ ppEP ep') br
  efs <- expAnd br' k epg g bt ep' gs p
  return $ (ep, pl, f, k, AndT epg, nt) : efs


expand' :: Invranch -> Form -> EP -> [Stelab] -> IO [Elab]
expand' br f ep [] = expand br f ep []
expand' br f ep (el : els) = do
  -- ptnl $ "Expanding : " <> elabNote el
  expand br f ep (el : els)

expand :: Invranch -> Form -> EP -> [Stelab] -> IO [Elab]
expand _ (Or []) ep [] = return [(ep, bt, Or [], 0, OrT (tlt $ ppEP ep), Just "'EOP'")]
expand _ f ep [] = et "last added formula must be bot\n"
expand br f ep (InfStep g p tx : els) = do
  let br' = HM.insert (f, bt) (tlt $ppEP ep) br
  efs0 <- expand' br' g (epFork 0 ep) els
  efs1 <- addExp br' g bf (epFork 1 ep) 0 p
  return $ (ep, bt, f, 0, Cut, Just tx) : efs0 ++ efs1
expand br f ep (RelD' g h p tx : els) = do
  let br' = HM.insert (f, bt) (tlt $ ppEP ep) br
  let ep' = epIncr ep
  let br'' = HM.insert (g, bt) (tlt $ ppEP ep') br'
  let ep'' = epIncr ep'
  efs0 <- addExp br'' h bf (epFork 1 ep') 0 p
  efs1 <- expand' br'' h ep'' els
  return $ (ep, bt, f, 0, RelD, Just tx) : (ep', bt, g, 0, Cut, Just "'rel-def-cut'") : efs0 ++ efs1
expand br f ep (AoC' xs g h p tx : els) = do
  let br' = HM.insert (f, bt) (tlt $ppEP ep) br
  let ep' = epIncr ep
  let br'' = HM.insert (g, bt) (tlt $ ppEP ep') br'
  let ep'' = epIncr ep'
  efs0 <- expand' br'' h ep'' els
  efs1 <- addExp br'' h bf (epFork 1 ep') 0 p
  return $ (ep, bt, f, 0, AoC xs, Just tx) : (ep', bt, g, 0, Cut, Just "'aoc-cut'") : efs0 ++ efs1

-}

stelabsToElabs :: NTF -> [Stelab] -> IO [Elab]
stelabsToElabs hs slbs = do
  -- let hbr = HM.foldrWithKey (\ nm_ f_ br_ -> HM.insert (f_, bt) nm_ br_) HM.empty hs 
  -- expand' hbr (And []) (0, []) slbs
  error "todo"
  
