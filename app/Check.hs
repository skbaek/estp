{-# LANGUAGE OverloadedStrings #-}

module Check where

import Types
import Basic
import PP (ppEP, ppSignForm, writeEF)
import Parse (readEp)
import Data.Text.Lazy as T (Text)
import Data.Map as HM (lookup, Map)
import Data.List as L (all)
import Control.Monad as M ( guard, when )

subEPRec :: EP -> EP -> Bool
subEPRec (0, []) _ = False
subEPRec (0, (_, m) : l) ep = subEP (m, l) ep
subEPRec (k, l) ep = subEP (k - 1, l) ep

subEP :: EP -> EP -> Bool
subEP ep ep'
  | ep == ep' = True
  | otherwise = subEPRec ep ep'

onPath :: EP -> Text -> Bool
onPath ep nm =
  case readEp nm of
    Just ep' -> subEP ep ep'
    _ -> True

pathLookup :: EP -> Nodes -> Maybe (Form, Bool, Int)
pathLookup = HM.lookup . pathText 

pathText :: EP -> Text
pathText = tlt . ppEP

checkEF :: Nodes -> EF -> IO ()
checkEF bm (ep, _, _, k, Cut, _) = do
  let ep0 = epFork 0 ep
  let ep1 = epFork 1 ep
  (g0, True, k0) <-  cast $ pathLookup ep0 bm
  (g1, False, k1) <- cast $ pathLookup ep1 bm
  guardMsg "cut fail" $ g0 == g1 && k == k0 && k == k1

checkEF fm (ep, _, _, k, OrT nm, _) = do
  guard $ onPath ep nm
  pf <- cast $ HM.lookup nm fm
  case pf of
    (Or fs, True, _) -> guard $ checkJunct fm ep k bt 0 fs
    (f, b, _) -> eb $ "Not a positive disjunction : " <> ppSignForm (f, b)

checkEF fm (ep, _, _, k, AndF nm, _) = do
  guard $ onPath ep nm
  (And fs, bf, _) <- cast $ HM.lookup nm fm
  guard $ checkJunct fm ep k bf 0 fs

checkEF fm (ep, _, _, k, AndT nm, _) = do
  guard $ onPath ep nm
  (And fs, bt, _) <- cast $ HM.lookup nm fm
  (f, bt, k') <- cast $ pathLookup (epIncr ep) fm
  guard $ k == k' && f `elem` fs

checkEF fm (ep, _, _, k, RelD, _) = do
  (f, bt, k') <- cast $ pathLookup (epIncr ep) fm
  guard $ k == k'

checkEF fm (ep, _, _, k, AoC xs, _) = do
  (f, bt, k') <- cast $ pathLookup (epIncr ep) fm
  isAoC' xs f
  -- guard $ k <= m 
  -- (f, bt, k') <- cast $ pathLookup (epIncr ep) fm
  -- kos <- offsetAoC m f
  -- guard $ kos <= k' 
  -- isAoC m f


checkEF fm (ep, _, _, k, OrF nm, _) = do
  guard $ onPath ep nm
  (Or fs, bf, _) <- cast $ HM.lookup nm fm
  (f, bf, k') <- cast $ pathLookup (epIncr ep) fm
  guard $ k == k' && f `elem` fs

checkEF fm (ep, _, _, k, ImpFA nm, _) = do
  guard $ onPath ep nm
  (Imp f g, bf, _) <- cast $ HM.lookup nm fm
  guard $ checkDown fm ep 0 [(f, bt, k)]

checkEF fm (ep, _, _, k, ImpFC nm, _) = do
  guard $ onPath ep nm
  (Imp f g, bf, _) <- cast $ HM.lookup nm fm
  guard $ checkDown fm ep 0 [(g, bf, k)]

checkEF fm (ep, _, _, k, IffTO nm, _) = do
  guard $ onPath ep nm
  (Iff f g, bt, _) <- cast $ HM.lookup nm fm
  guard $ checkJunct fm ep k bt 0 [Imp f g]

checkEF fm (ep, _, _, k, IffTR nm, _) = do
  guard $ onPath ep nm
  (Iff f g, bt, _) <- cast $ HM.lookup nm fm
  guard $ checkJunct fm ep k bt 0 [Imp g f]

checkEF fm (ep, _, _, k, ImpT nm, _) = do
  guard $ onPath ep nm
  (Imp f g, bt, _) <- cast $ HM.lookup nm fm
  guard $ checkDown fm ep 0 [(f, bf, k), (g, bt, k)]

checkEF fm (ep, _, _, k, IffF nm, _) = do
  guard $ onPath ep nm
  (Iff f g, bf, _) <- cast $ HM.lookup nm fm
  guard $ checkDown fm ep 0 [(f ==> g, bf, k), (g ==> f, bf, k)]

checkEF fm (ep, _, _, k, NotF nm, _) = do
  guard $ onPath ep nm
  (Not f, bf, _) <- cast $ HM.lookup nm fm
  guard $ checkDown fm ep 0 [(f, bt, k)]

checkEF fm (ep, _, _, k, NotT nm, _) = do
  guard $ onPath ep nm
  (Not f, bt, _) <- cast $ HM.lookup nm fm
  guard $ checkDown fm ep 0 [(f, bf, k)]

checkEF fm (ep, _, _, k, EqR nm, _) = do
  guard $ onPath ep nm
  (Eq x y, bf, _) <- cast $ HM.lookup nm fm
  guard $ x == y

checkEF fm (ep, _, _, k, Id nm0 nm1, _) = do
  guard $ onPath ep nm0
  guard $ onPath ep nm1
  (f, bt, _) <- cast $ HM.lookup nm0 fm
  (g, bf, _) <- cast $ HM.lookup nm1 fm
  guard $ f == g

checkEF fm (ep, _, _, k, FaF nm m, _) = do
  guard $ onPath ep nm && k <= m
  (Fa vs f, bf, _) <- cast $ HM.lookup nm fm
  let (m', vxs) = varPars m vs
  let f' = substForm vxs f
  guard $ checkDown fm ep 0 [(f', bf, m')]

checkEF fm (ep, _, _, k, FaT nm xs, _) = do
  guard $ onPath ep nm
  (Fa vs f, bt, _) <- cast $ HM.lookup nm fm
  vxs <- zipM vs xs
  let f' = substForm vxs f
  guard $ checkDown fm ep 0 [(f', bt, k)]

checkEF fm (ep, _, _, k, ExT nm m, _) = do
  guard $ onPath ep nm && k <= m
  (Ex vs f, bt, _) <- cast $ HM.lookup nm fm
  let (m', vxs) = varPars m vs
  let f' = substForm vxs f
  guard $ checkDown fm ep 0 [(f', bt, m')]

checkEF fm (ep, _, _, k, ExF nm xs, _) = do
  guard $ onPath ep nm
  (Ex vs f, bf, _) <- cast $ HM.lookup nm fm
  vxs <- zipM vs xs
  let f' = substForm vxs f
  guard $ checkDown fm ep 0 [(f', bf, k)]

checkEF fm (ep, _, _, k, EqS nm0 nm1, _) = do
  guard $ L.all (onPath ep) [nm0, nm1]
  (Eq x y, bt, _) <- cast $ HM.lookup nm0 fm
  (Eq y' x', bf, _) <- cast $ HM.lookup nm1 fm
  guardMsg "Eq-S : mismatch" $ x == x' && y == y'

checkEF fm (ep, _, _, k, EqT nm0 nm1 nm2, _) = do
  guard $ L.all (onPath ep) [nm0, nm1, nm2]
  (Eq x y, bt, _) <- cast $ HM.lookup nm0 fm
  (Eq y' z, bt, _) <- cast $ HM.lookup nm1 fm
  (Eq x' z', bf, _) <- cast $ HM.lookup nm2 fm
  guardMsg "Eq-T : mismatch" $ x == x' && y == y' && x == x'

checkEF fm (ep, _, _, k, FunC nms nm, _) = do
  guard $ L.all (onPath ep) (nm : nms)
  (Eq (Fun f xs) (Fun g ys), bf, _) <- cast $ HM.lookup nm fm
  nmxys <- zipM xs ys >>= zipM nms
  guardMsg "Fun-C : mismatch" $ L.all (checkEqPrem fm) nmxys

checkEF fm (ep, _, _, k, RelC nms nm0 nm1, _) = do
  guard $ L.all (onPath ep) (nm0 : nm1 : nms)
  (Rel r xs, bt, _) <- cast $ HM.lookup nm0 fm
  (Rel s ys, bf, _) <- cast $ HM.lookup nm1 fm
  guard $ r == s
  nmxys <- zipM xs ys >>= zipM nms
  guardMsg "Rel-C : mismatch" $ L.all (checkEqPrem fm) nmxys
checkEF fm (ep, _, _, k, Open, _) = return ()

checkEqPrem :: Nodes -> (Text, (Term, Term)) -> Bool
checkEqPrem bm (nm, (x, y)) =
  case HM.lookup nm bm of
    Just (Eq x' y', bt, _) -> x == x' && y == y'
    _ -> False

checkDown :: Nodes -> EP -> Int -> [(Form, Bool, Int)] -> Bool
checkDown fm ep k [] = True
checkDown fm ep k ((f, pl, m) : l) = do
  case pathLookup (epFork k ep) fm of
    Just (f', pl', m') -> f == f' && pl == pl' && m == m' && checkDown fm ep (k + 1) l
    _ -> False

checkJunct :: Nodes -> EP -> Int -> Bool -> Int -> [Form] -> Bool
checkJunct fm ep k pl m [] = True
checkJunct fm ep k pl m (f : fs) =
  case HM.lookup (pathText $ epFork m ep) fm of
    Just (f', pl', k') -> f == f' && pl == pl' && k == k' && checkJunct fm ep k pl (m + 1) fs
    _ -> False

checkEF' :: Bool -> HM.Map Text (Form, Bool, Int) -> EF -> IO ()
checkEF' vb bm ef = do
  when vb $ pb $ "checking EF : " <> writeEF ef <> "\n"
  checkEF bm ef

-- check :: [String] -> IO ()
-- check (tptp : estp : flags) = do
--   let vb = "silent" `notElem` flags
--   (nds, efs) <- nodeEFs vb tptp estp
--   (And [], True, 0) <- cast $ pathLookup (0, []) nds
--   mapM_ (checkEF' vb nds) efs
-- check _ = et "invalid args for check"

check :: Bool -> Nodes -> [EF] -> IO ()
check vb nds efs = do
  (And [], True, 0) <- cast $ pathLookup (0, []) nds
  mapM_ (checkEF' vb nds) efs