module App where

import Types
import Basic
import Data.List as L
import Control.Monad as M (guard, foldM, foldM_, (>=>), mzero)
import Data.Map as HM ( Map, insert, lookup, empty, map, member, mapMaybe, toList, fromListWithKey, delete )
import Control.Applicative ( Alternative((<|>)) )
import Debug.Trace (trace)

nt = Nothing

hasFv :: Int -> Term -> Bool
hasFv k (Fv m) = k == m
hasFv k (Fun _ xs) = L.any (hasFv k) xs
hasFv _ _ = False

isPar :: Term -> Bool
isPar (Par _) = True
isPar _ = False

isFv :: Term -> Bool
isFv (Fv _) = True
isFv _ = False

-- appBnd :: Bnd -> Term -> Term
-- appBnd b (Fv k) =
--   case HM.lookup k b of
--     Just x -> x
--     Nothing -> Fv k
-- appBnd b (Fun f xs) = Fun f $ L.map (appBnd b) xs
-- appBnd _ x = x

substFv :: Int -> Term -> Term -> Term
substFv k x (Bv v) = Bv v
substFv k x (Par m) = Par m
substFv k x (Fv m) = if k == m then x else Fv m
substFv k x (Fun f xs) = Fun f $ L.map (substFv k x) xs

-- breakFvLookup :: Term -> Bnd -> Maybe Term
-- breakFvLookup (Fv k) b = HM.lookup k b
-- breakFvLookup _ _ = nt
-- 
-- uniFv :: UniMode -> Bnd -> Int -> Term -> Maybe Bnd
-- scheme  : uniFv um b k x
-- assumes : k is unbound in b
-- assumes : if x is a FV, it is also unbound in b
-- uniFv Exact b k (Fv m) = guard (k == m) >> return b
-- uniFv Exact _ _ _  = nt
-- uniFv Pars b k (Par m) =
--   let b' = HM.map (substFv k (Par m)) b in
--   return $ HM.insert k (Par m) b'
-- uniFv Pars _ _ _ = nt
-- uniFv ParFvs b k x =
--   let x' = appBnd b x in
--   let b' = HM.map (substFv k x') b in
--   do guard $ isPar x || isFv x
--      guard $ not $ hasFv k x'
--      return $ HM.insert k x' b'
-- uniFv Lax b k x =
--   let x' = appBnd b x in
--   let b' = HM.map (substFv k x') b in
--   do guard $ not $ hasFv k x'
--      return $ HM.insert k x' b'
-- 
-- uniTerm' :: UniMode -> Bnd -> (Term, Term) -> Maybe Bnd
-- uniTerm' um bnd (x, y) = trace ("Unifying " ++ show x ++ " with " ++ show y) (uniTerm um bnd (x, y))
uniTerm' um bnd (x, y) = uniTerm um bnd (x, y)

uniTerm :: UniMode -> Bnd -> (Term, Term) -> Maybe Bnd
uniTerm s b (Bv v, Bv w)   = guard (v == w) >> return b
uniTerm s b (Par k, Par m) = guard (k == m) >> return b
uniTerm s b (Fun f xs, Fun g ys) = do
  xys <- zipM xs ys
  guard $ f == g
  foldM (uniTerm' s) b xys
uniTerm s b (x, y) =
  if x == y
  then return b
  else
    case (breakFvLookup x b, breakFvLookup y b, x, y) of
       (Just x', _, _, _) -> uniTerm' s b (x', y)
       (_, Just y', _, _) -> uniTerm' s b (x, y')
       (_, _, Fv k, _) -> uniFv s b k y
       (_, _, _, Fv m) -> uniFv s b m x
       (_, _, _, _) -> nt

uniForm :: UniMode -> Bnd -> (Form, Form) -> Maybe Bnd
uniForm s b (Eq x0 x1, Eq y0 y1) = foldM (uniTerm' s) b [(x0, y0), (x1, y1)]
uniForm s b (Rel p xs, Rel q ys) = guard (p == q) >> zipM xs ys >>= foldM (uniTerm' s) b
uniForm s b (Not f, Not g) = uniForm s b (f, g)
uniForm s b (And fs, And gs) = zipM fs gs >>= foldM (uniForm s) b
uniForm s b (Or fs,  Or gs)  = zipM fs gs >>= foldM (uniForm s) b
uniForm s b (Imp f0 f1, Imp g0 g1) = foldM (uniForm s) b [(f0, g0), (f1, g1)]
uniForm s b (Iff f0 f1, Iff g0 g1) = foldM (uniForm s) b [(f0, g0), (f1, g1)]
uniForm s b (Fa vs f, Fa ws g) = guard (vs == ws) >> uniForm s b (f, g)
uniForm s b (Ex vs f, Ex ws g) = guard (vs == ws) >> uniForm s b (f, g)
uniForm _ _ _ = nt

listEqGoals :: Int -> [(Term, Term)] -> (Int, [EqGoal])
listEqGoals k [] = (k, [])
listEqGoals k ((x, y) : xys) =
  let (m, egs) = listEqGoals (k + 1) xys in
  (m, (x, y, k) : egs)

getGoals :: Int -> [Form] -> (Int, [Goal])
getGoals k [] = (k, [])
getGoals k (f : fs) =
  let (m, pfs) = getGoals (k + 1) fs in
  (m, (f, k) : pfs)

appNestAndR' :: ([Goal], Ctx) -> ([Goal], Ctx)
appNestAndR' ([], c) = ([], c)
appNestAndR' (gl : gls, c0) = 
  let (gls1, c1) = appNestAndR gl c0 in
  let (gls2, c2) = appNestAndR' (gls, c1) in
  (gls1 ++ gls2, c2)

appNestAndR :: Goal -> Ctx -> ([Goal], Ctx)
appNestAndR (f, k) c =
  case appAndR (f, k) c of
    Just (fs, k') -> appNestAndR' (fs, k')
    _ -> ([(f, k)], c)

appNestOrL' :: [Goal] -> Ctx -> ([Goal], Ctx)
appNestOrL' [] c = ([], c)
appNestOrL' (gl : gls) c0 = 
  let (gls1, c1) = appNestOrL gl c0 in
  let (gls2, c2) = appNestOrL' gls c1 in
  (gls1 ++ gls2, c2)

appNestOrL :: Goal -> Ctx -> ([Goal], Ctx)
appNestOrL (f, k) c =
  case appOrL (f, k) c of
    Just (gls, c') -> appNestOrL' gls c'
    _ -> ([(f, k)], c)

appNestAndL' :: ([Form], Int, Ctx) -> ([Form], Int, Ctx)
appNestAndL' ([], k, c) = ([], k, c)
appNestAndL' (g : gs, k0, c0) = 
  let (gs1, k1, c1) = appNestAndL (g, k0) c0 in
  let (gs2, k2, c2) = appNestAndL' (gs, k1, c1) in
  (gs1 ++ gs2, k2, c2)

appNestAndL :: Goal -> Ctx -> ([Form], Int, Ctx)
appNestAndL (g, k) c = appAndL (g, k) c ?> appNestAndL' $ ([g], k, c)

appNestOrR' :: ([Form], Int, Ctx) -> ([Form], Int, Ctx)
appNestOrR' ([], k, c) = ([], k, c)
appNestOrR' (g : gs, k0, c0) = 
  let (gs1, k1, c1) = appNestOrR (g, k0) c0 in
  let (gs2, k2, c2) = appNestOrR' (gs, k1, c1) in
  (gs1 ++ gs2, k2, c2)

appNestOrR :: Goal -> Ctx -> ([Form], Int, Ctx)
appNestOrR (g, k) c = appOrR (g, k) c ?> appNestOrR' $ ([g], k, c)

bindPrf :: Int -> Prf_ -> Prfs -> Maybe Prfs
bindPrf k p ps =
  if HM.member k ps
  then error ("Cannot bind proof = " ++ show k)
  -- else trace ("Binding = " ++ show k) return $ HM.insert k p ps
  else return $ HM.insert k p ps

appNotL :: Goal -> Ctx -> Maybe (Goal, Ctx)
appNotL (Not f, p) c = do
  let k = fresh c
  ps <- bindPrf p (NotL_ f k) (proofs c)
  let c' = c {fresh = k + 1, proofs = ps}
  return ((f, k), c')
appNotL (f, _) _ = nt

appNotR :: Goal -> Ctx -> Maybe (Goal, Ctx)
appNotR (Not f, p) c =
  let k = fresh c in
  do ps' <- bindPrf p (NotR_ f k) (proofs c)
     let c' = c {fresh = k + 1, proofs = ps'} in
       return ((f, k), c')
appNotR (f, k) _ = nt

appNotNotL :: Goal -> Ctx -> Maybe (Goal, Ctx)
appNotNotL gl c = appNotL gl c >>= uncurry appNotR

appNotNotR :: Goal -> Ctx -> Maybe (Goal, Ctx)
appNotNotR gl c = appNotR gl c >>= uncurry appNotL

appNotLR :: PrvGoal -> Ctx -> Maybe (PrvGoal, Ctx)
appNotLR (f, g, k) c = do
  ((f', k0), c0) <- appNotL (f, k) c
  ((g', k1), c1) <- appNotR (g, k0) c0
  return ((g', f', k1), c1)

appNots :: PrvGoal -> Ctx -> Maybe (PrvGoal, Ctx)
appNots (f, g, k) c =
  ( do ((f', k'), c') <- appNotNotL (f, k) c
       return ((f', g, k'), c') ) <|>
  ( do ((g', k'), c') <- appNotNotR (g, k) c
       return ((f, g', k'), c') ) <|>
  appNotLR (f, g, k) c

interEq :: EqGoal -> Ctx -> Maybe (EqGoal, EqGoal, Term, Ctx)
interEq (x, z, k) c0 = do 
  let m = fresh c0
  let y = Fv m
  (k0, k1, c1) <- appCut (Just $ m + 1) (x === y, k) c0
  (k2, k3, c2) <- appCut nt (y === z, k1) c1
  c3 <- appEqT Exact (x === y) (y === z) (x === z) k3 c2
  return ((x, y, k0), (y, z, k2), y, c3)

appFaL :: Maybe [Term] -> Goal -> Ctx -> Maybe (Goal, Ctx)
appFaL Nothing (Fa vs f, p) c =
  let (k, xs) = listFvs (fresh c) vs in
  do vxs <- zipM vs xs
     ps' <- bindPrf p (FaL_ vxs f k) (proofs c)
     let c' = c {fresh = k + 1, proofs = ps'}
     return ((substForm vxs f, k), c')
appFaL (Just xs) (Fa vs f, p) c = do
  let k = fresh c
  vxs <- zipM vs xs
  ps' <- bindPrf p (FaL_ vxs f k) (proofs c)
  let c' = c {fresh = k + 1, proofs = ps'}
  return ((substForm vxs f, k), c')
appFaL _ _ _ = nt


appCut :: Maybe Int -> Goal -> Ctx -> Maybe (Int, Int, Ctx)
appCut mi (f, k) c = do
  let m = mi ?> id $ fresh c 
  ps <- bindPrf k (Cut_ f m $ m + 1) (proofs c)
  return (m, m + 1, c {fresh = m + 2, proofs = ps})

appExL :: Goal -> Ctx -> Maybe ([Term], Goal, Ctx)
appExL (Ex vs f, k) c =
  let (m, xs) = listPars (fresh c) vs in
  do vxs <- zipM vs xs
     ps' <- bindPrf k (ExL_ vs (fresh c) f m) (proofs c)
     let c' = c {fresh = m + 1, proofs = ps'}
     return (xs, (substForm vxs f, m), c')
appExL (f, _) _ = nt

appExR :: Maybe [Term] -> Goal -> Ctx -> Maybe (Goal, Ctx)
appExR Nothing (Ex vs f, p) c =
  let (k, xs) = listFvs (fresh c) vs in
  do vxs <- zipM vs xs
     ps' <- bindPrf p (ExR_ vxs f k) (proofs c)
     let c' = c {fresh = k + 1, proofs = ps'}
     return ((substForm vxs f, k), c')
appExR (Just xs) (Ex vs f, p) c = do
  let k = fresh c
  vxs <- zipM vs xs
  ps' <- bindPrf p (ExR_ vxs f k) (proofs c)
  let c' = c {fresh = k + 1, proofs = ps'}
  return ((substForm vxs f, k), c')
appExR _ _ _ = nt

appAndR :: Goal -> Ctx -> Maybe ([Goal], Ctx)
appAndR (And fs, p) c =
  let (m, pfs) = getGoals (fresh c) fs in
  do ps <- bindPrf p (AndR_ pfs) (proofs c)
     let c' = c {fresh = m, proofs = ps} in
       return (pfs, c')
appAndR _ _ = nt

appBotL :: Goal -> Ctx -> Maybe Ctx
appBotL (Or [], k) c = do
  ps <- bindPrf k (OrL_ []) (proofs c)
  return $ c {proofs = ps}
appBotL _ _ = nt

appTopR :: Goal -> Ctx -> Maybe Ctx
appTopR (And [], k) c = do
  ps <- bindPrf k (AndR_ []) (proofs c)
  return $ c {proofs = ps}
appTopR _ _ = nt

appAndL :: Goal -> Ctx -> Maybe ([Form], Int, Ctx)
appAndL (And fs, k) c =
  let m = fresh c in
  do ps <- bindPrf k (AndL_ fs fs m) (proofs c)
     return (fs, m, c {fresh = m + 1, proofs = ps})
appAndL (f, _) _ = nt

appOrL :: Goal -> Ctx -> Maybe ([Goal], Ctx)
appOrL (Or fs, p) c =
  let (m, pfs) = getGoals (fresh c) fs in
  do ps <- bindPrf p (OrL_ pfs) (proofs c)
     let c' = c {fresh = m, proofs = ps} in
       return (pfs, c')
appOrL _ _ = nt

appOrR :: Goal -> Ctx -> Maybe ([Form], Int, Ctx)
appOrR (Or fs, k) c =
  let m = fresh c in
  do ps <- bindPrf k (OrR_ fs fs m) (proofs c)
     return (fs, m, c {fresh = m + 1, proofs = ps})
appOrR (f, _) _ = nt

appOrRL :: PrvGoal -> Ctx -> Maybe ([PrvGoal], Ctx)
appOrRL pg@(f, g, k) c = do 
  (gs, k', c') <- appOrR (g, k) c
  (fks, c'') <- appOrL (f, k') c'
  fkgs <- zipM fks gs 
  let pgs = L.map (\ ((f_, k_), g_) -> (f_, g_, k_)) fkgs 
  return (pgs, c'')

appAndLR :: PrvGoal -> Ctx -> Maybe ([PrvGoal], Ctx)
appAndLR (f, g, k) c = do 
  (fs, k', c') <- appAndL (f, k) c
  (gks, c'') <- appAndR (g, k') c'
  fgks <- zipM fs gks 
  let pgs = L.map (\ (f_, (g_, k_)) -> (f_, g_, k_)) fgks 
  return (pgs, c'')

appImpL :: Goal -> Ctx -> Maybe (Goal, Goal, Ctx)
appImpL (Imp f g, k) c =
  let m = fresh c in
  do ps <- bindPrf k (ImpL_ f g m $ m + 1) (proofs c)
     return ((f, m), (g, m + 1), c {fresh = m + 2, proofs = ps})
appImpL (f, _) _ = nt

appIffR :: Goal -> Ctx -> Maybe (Goal, Goal, Ctx)
appIffR (Iff f g, k) c =
  let m = fresh c in
  do ps <- bindPrf k (IffR_ f g m $ m + 1) (proofs c)
     return ((Imp f g, m), (Imp g f, m + 1), c {fresh = m + 2, proofs = ps})
appIffR _ _ = nt

appIffLR :: Goal -> Ctx -> Maybe (Goal, Ctx)
appIffLR (Iff f g, k) c =
  let m = fresh c in
  do ps <- bindPrf k (IffLR_ f g m) (proofs c)
     return ((Imp g f, m), c {fresh = m + 1, proofs = ps})
appIffLR _ _ = nt

appIffLO :: Goal -> Ctx -> Maybe (Goal, Ctx)
appIffLO (Iff f g, k) c =
  let m = fresh c in
  do ps <- bindPrf k (IffLO_ f g m) (proofs c)
     return ((Imp f g, m), c {fresh = m + 1, proofs = ps})
appIffLO _ _ = nt

appImpRA :: Goal -> Ctx -> Maybe (Goal, Ctx)
appImpRA (Imp f g, k) c =
  let m = fresh c in
  do ps <- bindPrf k (ImpRA_ f g m) (proofs c)
     return ((f, m), c {fresh = m + 1, proofs = ps})
appImpRA (f, _) _ = nt

appImpR :: Goal -> Ctx -> Maybe (PrvGoal, Ctx)
appImpR (fg, k) c = do 
  ((f, k'), c') <- appImpRA (fg, k) c
  ((g, k''), c'') <- appImpRC (fg, k') c'
  return ((f, g, k''), c'')

appImpTrans :: Form -> Form -> Form -> Int -> Ctx -> Maybe Ctx
appImpTrans (Imp f g) (Imp g' h) (Imp f' h') k0 c0 = do 
  ((_, k1), (_, m0), c1) <- appImpL (Imp f g, k0) c0
  ((_, k2), c2) <- appImpRA (Imp f' h', k1) c1
  c3 <- appAx Exact (f', f, k2) c2
  ((_, m1), (_, n0), c4) <- appImpL (Imp g' h, m0) c3
  c5 <- appAx Exact (g, g', m1) c4
  ((_, n1), c6) <- appImpRC (Imp f' h', n0) c5
  appAx Exact (h, h', n1) c6
appImpTrans _ _ _ _ _ = mzero

appIffSym :: UniMode -> Form -> Form -> Int -> Ctx -> Maybe Ctx
appIffSym um (Iff f g) (Iff g' f') k c0 = do 
  ((_, m), (_, n), c1) <- appIffR (Iff g' f', k) c0  -- k1 : |- g' ==> f' // m0 : |- f' ==> g'
  ((_, n'), c2) <- appIffLO (Iff f g, n) c1
  c3 <- appAx um (f ==> g, f' ==> g', n') c2
  ((_, m'), c4) <- appIffLR (Iff f g, m) c3
  appAx um (g ==> f, g' ==> f', m') c4
appIffSym _ _ _ _ _ = mzero

appIffTrans :: Form -> Form -> Form -> Int -> Ctx -> Maybe Ctx
appIffTrans (Iff f g) (Iff g' h) (Iff f' h') k0 c0 = do 
  ((_, k1), (_, m0), c1) <- appIffR (Iff f' h', k0) c0  -- k1 : |- f' ==> h' // m0 : |- h' ==> f'
  ((_, k2), c2) <- appIffLO (Iff f g, k1) c1 -- k2 : f ==> g |- f' ==> h'
  ((_, k3), c3) <- appIffLO (Iff g' h, k2) c2 -- k3 : f ==> g, g' ==> h |- f' ==> h'
  c4 <- appImpTrans (f ==> g) (g' ==> h) (f' ==> h') k3 c3
  ((_, m1), c5) <- appIffLR (Iff f g, m0) c4
  ((_, m2), c6) <- appIffLR (Iff g' h, m1) c5 -- m2 : g ==> f, h ==> g' |- h' ==> f'
  appImpTrans (h ==> g') (g ==> f) (h' ==> f') m2 c6
appIffTrans _ _ _ _ _ = mzero

appIffTrans2 :: Form -> Form -> Form -> Form -> Int -> Ctx -> Maybe Ctx
appIffTrans2 (Iff f g) (Iff g' h) (Iff h' i) (Iff f' i') k0 c0 = do 
  (m0, k1, c1) <- appCut Nothing (f <=> h, k0) c0
  c2 <- appIffTrans (f <=> g) (g' <=> h) (f <=> h) m0 c1
  appIffTrans (f <=> h) (h' <=> i) (f' <=> i') k1 c2
appIffTrans2 _ _ _ _ _ _ = mzero

appImpRAC :: Goal -> Ctx -> Maybe (PrvGoal, Ctx)
appImpRAC (Imp f g, k) c = do 
  ((_, k'), c') <- appImpRA (f ==> g, k) c
  ((_, k''), c'') <- appImpRC (f ==> g, k') c' 
  return ((f, g, k''), c'')
appImpRAC _ _ = nt

appContra :: PrvGoal -> Ctx -> Maybe Ctx
appContra (fg, ngnf, k) c = do
  ((f, m0), (g, n0), c0) <- appImpL (fg, k) c
  ((f', m1), c1) <- appImpRC (ngnf, m0) c0 >>= uncurry appNotR
  -- trace ("\nobtained : " ++ show f' ++ "\n") (return ())
  c2 <- appAx Exact (f', f, m1) c1
  ((g', n1), c3) <- appImpRA (ngnf, n0) c2 >>= uncurry appNotL
  appAx Exact (g, g', n1) c3

appImpRC :: Goal -> Ctx -> Maybe (Goal, Ctx)
appImpRC (Imp f g, k) c =
  let m = fresh c in
  do ps <- bindPrf k (ImpRC_ f g m) (proofs c)
     return ((g, m), c {fresh = m + 1, proofs = ps})
appImpRC (f, _) _ = nt

appFunC :: EqGoal -> Ctx -> Maybe ([EqGoal], Ctx)
appFunC (Fun f xs, Fun g ys, k) c = do
  let m = fresh c 
  guard (f == g)
  xys <- zipM xs ys
  let (n, egs) = listEqGoals m xys 
  ps <- bindPrf k (FunC_ f egs) (proofs c)
  return (egs, c {fresh = n,  proofs = ps})
appFunC _ _ = nt

appEqC :: PrvGoal -> Ctx -> Maybe (EqGoal, EqGoal, Ctx)
appEqC (Eq x0 x1, Eq y0 y1, k) c =
  let m = fresh c in
  let eg0 = (x0, y0, m) in
  let eg1 = (x1, y1, m + 1) in
  do ps <- bindPrf k (EqC_ eg0 eg1) (proofs c)
     return (eg0, eg1, c {fresh = m + 2, proofs = ps})
appEqC _ _ = nt

appRelC :: PrvGoal -> Ctx -> Maybe ([EqGoal], Ctx)
appRelC (Rel r xs, Rel s ys, k) c =
  let m = fresh c in
  do guard (r == s)
     xys <- zipM xs ys
     let (n, egs) = listEqGoals m xys in
       do ps <- bindPrf k (RelC_ r egs) (proofs c)
          return (egs, c {fresh = n,  proofs = ps})
appRelC _ _ = nt

appIffRL :: PrvGoal -> Ctx -> Maybe (PrvGoal, PrvGoal, Ctx)
appIffRL (f, g, k) c0 = do
  ((g0, m), (g1, n), c1) <- appIffR (g, k) c0
  ((f0, m'), c2) <- appIffLO (f, m) c1
  ((f1, n'), c3) <- appIffLR (f, n) c2
  return ((f0, g0, m'), (f1, g1, n'), c3)

appImpLR :: PrvGoal -> Ctx -> Maybe (PrvGoal, PrvGoal, Ctx)
appImpLR (f, g, k) c = do
  ((f0, m), (f1, n), c') <- appImpL (f, k) c
  ((g0, m'), c'') <- appImpRA (g, m) c'
  ((g1, n'), c''') <- appImpRC (g, n) c''
  return ((g0, f0, m'), (f1, g1, n'), c''')

appFaRL :: InstMode -> PrvGoal -> Ctx -> Maybe (PrvGoal, Ctx)
appFaRL Same (f, g, k) c = do
  (xs, (g', k'), c') <- appFaR (g, k) c
  ((f', k''), c'') <- appFaL (Just xs) (f, k') c'
  return ((f', g', k''), c'')
appFaRL Perm (f, g, k) c = do
  (_, (g', k'), c') <- appFaR (g, k) c
  ((f', k''), c'') <- appFaL nt (f, k') c'
  return ((f', g', k''), c'')


appExLR :: InstMode -> PrvGoal -> Ctx -> Maybe (PrvGoal, Ctx)
appExLR Same (f, g, k) c = do
  (xs, (f', k'), c') <- appExL (f, k) c
  ((g', k''), c'') <- appExR (Just xs) (g, k') c'
  return ((f', g', k''), c'')
appExLR Perm (f, g, k) c = do
  (_, (f', k'), c') <- appExL (f, k) c
  ((g', k''), c'') <- appExR nt (g, k') c'
  return ((f', g', k''), c'')

appFaR :: Goal -> Ctx -> Maybe ([Term], Goal, Ctx)
appFaR (Fa vs f, k) c =
  let (m, xs) = listPars (fresh c) vs in
  do vxs <- zipM vs xs
     ps' <- bindPrf k (FaR_ vs (fresh c) f m) (proofs c)
     let c' = c {fresh = m + 1, proofs = ps'} in
       return (xs, (substForm vxs f, m), c')
appFaR (f, k) _ = nt

appSorry :: Int -> Ctx -> Maybe Ctx
appSorry k c = do 
  ps <- bindPrf k Sorry_ (proofs c)
  return $ c {proofs = ps}

appAx :: UniMode -> PrvGoal -> Ctx -> Maybe Ctx
appAx s (f, g, k) c = do
  b <- uniForm s (binding c) (f, g)
  ps <- bindPrf k (Ax_ f) (proofs c)
  return $ c {binding = b, proofs = ps}

appEqR :: UniMode -> Goal -> Ctx -> Maybe Ctx
appEqR s (Eq x y, k) c = do
  b <- uniTerm s (binding c) (x, y)
  ps <- bindPrf k (EqR_ x) (proofs c)
  return $ c {binding = b, proofs = ps}
appEqR _ _ _ = nt

appEqS :: UniMode -> PrvGoal -> Ctx -> Maybe Ctx
appEqS um (Eq x y, Eq y' x', k) c = do
  b <- uniTerm um (binding c) (x, x')
  b' <- uniTerm um b (y, y')
  ps <- bindPrf k (EqS_ x y) (proofs c)
  return $ c {binding = b', proofs = ps}
appEqS _ _ _ = nt

appEqSL :: Goal -> Ctx -> Maybe (Goal, Ctx)
appEqSL (Eq x y, k) c = do
  (m, n, c') <- appCut nt (Eq y x, k) c
  c'' <- appEqS Exact (Eq x y, Eq y x, m) c'
  return ((Eq y x, n), c'')
appEqSL _ _ = nt

appEqT :: UniMode -> Form -> Form -> Form -> Int -> Ctx -> Maybe Ctx
appEqT um (Eq x0 y0) (Eq y1 z1) (Eq x2 z2) k c = do
  b0 <- uniTerm um (binding c) (x0, x2)
  b1 <- uniTerm um b0 (y0, y1)
  b2 <- uniTerm um b1 (z1, z2)
  ps <- bindPrf k (EqT_ x0 y1 z2) (proofs c)
  return $ c {binding = b2, proofs = ps}
appEqT _ _ _ _ _ _ = nt

appMP :: Form -> PrvGoal -> Ctx -> Maybe Ctx
appMP fg (f, g, k) c = do
  ((f', m), (g', n), c') <- appImpL (fg, k) c
  appAx Lax (f, f', m) c' >>= appAx Lax (g', g, n)

rwro :: Form -> Form -> Int -> Ctx -> Maybe (Form, Int, Ctx)
rwro (Iff f g) f' k c = do
  ((_, m), (_, n), c') <- appIffLR (Iff f g, k) c >>= uncurry appImpL
  c'' <- appAx Lax (f, f', n) c'
  return (g, m, c'')
rwro _ _ _ _ = nt

rwrr :: Form -> Form -> Int -> Ctx -> Maybe (Form, Int, Ctx)
rwrr (Iff f g) g' k c = do
  ((_, m), (_, n), c') <- appIffLO (Iff f g, k) c >>= uncurry appImpL
  c'' <- appAx Lax (g, g', n) c'
  return (f, m, c'')
rwrr _ _ _ _ = nt

rwlo :: Form -> Form -> Int -> Ctx -> Maybe (Form, Int, Ctx)
rwlo (Iff f g) f' k c = do
  ((_, m), (_, n), c') <- appIffLO (Iff f g, k) c >>= uncurry appImpL
  c'' <- appAx Lax (f', f, m) c'
  return (g, n, c'')
rwlo _ _ _ _ = nt

rwlr :: Form -> Form -> Int -> Ctx -> Maybe (Form, Int, Ctx)
rwlr (Iff f g) g' k c = do
  ((_, m), (_, n), c') <- appIffLR (Iff f g, k) c >>= uncurry appImpL
  c'' <- appAx Lax (g', g, m) c'
  return (f, n, c'')
rwlr _ _ _ _ = nt