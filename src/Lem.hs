{-# LANGUAGE TupleSections #-}

module Lem where

import Types
import Basic
import Data.List
import Data.Text ( Text )
import Data.Map as HM (Map, lookup)

mt :: Form -> Form -> Prf -- f => g |- ~g => ~f
mt f g = ImpL f g (ImpRC (Not g) (Not f) $ NotR f $ Ax f) (ImpRA (Not g) (Not f) $ NotL g $ Ax g)

impRefl :: Form -> Prf
impRefl f = ImpRA f f $ ImpRC f f $ Ax f

-- impToOrNot f g : (f ==> g) |- (g \/ ~f)
impToOrNot :: Form -> Form -> Prf
impToOrNot f g = OrR [g, Not f] [g, Not f] $ NotR f $ ImpL f g (Ax f) (Ax g)

-- orNotToImp f g : (g \/ ~f) |- (f ==> g) 
orNotToImp :: Form -> Form -> Prf
orNotToImp f g = impRAC f g $ OrL [(g, Ax g), (Not f, NotL f $ Ax f)]

-- impIffOrNot vs f f : |- (f ==> g) <=> (g \/ ~f)
impIffOrNot :: Form -> Form -> Prf
impIffOrNot f g = iffRFull (f ==> g) (Or [g, Not f]) (impToOrNot f g) (orNotToImp f g)

-- iffIffAnd f g : |- (f <=> g) <=> ((f \/ ~g) /\ (g \/ ~f))
iffIffAnd :: Form -> Form -> Prf
iffIffAnd f g = 
  iffRFull (f <=> g) (And [Or [f, Not g], Or [g, Not f]])
    (AndR [(Or [f, Not g], IffLR f g $ impToOrNot g f), (Or [g, Not f], IffLO f g $ impToOrNot f g)])
    (AndL [Or [f, Not g], Or [g, Not f]] [Or [f, Not g], Or [g, Not f]] $ IffR f g (orNotToImp f g) (orNotToImp g f))

-- notFaIffExNot vs f : |- ~(! vs f) <=> ? vs (~ f)
notFaIffExNot :: Int -> [Text] -> Form -> Prf
notFaIffExNot k vs f = 
  let (_, vxs) = varPars k vs in
  let f' = substForm vxs f in
  iffRFull (Not (Fa vs f)) (Ex vs (Not f)) 
    (NotL (Fa vs f) $ FaR vs k f $ ExR vxs (Not f) $ NotR f' $ Ax f') 
    (NotR (Fa vs f) $ ExL vs k (Not f) $ FaL vxs f $ NotL f' $ Ax f')

-- notFaIffExNot vs f : |- ~(? vs f) <=> ! vs (~ f)
notExIffFaNot :: Int -> [Text] -> Form -> Prf
notExIffFaNot k vs f = 
  let (_, vxs) = varPars k vs in
  let f' = substForm vxs f in
  iffRFull (Not (Ex vs f)) (Fa vs (Not f)) 
    (NotL (Ex vs f) $ FaR vs k (Not f) $ ExR vxs f $ NotR f' $ Ax f') 
    (NotR (Ex vs f) $ ExL vs k f $ FaL vxs (Not f) $ NotL f' $ Ax f')

-- notOrIffAndNots fs : |- ~(\/ fs) <=> /\ (~fs)
notOrIffAndNots :: [Form] -> Prf
notOrIffAndNots fs = 
  let nfs = map Not fs in
  let nfps = map (\ f_ -> (Not f_, NotR f_ (Ax f_))) fs in
  let fps = map (\ f_ -> (f_, NotL f_ (Ax f_))) fs in
  iffRFull (Not $ Or fs) (And nfs) 
    (NotL (Or fs) $ OrR fs fs $ AndR nfps) 
    (NotR (Or fs) $ AndL nfs nfs $ OrL fps)

-- notAndIffOrNots fs : |- ~(/\ fs) <=> \/ (~fs)
notAndIffOrNots :: [Form] -> Prf
notAndIffOrNots fs = 
  let nfs = map Not fs in
  let nfps = map (\ f_ -> (Not f_, NotL f_ (Ax f_))) fs in
  let fps = map (\ f_ -> (f_, NotR f_ (Ax f_))) fs in
  iffRFull (Not $ And fs) (Or nfs) 
    (NotL (And fs) $ OrR nfs nfs $ AndR fps) 
    (NotR (And fs) $ AndL fs fs $ OrL nfps)

-- notImpIffNotand f g : |- ~(f ==> g) <=> (~g /\ f)
notImpIffNotAnd :: Form -> Form -> Prf
notImpIffNotAnd f g = 
  iffRFull (Not (f ==> g)) (And [Not g, f]) 
    (NotL (f ==> g) $ AndR [(Not g, NotR g $ ImpRC f g $ Ax g), (f, ImpRA f g $ Ax f)]) 
    (AndL [Not g, f] [Not g, f] $ NotL g $ NotR (f ==> g) $ ImpL f g (Ax f) (Ax g))

mp :: Form -> Form -> Prf
mp f g = ImpL f g (Ax f) (Ax g)

iffMP :: Form -> Form -> Prf  -- iffMP f g :  f <=> g, f |- g
iffMP f g = IffLO f g $ mp f g

iffMPR :: Form -> Form -> Prf -- iffMPR f g : f <=> g, g |- f
iffMPR f g = IffLR f g $ mp g f

iffToNotIffNot :: Form -> Form -> Prf -- f <=> g |- ~f <=> ~ g
iffToNotIffNot f g = 
  let pl = IffLR f g (mt g f) in  
  let pr = IffLO f g (mt f g) in
  IffR (Not f) (Not g) pl pr 

impRAC :: Form -> Form -> Prf -> Prf
impRAC f g = ImpRA f g . ImpRC f g

-- iffsToOrIffOr : f0 <=> g0, ..., fn <=> gn |- (f0 \/ ... \/ fn) <=> (g0 \/ ... \/ gn)
iffsToOrIffOr :: [Form] -> [Form] -> Maybe Prf
iffsToOrIffOr fs gs = do
  fgs <- zipM fs gs
  let fps = map (\ (f_, g_) -> (f_, iffMP f_ g_)) fgs 
  let gps = map (\ (f_, g_) -> (g_, iffMPR f_ g_)) fgs 
  return $ iffRFull (Or fs) (Or gs) (OrR gs gs $ OrL fps) (OrR fs fs $ OrL gps)

-- iffsToOrIffOr : f0 <=> g0, ..., fn <=> gn |- (f0 \/ ... \/ fn) <=> (g0 \/ ... \/ gn)
iffsToOrIffOr' :: [(Form, Form)] -> [Form] -> [Form] -> Maybe Prf
iffsToOrIffOr' fgs fs gs = do
  fps <- mapM (\ f_ -> find ((f_ ==) . fst) fgs >>= \ (_, g_) -> return (f_, iffMP f_ g_)) fs 
  gps <- mapM (\ g_ -> find ((g_ ==) . snd) fgs >>= \ (f_, _) -> return (g_, iffMPR f_ g_)) gs 
  return $ iffRFull (Or fs) (Or gs) (OrR gs gs $ OrL fps) (OrR fs fs $ OrL gps)

-- iffsToAndIffAnd : f0 <=> g0, ..., fn <=> gn |- (f0 /\ ... /\ fn) <=> (g0 /\ ... /\ gn)
iffsToAndIffAnd :: [Form] -> [Form] -> Maybe Prf
iffsToAndIffAnd fs gs = do
  fgs <- zipM fs gs
  let fps = map (\ (f_, g_) -> (f_, iffMPR f_ g_)) fgs 
  let gps = map (\ (f_, g_) -> (g_, iffMP  f_ g_)) fgs 
  return $ iffRFull (And fs) (And gs) (AndL fs fs $ AndR gps) (AndL gs gs $ AndR fps) 

iffsToAndIffAnd' :: [(Form, Form)] -> [Form] -> [Form] -> Maybe Prf
iffsToAndIffAnd' fgs fs gs = do
  -- let fps = map (\ (f_, g_) -> (f_, iffMPR f_ g_)) fgs 
  fps <- mapM (\ f_ -> find ((f_ ==) . fst) fgs >>= \ (_, g_) -> return (f_, iffMPR f_ g_)) fs 
  gps <- mapM (\ g_ -> find ((g_ ==) . snd) fgs >>= \ (f_, _) -> return (g_, iffMP f_ g_)) gs 
  return $ iffRFull (And fs) (And gs) (AndL fs fs $ AndR gps) (AndL gs gs $ AndR fps) 

-- impTrans f g h : f ==> g, g ==> h |- f ==> h
impTrans ::  Form -> Form -> Form -> Prf
impTrans f g h = impRAC f h $ ImpL f g (Ax f) $ ImpL g h (Ax g) (Ax h)

-- impTrans3 e f g h : e ==> f, f ==> g, g ==> h |- e ==> h
impTrans3 :: Form -> Form -> Form -> Form -> Prf
impTrans3 e f g h = Cut (f ==> h) (impTrans f g h) (impTrans e f h)

-- iffRefl f : |- f <=> f
iffRefl :: Form -> Prf 
iffRefl f = IffR f f (impRefl f) (impRefl f)

-- iffSym f g : f <=> g |- g <=> f
iffSym :: Form -> Form -> Prf 
iffSym f g = IffR g f (IffLR f g $ Ax (g ==> f)) (IffLO f g $ Ax (f ==> g))

-- iffTrans f g h : f <=> g, g <=> h |- f <=> h
iffTrans ::  Form -> Form -> Form -> Prf
iffTrans f g h = 
  let po = IffLO f g $ IffLO g h $ impTrans f g h in
  let pr = IffLR f g $ IffLR g h $ impTrans h g f in
  IffR f h po pr 

-- e <=> f, e <=> g, f <=> h |- g <=> h
iffCongLem :: Form -> Form -> Form -> Form -> Prf
iffCongLem e f g h = Cut (g <=> e) (iffSym e g) $ Cut (e <=> h) (iffTrans e f h) $ iffTrans g e h

-- e <=> g, f <=> h |- (e <=> f) <=> (g <=> h)
iffCong :: Form -> Form -> Form -> Form -> Prf
iffCong e f g h = 
  IffR (e <=> f) (g <=> h) 
    (impRAC (e <=> f) (g <=> h) $ iffCongLem e f g h) 
    (impRAC (g <=> h) (e <=> f) $ Cut (g <=> e) (iffSym e g) $ Cut (h <=> f) (iffSym f h) $ iffCongLem g h e f)

-- e <=> g, f <=> h |- (e ==> f) <=> (g ==> h)
impCong :: Form -> Form -> Form -> Form -> Prf
impCong e f g h = 
  IffR (e ==> f) (g ==> h) 
  (impRAC (e ==> f) (g ==> h) $ Cut (g ==> e) (IffLR e g $ Ax (g ==> e)) $ Cut (f ==> h) (IffLO f h $ Ax (f ==> h)) $ impTrans3 g e f h) 
  (impRAC (g ==> h) (e ==> f) $ Cut (e ==> g) (IffLO e g $ Ax (e ==> g)) $ Cut (h ==> f) (IffLR f h $ Ax (h ==> f)) $ impTrans3 e g h f)

-- requires : none of vs occurs in f
-- bloatFaIff k vs f : |- (! vs f) <=> f
bloatFaIff :: Int -> [Text] -> Form -> Prf 
bloatFaIff k vs f = 
  let vxs = map (, zt) vs in
  iffRFull (Fa vs f) f (FaL vxs f $ Ax f) (FaR vs k f $ Ax f)

-- requires : none of vs occurs in f
-- bloatExIff k vs f : |- (? vs f) <=> f
bloatExIff :: Int -> [Text] -> Form -> Prf 
bloatExIff k vs f = 
  let vxs = map (, zt) vs in
  iffRFull (Ex vs f) f (ExL vs k f $ Ax f) (ExR vxs f $ Ax f)

degenHelper :: [(Text, Term)] -> Text -> (Text, Term)
degenHelper vxs w = 
  case find ((w ==) . fst) vxs of 
    Just (_, x) -> (w, x) 
    _ -> (w, zt) 

-- requires : ws is a subset of vs 
-- requires : none of (vs \ ws) occurs in f
-- bloatFaIffFa k vs ws f : |- (! vs f) <=> (! ws f)
bloatFaIffFa :: Int -> [Text] -> [Text] -> Form -> Prf 
bloatFaIffFa k vs ws f = 
  let (_, vxs) = varPars k vs in
  let (_, wxs) = varPars k ws in
  let vxs' = map (degenHelper wxs) vs in
  let wxs' = map (degenHelper vxs) ws in
  let fv = substForm vxs f in
  let fw = substForm wxs f in
  iffRFull (Fa vs f) (Fa ws f) (FaR ws k f $ FaL vxs' f $ Ax fw) (FaR vs k f $ FaL wxs' f $ Ax fv)

-- requires : ws is a subset of vs 
-- requires : none of (vs \ ws) occurs in f
-- bloatExIffEx k vs ws f : |- (? vs f) <=> (? ws f)
bloatExIffEx :: Int -> [Text] -> [Text] -> Form -> Prf 
bloatExIffEx k vs ws f = 
  let (_, vxs) = varPars k vs in
  let (_, wxs) = varPars k ws in
  let vxs' = map (degenHelper wxs) vs in
  let wxs' = map (degenHelper vxs) ws in
  let fv = substForm vxs f in
  let fw = substForm wxs f in
  iffRFull (Ex vs f) (Ex ws f) (ExL vs k f $ ExR wxs' f $ Ax fv) (ExL ws k f $ ExR vxs' f $ Ax fw)

-- p : f' |- g'
---------------
-- exImpEx vs k f g p |- (? vs f) ==> (? vs g)
exImpEx :: [Text] -> Int -> Form -> Form -> Prf -> Prf
exImpEx vs k f g p = 
  let (_, vxs) = varPars k vs in
  let f' = substForm vxs f in
  let g' = substForm vxs g in
  impRAC (Ex vs f) (Ex vs g) $ ExL vs k f $ ExR vxs g p

-- p : f' |- g'
---------------
-- faImpFa vs k f g p : |- (! vs f) ==> (! vs g)
faImpFa :: [Text] -> Int -> Form -> Form -> Prf -> Prf
faImpFa vs k f g p = 
  let (_, vxs) = varPars k vs in
  let f' = substForm vxs f in
  let g' = substForm vxs g in
  impRAC (Fa vs f) (Fa vs g) $ FaR vs k g $ FaL vxs f p

-- ! vs (f <=> g) |- (? ws f) <=> (? ws g)
faIffToExIffEx' :: Int -> [Text] -> Form -> [Text] -> Form -> IO Prf
faIffToExIffEx' k vs f ws g = do
  let (_, vxs) = varPars k vs 
  let (_, wxs) = varPars k ws 
  wxs' <- mapM (\ w_ -> cast (find ((w_ ==) . fst) vxs)) ws
  vxs' <- mapM (\ v_ -> cast (find ((v_ ==) . fst) wxs)) vs
  let f' = substForm vxs f 
  let g' = substForm wxs g 
  let f'' = substForm vxs' f 
  let g'' = substForm wxs' g 
  return $ iffRFull (Ex vs f) (Ex ws g) 
    (ExL vs k f $ ExR wxs' g $ FaL vxs (f <=> g) $ iffMP f' g'') 
    (ExL ws k g $ ExR vxs' f $ FaL vxs' (f <=> g) $ iffMPR f'' g')

-- ! ws (f[vs |=> ws] <=> g) |- (? vs f) <=> (? ws g)
faIffToExIffEx'' :: Int -> [Text] -> Form -> [Text] -> Form -> IO Prf
faIffToExIffEx'' k vs f ws g = do
  let (_, vxs) = varPars k vs 
  let (_, wxs) = varPars k ws 
  vws <- mapM2 (\ v_ w_ -> return (v_, Var w_)) vs ws
  let f' = substForm vxs f 
  let f'' = substForm vws f 
  let g' = substForm wxs g 
  return $ iffRFull (Ex vs f) (Ex ws g) 
    (ExL vs k f $ ExR wxs g $ FaL wxs (f'' <=> g) $ iffMP f' g') 
    (ExL ws k g $ ExR vxs f $ FaL wxs (f'' <=> g) $ iffMPR f' g')


-- ! ws (f[vs |=> ws] <=> g) |- (! vs f) <=> (! ws g)
faIffToFaIffFa'' :: Int -> [Text] -> Form -> [Text] -> Form -> IO Prf
faIffToFaIffFa'' k vs f ws g = do
  let (_, vxs) = varPars k vs 
  let (_, wxs) = varPars k ws 
  vws <- mapM2 (\ v_ w_ -> return (v_, Var w_)) vs ws
  let f' = substForm vxs f 
  let f'' = substForm vws f 
  let g' = substForm wxs g 
  return $ iffRFull (Fa vs f) (Fa ws g) 
    (FaR ws k g $ FaL vxs f $ FaL wxs (f'' <=> g) $ iffMP f' g') 
    (FaR vs k f $ FaL wxs g $ FaL wxs (f'' <=> g) $ iffMPR f' g')

-- ! vs (f <=> g) |- (! vs f) <=> (! ws g)
faIffToFaIffFa' :: Int -> [Text] -> Form -> [Text] -> Form -> IO Prf
faIffToFaIffFa' k vs f ws g = do
  let (_, vxs) = varPars k vs 
  let (_, wxs) = varPars k ws 
  wxs' <- mapM (\ w_ -> cast (find ((w_ ==) . fst) vxs)) ws
  vxs' <- mapM (\ v_ -> cast (find ((v_ ==) . fst) wxs)) vs
  let f' = substForm vxs f 
  let g' = substForm wxs g 
  let f'' = substForm vxs' f 
  let g'' = substForm wxs' g 
  return $ iffRFull (Fa vs f) (Fa ws g) 
    (FaR ws k g $ FaL vxs' f $ FaL vxs' (f <=> g) $ iffMP f'' g') 
    (FaR vs k f $ FaL wxs' g $ FaL vxs  (f <=> g) $ iffMPR f' g'')

findEqvInst :: HM.Map Text Text -> [(Text, Term)] -> Text -> Maybe (Text, Term)
findEqvInst vw wxs v = do 
  w <- HM.lookup v vw 
  (_, x) <- find ((w ==) . fst) wxs
  return (v, x)

-- ! ws (f[vw] <=> g) |- (! vs f) <=> (! ws g)
genFaIffToFaIffFa :: VR -> Int -> [Text] -> Form -> [Text] -> Form -> IO Prf
genFaIffToFaIffFa (vw, wv) k vs f ws g = do
  let (_, vxs) = varPars k vs 
  let (_, wxs) = varPars k ws 
  vxs' <- mapM (cast . findEqvInst vw wxs) vs
  wxs' <- mapM (cast . findEqvInst wv vxs) ws
  let f' = substForm vxs f 
  let g' = substForm wxs g 
  let f'' = substForm vxs' f 
  let g'' = substForm wxs' g 
  let fp = appVrForm (vw, wv) f
  return $ iffRFull (Fa vs f) (Fa ws g) 
    (FaR ws k g $ FaL vxs' f $ FaL wxs  (fp <=> g) $ iffMP  f'' g' ) 
    (FaR vs k f $ FaL wxs' g $ FaL wxs' (fp <=> g) $ iffMPR f'  g'')

-- ! ws (f[vw] <=> g) |- (? vs f) <=> (? ws g)
genFaIffToExIffEx :: VR -> Int -> [Text] -> Form -> [Text] -> Form -> IO Prf
genFaIffToExIffEx (vw, wv) k vs f ws g = do
  let (_, vxs) = varPars k vs 
  let (_, wxs) = varPars k ws 
  vxs' <- mapM (cast . findEqvInst vw wxs) vs
  wxs' <- mapM (cast . findEqvInst wv vxs) ws
  let f' = substForm vxs f 
  let g' = substForm wxs g 
  let f'' = substForm vxs' f 
  let g'' = substForm wxs' g 
  let fp = appVrForm (vw, wv) f
  return $ iffRFull (Ex vs f) (Ex ws g) 
    (ExL vs k f $ ExR wxs' g $ FaL wxs' (fp <=> g) $ iffMP  f'' g' ) 
    (ExL ws k g $ ExR vxs' f $ FaL wxs  (fp <=> g) $ iffMPR f'  g'')

-- faFaIff k vs ws f : |- ! vs (! ws f) <=> ! (vs ++ ws) f
faFaIff :: Int -> [Text] -> [Text] -> Form -> Prf
faFaIff k vs ws f = 
  let (k', vxs) = varPars k vs in
  let (_, wxs) = varPars k' ws in
  let f' = substForm vxs f in
  let f'' = substForm wxs f' in
  iffRFull (Fa vs (Fa ws f)) (Fa (vs ++ ws) f) 
    (FaR (vs ++ ws) k f $ FaL vxs (Fa ws f) $ FaL wxs f' $ Ax f'') 
    (FaR vs k (Fa ws f) $ FaR ws k' f' $ FaL (vxs ++ wxs) f $ Ax f'')

-- exExIff k vs ws f : |- ? vs (? ws f) <=> ? (vs ++ ws) f
exExIff :: Int -> [Text] -> [Text] -> Form -> Prf
exExIff k vs ws f = 
  let (k', vxs) = varPars k vs in
  let (_, wxs) = varPars k' ws in
  let f' = substForm vxs f in
  let f'' = substForm wxs f' in
  iffRFull (Ex vs (Ex ws f)) (Ex (vs ++ ws) f) 
    (ExL vs k (Ex ws f) $ ExL ws k' f' $ ExR (vxs ++ wxs) f $ Ax f'')
    (ExL (vs ++ ws) k f $ ExR vxs (Ex ws f) $ ExR wxs f' $ Ax f'') 
    
faIffToFaIffFa :: [Text] -> Int -> Form -> Form -> Prf
faIffToFaIffFa vs k f g = 
  let (_, vxs) = varPars k vs in
  let f' = substForm vxs f in
  let g' = substForm vxs g in
  IffR (Fa vs f) (Fa vs g) 
    (faImpFa vs k f g $ FaL vxs (f <=> g) $ iffMP f' g') 
    (faImpFa vs k g f $ FaL vxs (f <=> g) $ iffMPR f' g')

-- ! vs (f <=> g) |- (? vs f) <=> (? vs g)
faIffToExIffEx :: [Text] -> Int -> Form -> Form -> Prf
faIffToExIffEx vs k f g = 
  let (_, vxs) = varPars k vs in
  let f' = substForm vxs f in
  let g' = substForm vxs g in
  IffR (Ex vs f) (Ex vs g) 
    (exImpEx vs k f g $ FaL vxs (f <=> g) $ iffMP f' g') 
    (exImpEx vs k g f $ FaL vxs (f <=> g) $ iffMPR f' g')

congAux :: [(Term, Term, Prf)] -> Prf -> Prf
congAux [] pf = pf
congAux ((x, y, p) : xyps) pf = 
  Cut (x === y) p $ Cut (y === x) (EqS x y) $ congAux xyps pf

-- eqCong w x y z : w = x, x = y, y = z |- w = z
eqTrans2 :: Term -> Term -> Term -> Term -> Prf
eqTrans2 w x y z = Cut (x === z) (EqT x y z) (EqT w x z)

-- -- pwy : |- w = y
-- -- pxz : |- x = z 
-- -- eqCong w x y z pwy pxz : |- (w = x) <=> (y = z) 
-- eqCong' :: Term -> Term -> Term -> Term -> Prf -> Prf -> Prf
-- eqCong' w x y z pwy pxz = cuts [(w === y, pwy), (x === z, pxz)] $ eqCong w x y z 
-- 
-- -- eqCong w x y z pwy pxz : w = y, x = z |- (w = x) <=> (y = z) 
-- eqCong :: Term -> Term -> Term -> Term -> Prf
-- eqCong w x y z = 
--   iffRFull (w === x) (y === z) 
--     (Cut (y === w) (EqS w y) $ eqTrans2 y w x z) -- w = y, x = z, w = x |- y = z
--     (Cut (z === x) (EqS x z) $ eqTrans2 w y z x) -- w = y, x = z, y = z |- w = x

-- eqCong :: (Term, Term, Prf) -> (Term, Term, Prf) -> Prf
-- eqCong tax@(a, x, _) tby@(b, y, _) = 
--   congAux [tax, tby] $ iffRFull (a === b) (x === y) (EqC (a, x, Ax (a === x)) (b, y, Ax (b === y))) (EqC (x, a, Ax (x === a)) (y, b, Ax (y === b))) 

iffRFull :: Form -> Form -> Prf -> Prf -> Prf
iffRFull f g po pr = IffR f g (impRAC f g po) (impRAC g f pr)

relCong :: Text -> [(Term, Term, Prf)] -> Prf
relCong r xyps = 
  let (xs, ys, _) = unzip3 xyps in
  let xyps' = map (\ (x_, y_, _) -> (x_, y_, Ax (x_ === y_))) xyps in
  let yxps = map (\ (x_, y_, _) -> (y_, x_, Ax (y_ === x_))) xyps in
  let f = Rel r xs in
  let g = Rel r ys in
  congAux xyps $ iffRFull f g (RelC r xyps') (RelC r yxps)

notLR :: Form -> Form -> Prf -> Prf
notLR f g p = NotL f $ NotR g p

-- notNotIff f : |- ~~f <=> f
notNotIff :: Form -> Prf
notNotIff f =
  iffRFull (Not (Not f)) f
    (NotL (Not f) $ NotR f $ Ax f)
    (NotR (Not f) $ NotL f $ Ax f)

rDefLemma0 :: Form -> Form -> Prf
rDefLemma0 f g =
  let p = IffLO f g (mp f g) in -- f, f <=> g |- g
  OrR [g, Not f] [g, Not f] $ NotR f p -- f <=> g |- g \/ ~f

rDefLemma1 :: Form -> [Form] -> [Form] -> Prf
rDefLemma1 r fs fsnr =
  let pl = rDefLemma0 r (Or fs) in -- (r <=> \/ fs) |- (\/ fs) \/ ~r
  let ps = map (\ f_ -> (f_, Ax f_)) fs in
  let pfsnr = OrL [(Or fs, OrL ps), (Not r, Ax (Not r))] in -- pfsnr : (\/ fs) \/ ~r |- fs, ~r
  let pr = OrR fsnr fsnr pfsnr in                           -- ps    : (\/ fs) \/ ~r |- \/ fsnr
  Cut (Or [Or fs, Not r]) pl pr -- (r <=> \/ fs) |- \/ fsnr

-- notIffIffAnd f g : |- ~(f <=> g) <=> ((~g \/ ~f) /\ (g \/ f))
notIffIffAnd :: Form -> Form -> Prf
notIffIffAnd f g = 
  let rhs = [Or [Not g, Not f], Or [g, f]] in
  let _p1 = IffR f g (ImpRC f g $ Ax g) (ImpRC g f $ Ax f) in -- _p1 : f, g |- f <=> g
  let p1 = OrR [Not g, Not f] [Not g, Not f] $ NotR g $ NotR f _p1 in -- p1 : |- f <=> g, (~g \/ ~f)
  let p2 = OrR [g, f] [g, f] $ IffR f g (ImpRA f g $ Ax f) (ImpRA g f $ Ax g) in -- p2 : |- f <=> g, (g \/ f)
  let p3 = OrL [(g, Ax g), (f, iffMP f g)] in -- p3 : f <=> g, (g \/ f) |- g
  let p4 = OrL [(g, iffMPR f g), (f, Ax f)] in -- p4 : f <=> g, (g \/ f) |- f
  iffRFull (Not (Iff f g)) (And rhs)
    (NotL (f <=> g) $ AndR [(Or [Not g, Not f], p1), (Or [g, f], p2)])
    (NotR (f <=> g) $ AndL rhs rhs $ OrL [(Not g, NotL g p3), (Not f, NotL f p4)]) 

singleOrIff :: Form -> Prf
singleOrIff f = iffRFull (Or [f]) f (OrL [(f, Ax f)]) (OrR [f] [f] $ Ax f)

singleAndIff :: Form -> Prf
singleAndIff f = iffRFull (And [f]) f (AndL [f] [f] $ Ax f) (AndR [(f, Ax f)])

-- faTopIff : Fa vs top <=> top
faTopIff :: Int -> [Text] -> Prf
faTopIff k vs = 
  iffRFull (Fa vs top) top (AndR []) (FaR vs k top $ AndR [])

-- faBotIff : Fa vs top <=> top
faBotIff :: [Text] -> Prf
faBotIff vs = 
  let vxs = map (, zt) vs in
  iffRFull (Fa vs bot) bot (FaL vxs bot $ OrL []) (OrL [])

-- exBotIff : Ex vs bot <=> bot
exBotIff :: Int -> [Text] -> Prf
exBotIff k vs =
  iffRFull (Ex vs bot) bot (ExL vs k bot $ OrL []) (OrL [])

-- exTopIff : Ex vs top <=> top
exTopIff :: [Text] -> Prf
exTopIff vs = 
  let vxs = map (, zt) vs in
  iffRFull (Ex vs top) top (AndR []) (ExR vxs top $ AndR [])

-- degenOrIff fs : \/ fs <=> top
-- requires : top <- fs
degenOrIff :: [Form] -> Prf
degenOrIff fs = iffRFull (Or fs) top (AndR []) (OrR fs [top] $ AndR [])

-- degenAndIff fs : /\ fs <=> bot
-- requires : bot <- fs
degenAndIff :: [Form] -> Prf
degenAndIff fs = iffRFull (And fs) bot (AndL fs [bot] $ OrL []) (OrL [])

-- bloatOrIff fs : \/ fs <=> \/ (fs \ {bot})
bloatOrIff :: [Form] -> Prf
bloatOrIff fs = 
  let gs = filter (not . isBot) fs in 
  let fps = 
        map 
          ( \ f_ -> 
             case f_ of 
               Or [] -> (f_, OrL [])
               _ -> (f_, Ax f_ ) )
          fs in
  let gps = map (\ g_ -> (g_, Ax g_)) gs in
  iffRFull (Or fs) (Or gs) (OrR gs gs $ OrL fps) (OrR fs fs $ OrL gps)

-- bloatAndIff fs : /\ fs <=> /\ (fs \ {top})
bloatAndIff :: [Form] -> Prf
bloatAndIff fs = 
  let gs = filter (not . isTop) fs in 
  let fps = 
        map 
          ( \ f_ -> 
             case f_ of 
               And [] -> (f_, AndR [])
               _ -> (f_, Ax f_ ) )
          fs in
  let gps = map (\ g_ -> (g_, Ax g_)) gs in
  iffRFull (And fs) (And gs) (AndL fs fs $ AndR gps) (AndL gs gs $ AndR fps)