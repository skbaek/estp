{-# LANGUAGE TupleSections #-}

module Lem where

import Types
import Basic
import Data.List
import Data.Text ( Text )

mt :: Form -> Form -> Prf -- f => g |- ~g => ~f
mt f g = ImpL f g (ImpRC (Not g) (Not f) $ NotR f $ Ax f) (ImpRA (Not g) (Not f) $ NotL g $ Ax g)

impRefl :: Form -> Prf
impRefl f = ImpRA f f $ ImpRC f f $ Ax f


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
-- degenFaIff k vs f : |- (! vs f) <=> f
degenFaIff :: Int -> [Text] -> Form -> Prf 
degenFaIff k vs f = 
  let vxs = map (, zt) vs in
  iffRFull (Fa vs f) f (FaL vxs f $ Ax f) (FaR vs k f $ Ax f)

-- requires : none of vs occurs in f
-- degenExIff k vs f : |- (? vs f) <=> f
degenExIff :: Int -> [Text] -> Form -> Prf 
degenExIff k vs f = 
  let vxs = map (, zt) vs in
  iffRFull (Ex vs f) f (ExL vs k f $ Ax f) (ExR vxs f $ Ax f)

degenHelper :: [(Text, Term)] -> Text -> (Text, Term)
degenHelper vxs w = 
  case find ((w ==) . fst) vxs of 
    Just (_, x) -> (w, x) 
    _ -> (w, zt) 

-- requires : ws is a subset of vs 
-- requires : none of (vs \ ws) occurs in f
-- degenFaIffFa k vs ws f : |- (! vs f) <=> (! ws f)
degenFaIffFa :: Int -> [Text] -> [Text] -> Form -> Prf 
degenFaIffFa k vs ws f = 
  let (_, vxs) = varPars k vs in
  let (_, wxs) = varPars k ws in
  let vxs' = map (degenHelper wxs) vs in
  let wxs' = map (degenHelper vxs) ws in
  let fv = substForm vxs f in
  let fw = substForm wxs f in
  iffRFull (Fa vs f) (Fa ws f) (FaR ws k f $ FaL vxs' f $ Ax fw) (FaR vs k f $ FaL wxs' f $ Ax fv)

-- requires : ws is a subset of vs 
-- requires : none of (vs \ ws) occurs in f
-- degenFaIffFa k vs ws f : |- (? vs f) <=> (? ws f)
degenExIffEx :: Int -> [Text] -> [Text] -> Form -> Prf 
degenExIffEx k vs ws f = 
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

eqCong :: (Term, Term, Prf) -> (Term, Term, Prf) -> Prf
eqCong tax@(a, x, _) tby@(b, y, _) = 
  congAux [tax, tby] $ iffRFull (a === b) (x === y) (EqC (a, x, Ax (a === x)) (b, y, Ax (b === y))) (EqC (x, a, Ax (x === a)) (y, b, Ax (y === b))) 

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