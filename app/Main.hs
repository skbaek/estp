{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Types
import Basic
import PP
import Parse ( parseName, parsePreName, decimal )
import Sat ( sat )
import Lem
import Norm
import Prove

import Control.Monad as M ( guard, MonadPlus(mzero), foldM_, when )
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative ( Alternative((<|>)) )
import System.Environment ( getArgs )
import Data.List as L ( map, foldl, all, sortBy, concat, reverse, length, any, filter, delete )
import Data.Text.Lazy as T ( Text, unpack, intercalate, pack, null, splitOn, unsnoc )
import Data.Text.Lazy.Builder (Builder)
import Data.Set as S ( empty, insert, member, singleton, toList )
import Data.Map as HM ( Map, empty, insert, lookup, toList, foldrWithKey )
import Data.Text.Lazy.IO as TIO ( hPutStrLn, hPutStr, writeFile )
import Data.Bifunctor as DBF (first, second, bimap)
import System.IO as SIO ( openFile, hClose, IOMode(WriteMode) )

putAF :: AF -> IO ()
putAF af = pb $ fmtAF af <> "\n"

addHyp :: Hyps -> AF -> Hyps
addHyp (nsq, sq) (n, _, f, _) = (HM.insert n f nsq, S.insert f sq)

sortAfs :: [AF] -> [AF]
sortAfs = sortBy compareAfs

compareAfs :: AF -> AF -> Ordering
compareAfs (m :> ms, _, _, _) (n :> ns, _, _, _) =
  case compare m n of
    EQ -> 
      case (readInt ms, readInt ns) of 
        (Just i, Just j) -> compare i j
        _ -> et "Cannot compare step names"
    other -> other
compareAfs _ _ = LT

addIf :: Bool -> NSeq-> Text -> Form -> [NSeq]
addIf True c n f = return $ HM.insert n f c
addIf False _ n _ = error $ "Cannot add formula : " ++ unpack n

getHyp :: Text -> NSeq -> IO Form
getHyp n c =
  case HM.lookup n c of
    Just f -> return f
    _ -> MF.fail $ "Hypothesis does not exist : " ++ show n

isRelD' :: Text -> Form -> Bool
isRelD' r (Fa vs (Iff (Rel s xs) f)) = r == s && L.map Var vs == xs && isGndForm vs f
isRelD' r (Iff (Rel s []) f) = isGndForm [] f
isRelD' _ _ = False

checkElab :: Seq -> Form -> Elab -> IO ()
checkElab sq g (Plab g' p _) = do
  guard (g == g')
  verify 0 sq (S.singleton g) p
checkElab sq g (RelD' r f g' p _) = do
  guard (g == g')
  guard $ isRelD' r f
  verify 0 (S.singleton f) (S.singleton g) p
checkElab sq g (AoC' xs f g' p _) = do 
  guard $ g == g'
  isAoC' xs f 
  verify 0 (S.singleton f) (S.singleton g) p

isSkolemTerm :: [Text] -> Term -> Bool
isSkolemTerm vs (Fun _ xs) =
  case mapM breakVar xs of
    Just ws -> isPerm vs ws -- sublist vs ws && sublist ws vs
    _ -> False
isSkolemTerm _ _ = False

-- isAoC :: Int -> Form -> IO ()
-- isAoC k (Fa vs (Imp (Ex ws f) g)) = do

isAoC' :: [Term] -> Form -> IO ()
isAoC' xs (Fa vs (Imp (Ex ws f) g)) = do
  guard $ L.all (isSkolemTerm vs) xs
  wxs <- zipM ws xs
  guard $ substForm wxs f == g
isAoC' xs (Imp (Ex ws f) g) = do
  guard $ L.all isConstant xs
  wxs <- zipM ws xs
  guard $ substForm wxs f == g
isAoC' _ _ = mzero

elabIO :: Bool -> Hyps -> AF -> IO (Hyps, Elab) -- todo : eliminate checking during elab-IO
elabIO vb (nsq, sq) af@(n, _, f, a) = do
  when vb $ print $ "Elaborating step = " <> n
  e <- elab nsq af
  checkElab sq f e <|> et ("precheck fail : " <> n)
  let e' = resj e
  checkElab sq f e' <|> et "postcheck fail"
  return ((HM.insert n f nsq, S.insert f sq), e')

gFunFunctor :: Gterm -> Maybe Text
gFunFunctor (Gfun t []) = return t
gFunFunctor _ = Nothing


infer :: Text -> [Form] -> Form -> IO Prf
infer "superposition" [f, g] h         = superpose f g h
infer "forward_demodulation" [f, g] h  = superpose f g h
infer "backward_demodulation" [f, g] h = superpose f g h
infer "negated_conjecture" [f] g = guard (f == g) >> return (Id' f)
infer "factoring" [f] g = efactor (Just True) f g
infer "nnf_transformation" [f] g = nnfTrans False f g
infer "ennf_transformation" [f] g = nnfTrans True f g
infer "true_and_false_elimination" [f] g = trueFalseElim f g
infer "duplicate_literal_removal" [f] g = efactor (Just True) f g
infer "equality_resolution" [f] g = efactor (Just False) f g
infer "trivial_inequality_removal" [f] g = efactor nt f g
infer "subsumption_resolution" [f, g] h = resolve f g h
infer "resolution" [f, g] h = resolve f g h
infer "definition_folding" (f : fs) g = definFold fs f g
infer "definition_unfolding" (f : fs) g = dunfold fs f g
infer "pure_predicate_removal" [f] g = removePure f g
infer "flattening" [f] g = flattening f g
infer "equality_factoring" [f] g = eqfactor f g
infer "rectify" [f] g            = rectify f g
infer "avatar_component_clause" [f] g = avatarComp f g
infer "cnf_transformation" [f] g = cnfTrans f g
infer "avatar_split_clause" (f : fs) g    = avatarSplit fs f g
infer "unused_predicate_definition_removal" [f] g = updr 0 f g
infer "avatar_contradiction_clause" [f] g = efactor (Just True) f g
infer "skolemisation" (f : fs) g = skolemize fs 0 f g
infer r fs g = et $ "No inference : " <> r

elab :: NSeq -> AF -> IO Elab
elab s (n, _, h, Just (Gfun "file" [_, Gfun m []], _)) = do
  f <- getHyp m s
  p <- orig f h
  return $ Plab h p n
elab _ (n, _, g, Just (Gfun "introduced" [Gfun "predicate_definition_introduction" [],Glist [Gfun "new_symbols" [Gfun "naming" [],Glist [Gfun r []]]]], _)) = relDef n r g
elab _ (n, _, g, Just (Gfun "introduced" [Gfun "avatar_definition" [], Glist [Gfun "new_symbols" [Gfun "naming" [], Glist [Gfun r []]]]], _)) = relDef n r g
elab s (n, _, g, Just (Gfun "introduced" [Gfun "choice_axiom" [], Glist []], _)) = do
  (xs, f) <- normalizeAoC g
  p <- orig f g
  return $ AoC' xs f g p n
elab s (n, _, g, Just (Gfun "inference" [Gfun "avatar_sat_refutation" [], _, Glist l], _)) = do
  fs <- cast (mapM gFunFunctor l) >>= mapM (`lookupM` s)
  p <- sat fs
  return $ Plab g p n
elab s (n, _, g, Just (Gfun "inference" [Gfun r [], _, Glist l], _)) = do
  fs <- cast (mapM gFunFunctor l) >>= mapM (`lookupM` s)
  p <- infer r fs g
  return $ Plab g p n
elab _ (_, _, _, a) = error $ "No elaborator for inference : " ++ show a


type Step = (Text, Text, [Text], Form) -- (name, inference, hyps, conc)

afToStep :: AF -> IO Step
afToStep (n, _, h, Just (Gfun "file" [_, Gfun m []], _)) = return (n, "file", [m], h)

{- Verification -}

verifyEqGoal :: Int -> Seq -> Seq -> (Term, Term, Prf) -> IO ()
verifyEqGoal k lft rgt (x, y, p) = verify k lft (S.insert (Eq x y) rgt) p

verifyRgtGoal :: Int -> Seq -> Seq -> (Form, Prf) -> IO ()
verifyRgtGoal k lft rgt (f, p) = verify k lft (S.insert f rgt) p

verifyLftGoal :: Int -> Seq -> Seq -> (Form, Prf) -> IO ()
verifyLftGoal k lft rgt (f, p) = verify k (S.insert f lft) rgt p

ev :: Text -> Form -> Seq -> Seq -> IO ()
ev t f lhs rhs = eb $ ft t <> ppForm f <> "\nLHS :\n" <> ppListNl ppForm (S.toList lhs) <> "\nRHS :\n" <> ppListNl ppForm (S.toList rhs) <> "\n"

verify :: Int -> Seq -> Seq -> Prf -> IO ()
verify _ _ _ Asm = return ()
verify _ lft rgt (Id' f) = do
  let lhs_text = ppListNl ppForm (S.toList lft)
  let rhs_text = ppListNl ppForm (S.toList rgt)
  guard (S.member f lft) <|> eb ("Id' fail, LHS missing : " <> ppForm f <> "\nLHS = " <> lhs_text)
  guard (S.member f rgt) <|> eb ("Id' fail, RHS missing : " <> ppForm f <> "\nRHS = " <> rhs_text)
verify _ lft rgt (EqR' x) = guard (S.member (Eq x x) rgt) <|> error "EqR'-fail"
-- verify k lft rgt (EqC (x0, y0, p0) (x1, y1, p1)) = do
--   guard (S.member (Eq x0 x1) lft && S.member (Eq y0 y1) rgt) <|> error "EqC-fail"
--   verifyEqGoal k lft rgt (x0, y0, p0)
--   verifyEqGoal k lft rgt (x1, y1, p1)
verify k lft rgt (EqS' x y) =
  guard (S.member (Eq x y) lft && S.member (Eq y x) rgt) <|> error "EqS'-fail"
verify k lft rgt (EqT' x y z) =
  guard (S.member (Eq x y) lft && S.member (Eq y z) lft && S.member (Eq x z) rgt) <|> error "EqT'-fail"
verify k lft rgt (FunC' f xs ys) = do
  xys <- zipM xs ys 
  guardMsg "Fun-C : premise missing" $ L.all (\ (x_, y_) -> S.member (x_ === y_) lft) xys
  guardMsg "Fun-C : conclusion missing" $ S.member (Fun f xs === Fun f ys) rgt

-- verify k lft rgt (FunC' f egs) = do
--   let xs = L.map (\ (x, _, _) -> x) egs
--   let ys = L.map (\ (_, y, _) -> y) egs
--   guard (S.member (Eq (Fun f xs) (Fun f ys)) rgt) <|> error "FunC'-fail"
--   mapM_ (verifyEqGoal k lft rgt) egs
verify k lft rgt (RelC' r xs ys) = do
  -- let xs = L.map (\ (x, _, _) -> x) egs
  -- let ys = L.map (\ (_, y, _) -> y) egs
  -- guard (S.member (Rel r xs) lft && S.member (Rel r ys) rgt) <|> error "RelC'-fail"
  -- mapM_ (verifyEqGoal k lft rgt) egs
  xys <- zipM xs ys 
  guardMsg "Fun-C : eq-premise missing" $ L.all (\ (x_, y_) -> S.member (x_ === y_) lft) xys
  guardMsg "Fun-C : premise missing" $ S.member (Rel r xs) lft
  guardMsg "Fun-C : conclusion missing" $ S.member (Rel r ys) rgt
verify k lft rgt (NotT' f p) = do
  guard (S.member (Not f) lft) <|> error "NotT'-fail"
  verify k lft (S.insert f rgt) p
verify k lft rgt (NotF' f p) = do
  guard (S.member (Not f) rgt) <|> error "NotF'-fail"
  verify k (S.insert f lft) rgt p
verify k lft rgt (OrT' gls) = do
  let fs = L.map fst gls
  guard (S.member (Or fs) lft) <|> eb ("OrT'-fail : " <> ppForm (Or fs))
  mapM_ (verifyLftGoal k lft rgt) gls
verify k lft rgt (OrF' fs gs p) = do
  guard (sublist gs fs && S.member (Or fs) rgt) <|> error "OrF'-fail"
  verify k lft (L.foldl (flip S.insert) rgt gs) p
verify k lft rgt (AndT' fs gs p) = do
  guard (sublist gs fs) <|> error "AndT'-fail : not subset"
  guard (S.member (And fs) lft) <|> ev "AndT'-fail : " (And fs) lft rgt
  verify k (L.foldl (flip S.insert) lft gs) rgt p
verify k lft rgt (AndF' gls) = do
  let fs = L.map fst gls
  guard (S.member (And fs) rgt) <|> ev "AndF'-fail" (And fs) lft rgt
  mapM_ (verifyRgtGoal k lft rgt) gls
verify k lft rgt (ImpT' f g p q) = do
  guard (S.member (Imp f g) lft) <|> ev "ImpT'-fail" (f ==> g) lft rgt
  verify k lft (S.insert f rgt) p
  verify k (S.insert g lft) rgt q
verify k lft rgt (ImpFA' f g p) = do
  guard (S.member (Imp f g) rgt) <|> error "ImpFA'-fail"
  verify k (S.insert f lft) rgt p
verify k lft rgt (ImpFC' f g p) = do
  guard (S.member (Imp f g) rgt) <|> error "ImpFC'-fail"
  verify k lft (S.insert g rgt) p
verify k lft rgt (IffF' f g p q) = do
  guard (S.member (Iff f g) rgt) <|> ev "IffF'-fail" (f <=> g) lft rgt
  verify k lft (S.insert (Imp f g) rgt) p
  verify k lft (S.insert (Imp g f) rgt) q
verify k lft rgt (IffTO' f g p) = do
  guard (S.member (Iff f g) lft) <|> ev "IffTO'-fail : " (f <=> g) lft rgt
  verify k (S.insert (Imp f g) lft) rgt p
verify k lft rgt (IffTR' f g p) = do
  guard (S.member (f <=> g) lft) <|> ev "IffTR'-fail : " (f <=> g) lft rgt
  verify k (S.insert (Imp g f) lft) rgt p
verify k lft rgt (FaT' vxs f p) = do
  let vs = L.map fst vxs
  guard (S.member (Fa vs f) lft) <|> ev "FaT'-fail : " (Fa vs f) lft rgt
  verify k (S.insert (substForm vxs f) lft) rgt p
verify k lft rgt (FaF' vs m f p) = do
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "FaF'-fail : cannot zip"
  guard (k <= m && S.member (Fa vs f) rgt) <|> ev "FaF'-fail" (Fa vs f) lft rgt
  verify k' lft (S.insert (substForm vxs f) rgt) p
verify k lft rgt (ExT' vs m f p) = do
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "ExT'-fail"
  guard (k <= m && S.member (Ex vs f) lft) <|> error "ExT'-fail"
  verify k' (S.insert (substForm vxs f) lft) rgt p
verify k lft rgt (ExF' vxs f p) = do
  let vs = L.map fst vxs
  guard (S.member (Ex vs f) rgt) <|> error "ExF'-fail"
  verify k lft (S.insert (substForm vxs f) rgt) p
verify k lft rgt (Cut' f p0 p1) = do
  verify k lft (S.insert f rgt) p0
  verify k (S.insert f lft) rgt p1
verify k lft rgt (Mrk s p) = verify k lft rgt p


-- k.m:n.m:n
-- k:m.n:m.n

{- Main -}

skipList :: String -> Bool
skipList n = False

epIncr :: EP -> EP
epIncr (k, l) = (k + 1, l)

epFork :: Int -> EP -> EP
epFork 0 ep = epIncr ep
epFork m (k, l) = (0, (m - 1, k) : l)

ppBranch :: Branch -> Builder
ppBranch br = ppListNl ppSignForm $ L.map fst (HM.toList br)

addExp :: Branch -> Form -> Bool -> EP -> Int -> Prf -> IO [EF]
addExp br f pl ep k p = do
  let br' = HM.insert (f, pl) (tlt $ ppEP ep) br
  -- pt "\nWorking on proof :\n"
  -- pt $ T.intercalate "\n" $ ppPrf 100 p
  -- pt "\nContext:\n"
  -- pt $ ppListNl ppSignForm $ L.map fst (HM.toList br')
  -- pt "\n\n"
  expp br' f pl ep k p

expp :: Branch -> Form -> Bool -> EP -> Int -> Prf -> IO [EF]
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

expp br f sd ep k Asm = return [(ep, sd, f, k, Open, nt)]

expp _ f b ep k p = eb $ ppInter "\n" $ "expansion not implemented" : ppPrf 10 p

expOr :: Branch -> Int -> Text ->  Form -> Bool -> EP -> [Form] -> Prf -> IO [EF]
expOr br k epg f pl ep [] p = expp br f pl ep k p
expOr br k epg f pl ep (g : gs) p = do
  let ep' = epIncr ep 
  let br' = HM.insert (g, bf) (tlt $ ppEP ep') br 
  efs <- expOr br' k epg g bf ep' gs p
  return $ (ep, pl, f, k, OrF epg, nt) : efs

expAnd :: Branch -> Int -> Text -> Form -> Bool -> EP -> [Form] -> Prf -> IO [EF]
expAnd br k epg f pl ep [] p = expp br f pl ep k p
expAnd br k epg f pl ep (g : gs) p = do
  let ep' = epIncr ep 
  let br' = HM.insert (g, bt) (tlt $ ppEP ep') br 
  efs <- expAnd br' k epg g bt ep' gs p
  return $ (ep, pl, f, k, AndT epg, nt) : efs

type Branch = HM.Map (Form, Bool) Text

elabNote :: Elab -> Text
elabNote (Plab _ _ n) = n
elabNote (RelD' _ _ _ _ n) = n
elabNote (AoC' _ _ _ _ n) = n
-- elabNote (ElabFail _ n) = n

expand' :: Branch -> Form -> EP -> [Elab] -> IO [EF]
expand' br f ep [] = expand br f ep [] 
expand' br f ep (el : els) = do 
  -- ptnl $ "Expanding : " <> elabNote el
  expand br f ep (el : els) 

expand :: Branch -> Form -> EP -> [Elab] -> IO [EF]
expand _ (Or []) ep [] = return [(ep, bt, Or [], 0, OrT (tlt $ ppEP ep), Just "'EOP'")]
expand _ f ep [] = et "last added formula must be bot\n"
expand br f ep (Plab g p tx : els) = do
  let br' = HM.insert (f, bt) (tlt $ppEP ep) br
  efs0 <- expand' br' g (epFork 0 ep) els
  efs1 <- addExp br' g bf (epFork 1 ep) 0 p
  return $ (ep, bt, f, 0, Cut, Just tx) : efs0 ++ efs1
expand br f ep (RelD' r g h p tx : els) = do 
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

elabHasAsm :: Elab -> Bool
elabHasAsm (Plab _ p _) = prfHasAsm p
elabHasAsm (RelD' _ _ _ p _) = prfHasAsm p
elabHasAsm (AoC' _ _ _ p _) = prfHasAsm p

-- elabHasSjt :: Elab -> Bool
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



getElabHypsAFs :: Bool -> String -> String -> IO (Hyps, [AF])
getElabHypsAFs verbose tptp tstp = do
  tptp_afs <- parseName tptp
  tstp_afs <- sortAfs <$> parseName tstp
  let hs = L.foldl addHyp (HM.empty, S.empty) tptp_afs
  Prelude.putStr $ tptp ++ "\n"
  when verbose $ mapM_ putAF tptp_afs
  Prelude.putStr $ tstp ++ "\n"
  when verbose $ mapM_ putAF tstp_afs
  return (hs, tstp_afs)

elaborate :: [String] -> IO ()
elaborate (tptp : tstp : estp : flags) = do
  let verbose = "silent" `notElem` flags
  (hs, afs) <- getElabHypsAFs verbose tptp tstp
  (_, es) <- mapAccumM (elabIO verbose) hs afs 
  -- let allCount = L.length es 
  -- let fullCount = L.length $ L.filter (not . elabHasAsm) es
  -- pb $ "Full elaboration rate = " <> ppInt fullCount <> "/" <> ppInt allCount <> "\n"
  let hbr = HM.foldrWithKey (\ nm_ f_ br_ -> HM.insert (f_, bt) nm_ br_) HM.empty (fst hs)
  efs <- expand' hbr (And []) (0, []) es
  --guardMsg "single-junct detected" $ not $ L.any elabSingleJunct efs
  -- mapM_ (\ ef_ -> when (formSJ $ efForm ef_) $ ptnl $ "single junct EF : " <> ppEP (efEP ef_)) efs
  writeElab estp efs

elaborate args = error $ "invalid arguments : " ++ unwords args

rfsj :: Form -> Form
rfsj (Not f) = Not $ rfsj f
rfsj (Imp f g) = Imp (rfsj f) (rfsj g)
rfsj (Iff f g) = Iff (rfsj f) (rfsj g)
rfsj (Or fs) = 
  case L.map rfsj fs of 
    [f] -> f 
    fs' -> Or fs'
rfsj (And fs) = 
  case L.map rfsj fs of 
    [f] -> f 
    fs' -> And fs'
rfsj (Fa vs f) = Fa vs $ rfsj f
rfsj (Ex vs f) = Ex vs $ rfsj f
rfsj f@(Rel _ _) = f
rfsj f@(Eq _ _) = f

rpsj :: Prf -> Prf
rpsj (Id' f) = Id' $ rfsj f
rpsj p@(EqR' _) = p
rpsj p@(EqS' _ _) = p
rpsj p@EqT' {} = p
rpsj p@FunC' {} = p
rpsj p@RelC' {} = p
rpsj (NotT' f p) = NotT' (rfsj f) (rpsj p)
rpsj (NotF' f p) = NotF' (rfsj f) (rpsj p)

rpsj (OrT' [(f, p)]) = rpsj p
rpsj (OrT' ps) = OrT' $ L.map (bimap rfsj rpsj) ps

rpsj (AndF' [(f, p)]) = rpsj p
rpsj (AndF' ps) = AndF' $ L.map (bimap rfsj rpsj) ps

rpsj (AndT' [f] gs p) = 
  case gs of 
    [g] -> if f == g then rpsj p else et "rpsj-and-lft-0"
    _ -> et "rpsj-and-lft-1"
rpsj (AndT' fs gs p) = AndT' (L.map rfsj fs) (L.map rfsj gs) (rpsj p)

rpsj (OrF' [f] gs p) = 
  case gs of 
    [g] -> if f == g then rpsj p else et "rpsj-or-rgt-0"
    _ -> et "rpsj-or-rgt-1"
rpsj (OrF' fs gs p) = OrF' (L.map rfsj fs) (L.map rfsj gs) (rpsj p)

rpsj (ImpFA' f g p) = ImpFA' (rfsj f) (rfsj g) (rpsj p)
rpsj (ImpFC' f g p) = ImpFC' (rfsj f) (rfsj g) (rpsj p)
rpsj (ImpT' f g pf pg) = ImpT' (rfsj f) (rfsj g) (rpsj pf) (rpsj pg)
rpsj (IffTO' f g p) = IffTO' (rfsj f) (rfsj g) (rpsj p)
rpsj (IffTR' f g p) = IffTR' (rfsj f) (rfsj g) (rpsj p)
rpsj (IffF' f g pf pg) = IffF' (rfsj f) (rfsj g) (rpsj pf) (rpsj pg)
rpsj (FaT' vxs f p) = FaT' vxs (rfsj f) (rpsj p)
rpsj (ExF' vxs f p) = ExF' vxs (rfsj f) (rpsj p)
rpsj (FaF' xs k f p) = FaF' xs k (rfsj f) (rpsj p)
rpsj (ExT' xs k f p) = ExT' xs k (rfsj f) (rpsj p)
rpsj (Cut' f pl pr) = Cut' (rfsj f) (rpsj pl) (rpsj pr)
rpsj (Mrk t p) = Mrk t $ rpsj p
rpsj Asm = Asm

resj :: Elab -> Elab
resj (Plab n p t) = Plab n (rpsj p) t
resj (AoC' xs f g p t) = AoC' xs (rfsj f) (rfsj g) (rpsj p) t
resj (RelD' xs f g p t) = RelD' xs (rfsj f) (rfsj g) (rpsj p) t

detectSJ :: EF -> IO ()
detectSJ (ep, _, f, _, _, _) 
  | formSJ f = eb $ "single junct at EP : " <> ppEP ep
  | otherwise = return ()

writeElab :: String -> [EF] -> IO ()
writeElab nm efs = do
  let output = tlt $ ppInter "\n" $ L.map writeEF efs
  Prelude.putStrLn $ "Writing EF : " <> nm
  -- h <- SIO.openFile nm WriteMode
  -- -- mapM_ (TIO.hPutStrLn h . writeEF) efs
  -- TIO.hPutStr h output 
  -- hClose h
  TIO.writeFile nm output

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = nt
unsnoc [x] = Just ([], x)
unsnoc (x : xs) = do 
  (xs', x') <- Main.unsnoc xs 
  return (x : xs', x')

readEpAux :: Text -> Maybe (Int, Int)
readEpAux t = 
  case T.splitOn "." t of
    [t0, t1] -> do 
      k <- cast $ readInt t0
      m <- cast $ readInt t1
      return (k, m)
    _ -> mzero

unsq :: Text -> Maybe Text
unsq ('\'' :> t) = do 
  (t', '\'') <- T.unsnoc t
  return t'
unsq _ = mzero

readEp :: Text -> Maybe EP
readEp t = do
  (t' : ts') <- T.splitOn ":" <$> unsq t
  -- (ts', t') <- cast $ Main.unsnoc ts
  k <- cast $ readInt t'
  l <- mapM readEpAux ts'
  return (k, l)

gTermToText :: Gterm -> IO Text
gTermToText (Gfun t []) = return t 
gTermToText _ = mzero

gTermToTerm :: Gterm -> IO Term
gTermToTerm (Gfun f ts) = Fun f <$> mapM gTermToTerm ts 
gTermToTerm (Gvar v) = return $ Var v
gTermToTerm _ = mzero

gTermToInf :: Gterm -> IO Inf
gTermToInf (Gfun "cut" []) = return Cut
gTermToInf (Gfun "id" [gt0, gt1]) = do 
  m <- gTermToText gt0
  n <- gTermToText gt1 
  return $ Id m n

gTermToInf (Gfun "iffto" [gt]) = IffTO <$> gTermToText gt
gTermToInf (Gfun "ifftr" [gt]) = IffTR <$> gTermToText gt
gTermToInf (Gfun "ifff" [gt]) = IffF <$> gTermToText gt 

gTermToInf (Gfun "impfa" [gt]) = ImpFA <$> gTermToText gt 
gTermToInf (Gfun "impfc" [gt]) = ImpFC <$> gTermToText gt 
gTermToInf (Gfun "impt" [gt]) = ImpT <$> gTermToText gt 

gTermToInf (Gfun "ort" [gt]) = OrT <$> gTermToText gt 
gTermToInf (Gfun "orf" [gt]) = OrF <$> gTermToText gt 

gTermToInf (Gfun "andt" [gt]) = AndT <$> gTermToText gt 
gTermToInf (Gfun "andf" [gt]) = AndF <$> gTermToText gt 

gTermToInf (Gfun "faf" [gt, Gnum k]) = (`FaF` k) <$> gTermToText gt
gTermToInf (Gfun "ext" [gt, Gnum k]) = (`ExT` k) <$> gTermToText gt

gTermToInf (Gfun "fat" [gt, Glist gts]) = do 
  nm <- gTermToText gt
  xs <- mapM gTermToTerm gts 
  return $ FaT nm xs
gTermToInf (Gfun "exf" [gt, Glist gts]) = do 
  nm <- gTermToText gt
  xs <- mapM gTermToTerm gts 
  return $ ExF nm xs

gTermToInf (Gfun "nott" [gt]) = NotT <$> gTermToText gt 
gTermToInf (Gfun "notf" [gt]) = NotF <$> gTermToText gt 
gTermToInf (Gfun "eqr" [gt]) = EqR <$> gTermToText gt 
gTermToInf (Gfun "eqs" gts) = do
  [nm0, nm1] <- mapM gTermToText gts
  return $ EqS nm0 nm1 
gTermToInf (Gfun "eqt" gts) = do
  [nm0, nm1, nm2] <- mapM gTermToText gts
  return $ EqT nm0 nm1 nm2
gTermToInf (Gfun "func" [Glist gts, gt]) = do
  nms <- mapM gTermToText gts 
  nm <- gTermToText gt
  return $ FunC nms nm
gTermToInf (Gfun "relc" [Glist gts, gt0, gt1]) = do
  nms <- mapM gTermToText gts 
  m <- gTermToText gt0
  n <- gTermToText gt1
  return $ RelC nms m n
gTermToInf (Gfun "aoc" gts) =  -- return $ AoC k
  AoC <$> mapM gTermToTerm gts
gTermToInf (Gfun "reld" []) = return RelD 
gTermToInf (Gfun "open" []) = return Open

gTermToInf t = et $ "inf reader : " <> pack (show t)

textToDir :: Text -> IO Dir
textToDir "obv" = return Obv
textToDir "rev" = return Rev
textToDir _ = et "Cannot read direction"

textToBool :: Text -> IO Bool
textToBool "true" = return bt
textToBool "false" = return bf
textToBool _ = et "Cannot read Boolarity"

-- readSide :: Text -> IO Side
-- readSide "lft" = return Lft
-- readSide "rgt" = return Rgt
-- readSide _ = et "Cannot read side"

gTermsToMaybeText :: Maybe [Gterm] -> IO (Maybe Text)
gTermsToMaybeText Nothing = return nt
gTermsToMaybeText (Just [Gfun tx []]) = return $ Just tx
gTermsToMaybeText _ = et "Cannot extact maybe text"

afToEf :: AF -> IO EF
afToEf (nm, sgn, f, Just (Gfun "inference" [Gnum k, gt], gts)) = do 
  pl <- textToBool sgn
  ep <- cast $ readEp nm
  i <- gTermToInf gt
  mtx <- gTermsToMaybeText gts
  return (ep, pl, f, k, i, mtx)
afToEf af = et $ "cannot read AF into EF : " <> tlt (fmtAF af)

isVar :: Term -> Bool 
isVar (Var _) = True
isVar _ = False
-- 
-- bar :: [Term] -> IO ()
-- bar [] = return ()
-- bar [_] = return ()
-- bar (Fun _ vvs : Fun f vws : xs) = do
--   guardMsg "bar-fail" (vvs == vws && L.all isVar vvs) 
--   bar (Fun f vws : xs)
-- bar (x : y : xs) = et "bar-fail-2"
-- 
-- foo :: AF -> IO ()
-- foo (n, g, Just (Gfun "introduced" [Gfun "choice_axiom" [], Glist []])) = do
--   (xs, f) <- normalizeAoC g
--   bar xs
-- foo _ = return ()

dev :: [String] -> IO ()
dev (tptp : flags) 
  | "quick" `elem` flags = do
     afs <- parsePreName tptp
     let k = L.length afs
     pb $ ppInt k <> " annotated pre-formulas parsed.\n"
  | otherwise = do 
     afs <- parseName tptp
     let k = L.length afs
     pb $ ppInt k <> " annotated formulas parsed.\n"
dev _ = et "invalid args"

check :: [String] -> IO ()
check (tptp : estp : flags) = do
  let vb = "silent" `notElem` flags
  pt $ "TPTP : " <> pack tptp <> "\n" 
  tptp_afs <- parseName tptp
  pt $ "ESTP : " <> pack estp <> "\n" 
  estp_afs <- parseName estp
  efs <- mapM afToEf estp_afs
  let _bmp = L.foldl (\ mp_ (nm_, _, f_, _) -> HM.insert nm_ (f_, bt) mp_) HM.empty tptp_afs
  -- pt $ ppListNl writeEF efs
  let bmp = L.foldl (\ mp_ (ep_, pl_, f_, _, _, _) -> HM.insert (tlt $ ppEP ep_) (f_, pl_) mp_) _bmp efs
  let fmp = L.foldl (\ mp_ (ep_, pl_, f_, k_, _, _) -> HM.insert ep_ (f_, pl_, k_) mp_) HM.empty efs
  (top, bt, 0) <- cast $ HM.lookup (0, []) fmp
  mapM_ (checkEF' vb bmp fmp) efs
check _ = et "invalid args for check"

checkEF' :: Bool -> HM.Map Text (Form, Bool) -> HM.Map EP (Form, Bool, Int) -> EF -> IO ()
checkEF' vb bm fm ef = do
  when vb $ pb $ "checking EF : " <> writeEF ef <> "\n"
  checkEF bm fm ef 

ppFM :: HM.Map EP (Form, Bool, Int) -> Builder
ppFM fm = ppListNl (\ (ep_, (f_, pl_, _)) -> ppEP ep_ <> " : " <> ppSignForm (f_, pl_)) $ HM.toList fm

type Branch' = HM.Map Text (Form, Bool) 

ppHM :: (a -> Builder) -> (b -> Builder) -> HM.Map a b -> Builder
ppHM f g m = ppListNl (\ (x_, y_) -> f x_ <> " : " <> g y_) $ HM.toList m

ppBranch' :: Branch' -> Builder
ppBranch' = ppHM ft ppSignForm 

checkEF :: Branch' -> HM.Map EP (Form, Bool, Int) -> EF -> IO ()

checkEF bm fm (ep, _, _, k, Cut, _) = do 
  let ep0 = epFork 0 ep 
  let ep1 = epFork 1 ep
  (g0, bt, k0) <- cast $ HM.lookup ep0 fm
  (g1, bf, k1) <- cast $ HM.lookup ep1 fm
  guardMsg "cut fail" $ g0 == g1 && k == k0 && k == k1

checkEF bm fm (ep, _, _, k, OrT nm, _) = do 
  guard $ onPath ep nm
  pf <- cast $ HM.lookup nm bm 
  case pf of 
    (Or fs, bt) -> guard $ checkJunct fm ep k bt 0 fs
    _ -> eb $ "Not a positive disjunction : " <> ppSignForm pf


checkEF bm fm (ep, _, _, k, AndF nm, _) = do 
  guard $ onPath ep nm
  (And fs, bf) <- cast $ HM.lookup nm bm 
  guard $ checkJunct fm ep k bf 0 fs

checkEF bm fm (ep, _, _, k, AndT nm, _) = do 
  guard $ onPath ep nm
  (And fs, bt) <- cast $ HM.lookup nm bm 
  (f, bt, k') <- cast $ HM.lookup (epIncr ep) fm  
  guard $ k == k' && f `elem` fs

checkEF bm fm (ep, _, _, k, RelD, _) = do 
  (f, bt, k') <- cast $ HM.lookup (epIncr ep) fm  
  guard $ k == k' 
 -- checkRelD k f k' 

checkEF bm fm (ep, _, _, k, AoC xs, _) = do 
  (f, bt, k') <- cast $ HM.lookup (epIncr ep) fm
  isAoC' xs f
  -- guard $ k <= m 
  -- (f, bt, k') <- cast $ HM.lookup (epIncr ep) fm
  -- kos <- offsetAoC m f
  -- guard $ kos <= k' 
  -- isAoC m f


checkEF bm fm (ep, _, _, k, OrF nm, _) = do 
  guard $ onPath ep nm
  (Or fs, bf) <- cast $ HM.lookup nm bm 
  (f, bf, k') <- cast $ HM.lookup (epIncr ep) fm  
  guard $ k == k' && f `elem` fs

checkEF bm fm (ep, _, _, k, ImpFA nm, _) = do 
  guard $ onPath ep nm
  (Imp f g, bf) <- cast $ HM.lookup nm bm 
  guard $ checkDown fm ep 0 [(f, bt, k)]

checkEF bm fm (ep, _, _, k, ImpFC nm, _) = do 
  guard $ onPath ep nm
  (Imp f g, bf) <- cast $ HM.lookup nm bm 
  guard $ checkDown fm ep 0 [(g, bf, k)]

checkEF bm fm (ep, _, _, k, IffTO nm, _) = do 
  guard $ onPath ep nm
  (Iff f g, bt) <- cast $ HM.lookup nm bm 
  guard $ checkJunct fm ep k bt 0 [Imp f g]

checkEF bm fm (ep, _, _, k, IffTR nm, _) = do 
  guard $ onPath ep nm
  (Iff f g, bt) <- cast $ HM.lookup nm bm 
  guard $ checkJunct fm ep k bt 0 [Imp g f]

checkEF bm fm (ep, _, _, k, ImpT nm, _) = do 
  guard $ onPath ep nm
  (Imp f g, bt) <- cast $ HM.lookup nm bm 
  guard $ checkDown fm ep 0 [(f, bf, k), (g, bt, k)]

checkEF bm fm (ep, _, _, k, IffF nm, _) = do 
  guard $ onPath ep nm
  (Iff f g, bf) <- cast $ HM.lookup nm bm 
  guard $ checkDown fm ep 0 [(f ==> g, bf, k), (g ==> f, bf, k)]

checkEF bm fm (ep, _, _, k, NotF nm, _) = do 
  guard $ onPath ep nm
  (Not f, bf) <- cast $ HM.lookup nm bm 
  guard $ checkDown fm ep 0 [(f, bt, k)]

checkEF bm fm (ep, _, _, k, NotT nm, _) = do 
  guard $ onPath ep nm
  (Not f, bt) <- cast $ HM.lookup nm bm 
  guard $ checkDown fm ep 0 [(f, bf, k)]

checkEF bm fm (ep, _, _, k, EqR nm, _) = do 
  guard $ onPath ep nm
  (Eq x y, bf) <- cast $ HM.lookup nm bm 
  guard $ x == y

checkEF bm fm (ep, _, _, k, Id nm0 nm1, _) = do 
  guard $ onPath ep nm0
  guard $ onPath ep nm1
  (f, bt) <- cast $ HM.lookup nm0 bm 
  (g, bf) <- cast $ HM.lookup nm1 bm 
  guard $ f == g

checkEF bm fm (ep, _, _, k, FaF nm m, _) = do 
  guard $ onPath ep nm && k <= m
  (Fa vs f, bf) <- cast $ HM.lookup nm bm 
  let (m', vxs) = varPars m vs
  let f' = substForm vxs f
  guard $ checkDown fm ep 0 [(f', bf, m')]

checkEF bm fm (ep, _, _, k, FaT nm xs, _) = do 
  guard $ onPath ep nm
  (Fa vs f, bt) <- cast $ HM.lookup nm bm 
  vxs <- zipM vs xs
  let f' = substForm vxs f
  guard $ checkDown fm ep 0 [(f', bt, k)]

checkEF bm fm (ep, _, _, k, ExT nm m, _) = do 
  guard $ onPath ep nm && k <= m
  (Ex vs f, bt) <- cast $ HM.lookup nm bm 
  let (m', vxs) = varPars m vs
  let f' = substForm vxs f
  guard $ checkDown fm ep 0 [(f', bt, m')]

checkEF bm fm (ep, _, _, k, ExF nm xs, _) = do 
  guard $ onPath ep nm
  (Ex vs f, bf) <- cast $ HM.lookup nm bm 
  vxs <- zipM vs xs
  let f' = substForm vxs f
  guard $ checkDown fm ep 0 [(f', bf, k)]

checkEF bm fm (ep, _, _, k, EqS nm0 nm1, _) = do 
  guard $ L.all (onPath ep) [nm0, nm1]
  (Eq x y, bt) <- cast $ HM.lookup nm0 bm 
  (Eq y' x', bf) <- cast $ HM.lookup nm1 bm 
  guardMsg "Eq-S : mismatch" $ x == x' && y == y' 

checkEF bm fm (ep, _, _, k, EqT nm0 nm1 nm2, _) = do 
  guard $ L.all (onPath ep) [nm0, nm1, nm2]
  (Eq x y, bt) <- cast $ HM.lookup nm0 bm 
  (Eq y' z, bt) <- cast $ HM.lookup nm1 bm 
  (Eq x' z', bf) <- cast $ HM.lookup nm2 bm 
  guardMsg "Eq-T : mismatch" $ x == x' && y == y' && x == x'

checkEF bm fm (ep, _, _, k, FunC nms nm, _) = do 
  guard $ L.all (onPath ep) (nm : nms)
  (Eq (Fun f xs) (Fun g ys), bf) <- cast $ HM.lookup nm bm 
  nmxys <- zipM xs ys >>= zipM nms
  guardMsg "Fun-C : mismatch" $ L.all (checkEqPrem bm) nmxys

checkEF bm fm (ep, _, _, k, RelC nms nm0 nm1, _) = do 
  guard $ L.all (onPath ep) (nm0 : nm1 : nms)
  (Rel r xs, bt) <- cast $ HM.lookup nm0 bm 
  (Rel s ys, bf) <- cast $ HM.lookup nm1 bm 
  guard $ r == s
  nmxys <- zipM xs ys >>= zipM nms
  guardMsg "Rel-C : mismatch" $ L.all (checkEqPrem bm) nmxys

checkEF bm fm (ep, _, _, k, Open, _) = return ()

-- checkEF bm fm (_, _, _, _, i) = et $ "unsupported inference : " <> ppInf i

checkEqPrem :: Map Text (Form, Bool) -> (Text, (Term, Term)) -> Bool
checkEqPrem bm (nm, (x, y)) = 
  case HM.lookup nm bm of 
    Just (Eq x' y', bt) -> x == x' && y == y'
    _ -> False

breakIff :: Form -> Form -> Dir -> Form
breakIff f g Obv = f ==> g
breakIff f g Rev = g ==> f

checkDown :: HM.Map EP (Form, Bool, Int) -> EP -> Int -> [(Form, Bool, Int)] -> Bool
checkDown fm ep k [] = True
checkDown fm ep k ((f, pl, m) : l) = do
  case HM.lookup (epFork k ep) fm of 
    Just (f', pl', m') -> f == f' && pl == pl' && m == m' && checkDown fm ep (k + 1) l
    _ -> False

checkJunct :: HM.Map EP (Form, Bool, Int) -> EP -> Int -> Bool -> Int -> [Form] -> Bool
checkJunct fm ep k pl m [] = True
checkJunct fm ep k pl m (f : fs) = 
  case HM.lookup (epFork m ep) fm of 
    Just (f', pl', k') -> f == f' && pl == pl' && k == k' && checkJunct fm ep k pl (m + 1) fs 
    _ -> False
    
subEPRec :: EP -> EP -> Bool
subEPRec (0, []) _ = False
subEPRec (0, (_, m) : l) ep = subEP (m, l) ep
subEPRec (k, l) ep = subEP (k - 1, l) ep

subEP :: EP -> EP -> Bool
subEP ep ep'
  | ep == ep' = True
  | otherwise = subEPRec ep ep'

-- subEP :: [(Int, Int)] -> Int -> [(Int, Int)] -> Int -> Bool
-- subEP 

onPath :: EP -> Text -> Bool
onPath ep nm = 
  case readEp nm of 
    Just ep' -> subEP ep ep'
    _ -> True

ps :: String -> IO ()
ps = Prelude.putStr 

main :: IO ()
main = do
  (cmd : args) <- getArgs
  case cmd of
    "elab" -> elaborate args
    "check" -> check args
    "dev" -> dev args
    _ -> et "undefined command"