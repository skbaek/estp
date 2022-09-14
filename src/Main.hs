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
import Parse ( parseName )
import Sat ( sat )
import Lem
import Norm
import Prove

import Control.Monad as M ( guard, MonadPlus(mzero), foldM_, when )
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative ( Alternative((<|>)) )
import System.Environment ( getArgs )
import Data.List as L ( map, foldl, all, sortBy, concat, reverse, length )
import Data.Text as T ( Text, unpack, intercalate, pack, null, splitOn, unsnoc )
import Data.Set as S ( empty, insert, member, singleton, toList )
import Data.Map as HM ( Map, empty, insert, lookup, toList, foldrWithKey )
import Data.Text.IO as TIO ( hPutStrLn )
import System.IO as SIO ( openFile, hClose, IOMode(WriteMode) )

putAnForm :: AnForm -> IO ()
putAnForm i = Prelude.putStr $ ppAnForm i ++ "\n"

addHyp :: Hyps -> AnForm -> Hyps
addHyp (nsq, sq) (Af n f _) = (HM.insert n f nsq, S.insert f sq)

sortAfs :: [AnForm] -> [AnForm]
sortAfs = sortBy compareAfs

compareAfs :: AnForm -> AnForm -> Ordering
compareAfs (Af (m :> ms) _ _) (Af (n :> ns) _ _) =
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

isRdef :: Text -> Form -> Bool
isRdef r (Fa _ (Iff (Rel s _) _)) = r == s
isRdef r (Iff (Rel s _) _) = r == s
isRdef _ _ = False

checkElab :: Seq -> Form -> Elab -> IO ()
checkElab sq g (Plab g' p) = do
  guard (g == g')
  verify 0 sq (S.singleton g) p
checkElab sq g (Rdef r f p) = do
  guard $ isRdef r f
  verify 0 (S.singleton f) (S.singleton g) p
checkElab sq g (AOC xs f p) = isAOC xs f

isSkolemTerm :: [Text] -> Term -> Bool
isSkolemTerm vs (Fun _ xs) =
  case mapM breakVar xs of
    Just ws -> sublist vs ws && sublist ws vs
    _ -> False
isSkolemTerm _ _ = False

isAOC :: [Term] -> Form -> IO ()
isAOC xs (Fa vs (Imp (Ex ws f) g)) = do
  guard $ L.all (isSkolemTerm vs) xs
  wxs <- zipM ws xs
  guard $ substForm wxs f == g
isAOC xs (Imp (Ex ws f) g) = do
  guard $ L.all isConstant xs
  wxs <- zipM ws xs
  guard $ substForm wxs f == g
isAOC _ _ = mzero

elabIO :: Hyps -> AnForm -> IO (Hyps, Elab) -- todo : eliminate checking during elab-IO
elabIO (nsq, sq) (Af n f a) = do
  print $ "Elaborating step = " <> n
  e <- elab nsq (Af n f a)
  checkElab sq f e
  return ((HM.insert n f nsq, S.insert f sq), e)

gFunFunctor :: Gterm -> Maybe Text
gFunFunctor (Gfun t []) = return t
gFunFunctor _ = Nothing

infer :: Text -> [Form] -> Form -> IO Prf
infer "superposition" [f, g] h         = superpose f g h
infer "forward_demodulation" [f, g] h  = superpose f g h
infer "backward_demodulation" [f, g] h = superpose f g h
infer "negated_conjecture" [f] g = guard (f == g) >> return (Ax f)
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

elab :: NSeq -> AnForm -> IO Elab
elab s (Af n h (Just (Gfun "file" [_, Gfun m []]))) = do
  f <- getHyp m s
  Plab h <$> orig f h
elab _ (Af n g (Just (Gfun "introduced" [Gfun "predicate_definition_introduction" [],Glist [Gfun "new_symbols" [Gfun "naming" [],Glist [Gfun r []]]]]))) = relDef r g
elab _ (Af n g (Just (Gfun "introduced" [Gfun "avatar_definition" [], Glist [Gfun "new_symbols" [Gfun "naming" [], Glist [Gfun r []]]]]))) = relDef r g
elab s (Af n g (Just (Gfun "introduced" [Gfun "choice_axiom" [], Glist []]))) = do
  (xs, f) <- normalizeAOC g
  p <- orig f g
  return $ AOC xs f p
elab s (Af _ g (Just (Gfun "inference" [Gfun "avatar_sat_refutation" [], _, Glist l]))) = do
  fs <- cast (mapM gFunFunctor l) >>= mapM (`lookupM` s)
  Plab g <$> sat fs
elab s (Af _ g (Just (Gfun "inference" [Gfun r [], _, Glist l]))) = do
  fs <- cast (mapM gFunFunctor l) >>= mapM (`lookupM` s)
  p <- infer r fs g
  return $ Plab g p
elab _ (Af _ _ a) = error $ "No elaborator for inference : " ++ show a



{- Verification -}

verifyEqGoal :: Int -> Seq -> Seq -> (Term, Term, Prf) -> IO ()
verifyEqGoal k lft rgt (x, y, p) = verify k lft (S.insert (Eq x y) rgt) p

verifyRgtGoal :: Int -> Seq -> Seq -> (Form, Prf) -> IO ()
verifyRgtGoal k lft rgt (f, p) = verify k lft (S.insert f rgt) p

verifyLftGoal :: Int -> Seq -> Seq -> (Form, Prf) -> IO ()
verifyLftGoal k lft rgt (f, p) = verify k (S.insert f lft) rgt p

ev :: Text -> Form -> Seq -> Seq -> IO ()
ev t f lhs rhs = et $ t <> ppForm f <> "\nLHS :\n" <> ppListNl ppForm (S.toList lhs) <> "\nRHS :\n" <> ppListNl ppForm (S.toList rhs) <> "\n"

verify :: Int -> Seq -> Seq -> Prf -> IO ()
verify _ _ _ Asm = return ()
verify _ lft rgt (Ax f) = do
  let lhs_text = ppListNl ppForm (S.toList lft)
  let rhs_text = ppListNl ppForm (S.toList rgt)
  guard (S.member f lft) <|> et ("Ax fail, LHS missing : " <> ppForm f <> "\nLHS = " <> lhs_text)
  guard (S.member f rgt) <|> et ("Ax fail, RHS missing : " <> ppForm f <> "\nRHS = " <> rhs_text)
verify _ lft rgt (EqR x) = guard (S.member (Eq x x) rgt) <|> error "EqR-fail"
-- verify k lft rgt (EqC (x0, y0, p0) (x1, y1, p1)) = do
--   guard (S.member (Eq x0 x1) lft && S.member (Eq y0 y1) rgt) <|> error "EqC-fail"
--   verifyEqGoal k lft rgt (x0, y0, p0)
--   verifyEqGoal k lft rgt (x1, y1, p1)
verify k lft rgt (EqS x y) =
  guard (S.member (Eq x y) lft && S.member (Eq y x) rgt) <|> error "EqS-fail"
verify k lft rgt (EqT x y z) =
  guard (S.member (Eq x y) lft && S.member (Eq y z) lft && S.member (Eq x z) rgt) <|> error "EqT-fail"
verify k lft rgt (FunC f egs) = do
  let xs = L.map (\ (x, _, _) -> x) egs
  let ys = L.map (\ (_, y, _) -> y) egs
  guard (S.member (Eq (Fun f xs) (Fun f ys)) rgt) <|> error "FunC-fail"
  mapM_ (verifyEqGoal k lft rgt) egs
verify k lft rgt (RelC r egs) = do
  let xs = L.map (\ (x, _, _) -> x) egs
  let ys = L.map (\ (_, y, _) -> y) egs
  guard (S.member (Rel r xs) lft && S.member (Rel r ys) rgt) <|> error "RelC-fail"
  mapM_ (verifyEqGoal k lft rgt) egs
verify k lft rgt (NotL f p) = do
  guard (S.member (Not f) lft) <|> error "NotL-fail"
  verify k lft (S.insert f rgt) p
verify k lft rgt (NotR f p) = do
  guard (S.member (Not f) rgt) <|> error "NotR-fail"
  verify k (S.insert f lft) rgt p
verify k lft rgt (OrL gls) = do
  let fs = L.map fst gls
  guard (S.member (Or fs) lft) <|> error ("OrL-fail : " ++ unpack (ppForm (Or fs)))
  mapM_ (verifyLftGoal k lft rgt) gls
verify k lft rgt (OrR fs gs p) = do
  guard (sublist gs fs && S.member (Or fs) rgt) <|> error "OrR-fail"
  verify k lft (L.foldl (flip S.insert) rgt gs) p
verify k lft rgt (AndL fs gs p) = do
  guard (sublist gs fs) <|> error "AndL-fail : not subset"
  guard (S.member (And fs) lft) <|> ev "AndL-fail : " (And fs) lft rgt
  verify k (L.foldl (flip S.insert) lft gs) rgt p
verify k lft rgt (AndR gls) = do
  let fs = L.map fst gls
  guard (S.member (And fs) rgt) <|> error "AndR-fail"
  mapM_ (verifyRgtGoal k lft rgt) gls
verify k lft rgt (ImpL f g p q) = do
  guard (S.member (Imp f g) lft) <|> ev "ImpL-fail" (f ==> g) lft rgt
  verify k lft (S.insert f rgt) p
  verify k (S.insert g lft) rgt q
verify k lft rgt (ImpRA f g p) = do
  guard (S.member (Imp f g) rgt) <|> error "ImpRA-fail"
  verify k (S.insert f lft) rgt p
verify k lft rgt (ImpRC f g p) = do
  guard (S.member (Imp f g) rgt) <|> error "ImpRC-fail"
  verify k lft (S.insert g rgt) p
verify k lft rgt (IffR f g p q) = do
  guard (S.member (Iff f g) rgt) <|> ev "IffR-fail" (f <=> g) lft rgt
  verify k lft (S.insert (Imp f g) rgt) p
  verify k lft (S.insert (Imp g f) rgt) q
verify k lft rgt (IffLO f g p) = do
  guard (S.member (Iff f g) lft) <|> ev "IffLO-fail : " (f <=> g) lft rgt
  verify k (S.insert (Imp f g) lft) rgt p
verify k lft rgt (IffLR f g p) = do
  guard (S.member (f <=> g) lft) <|> ev "IffLR-fail : " (f <=> g) lft rgt
  verify k (S.insert (Imp g f) lft) rgt p
verify k lft rgt (FaL vxs f p) = do
  let vs = L.map fst vxs
  guard (S.member (Fa vs f) lft) <|> ev "FaL-fail : " (Fa vs f) lft rgt
  verify k (S.insert (substForm vxs f) lft) rgt p
verify k lft rgt (FaR vs m f p) = do
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "FaR-fail : cannot zip"
  guard (k <= m && S.member (Fa vs f) rgt) <|> ev "FaR-fail" (Fa vs f) lft rgt
  verify k' lft (S.insert (substForm vxs f) rgt) p
verify k lft rgt (ExL vs m f p) = do
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "ExL-fail"
  guard (k <= m && S.member (Ex vs f) lft) <|> error "ExL-fail"
  verify k' (S.insert (substForm vxs f) lft) rgt p
verify k lft rgt (ExR vxs f p) = do
  let vs = L.map fst vxs
  guard (S.member (Ex vs f) rgt) <|> error "ExR-fail"
  verify k lft (S.insert (substForm vxs f) rgt) p
verify k lft rgt (Cut f p0 p1) = do
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

ppBranch :: Branch -> Text
ppBranch br = ppListNl ppPolForm $ L.map fst (HM.toList br)

addExp :: Branch -> Form -> Pol -> EP -> Int -> Prf -> IO [EF]
addExp br f pl ep k p = do
  let br' = HM.insert (f, pl) (ppEP ep) br
  -- pt "\nWorking on proof :\n"
  -- pt $ T.intercalate "\n" $ ppPrf 100 p
  -- pt "\nContext:\n"
  -- pt $ ppListNl ppPolForm $ L.map fst (HM.toList br')
  -- pt "\n\n"
  expp br' f pl ep k p

expp :: Branch -> Form -> Pol -> EP -> Int -> Prf -> IO [EF]
expp br f sd ep k (Cut g p0 p1) = do
  let ep0 = epFork 0 ep
  let ep1 = epFork 1 ep
  efs0 <- addExp br g Pos ep0 k p1
  efs1 <- addExp br g Neg ep1 k p0
  return $ (f, sd, ep, k, InfCut) : efs0 ++ efs1

expp br f sd ep k (IffLO g h p) = do
  epgh <- cast $ HM.lookup (g <=> h, Pos) br
  efs <- addExp br (g ==> h) Pos (epIncr ep) k p
  return $ (f, sd, ep, k, InfIffL epgh Obv) : efs

expp br f sd ep k (IffLR g h p) = do
  epgh <- cast $ HM.lookup (g <=> h, Pos) br
  efs <- addExp br (h ==> g) Pos (epIncr ep) k p
  return $ (f, sd, ep, k, InfIffL epgh Rev) : efs

expp br f sd ep k (IffR g h p0 p1) = do
  epgh <- cast $ HM.lookup (g <=> h, Neg) br
  efs0 <- addExp br (g ==> h) Neg (epFork 0 ep) k p0
  efs1 <- addExp br (h ==> g) Neg (epFork 1 ep) k p1
  return $ (f, sd, ep, k, InfIffR epgh) : efs0 ++ efs1

expp br f sd ep k (ImpL g h p0 p1) = do
  epgh <- cast $ HM.lookup (g ==> h, Pos) br
  efs0 <- addExp br g Neg (epFork 0 ep) k p0
  efs1 <- addExp br h Pos (epFork 1 ep) k p1
  return $ (f, sd, ep, k, InfImpL epgh) : efs0 ++ efs1

expp br f sd ep k (NotL g p) = do
  epg <- cast $ HM.lookup (Not g, Pos) br
  efs <- addExp br g Neg (epIncr ep) k p
  return $ (f, sd, ep, k, InfNotL epg) : efs

expp br f sd ep k (NotR g p) = do
  epg <- cast $ HM.lookup (Not g, Neg) br
  efs <- addExp br g Pos (epIncr ep) k p
  return $ (f, sd, ep, k, InfNotR epg) : efs

expp br f sd ep k (ImpRA g h p) = do
  epgh <- cast $ HM.lookup (g ==> h, Neg) br
  efs <- addExp br g Pos (epIncr ep) k p
  return $ (f, sd, ep, k, InfImpR epgh Lft) : efs

expp br f sd ep k (ImpRC g h p) = do
  epgh <- cast $ HM.lookup (g ==> h, Neg) br
  efs <- addExp br h Neg (epIncr ep) k p
  return $ (f, sd, ep, k, InfImpR epgh Rgt) : efs

expp br f sd ep k (FaR vs m g p) = do
  epg <- cast $ HM.lookup (Fa vs g, Neg) br
  let (k', vxs) = varPars k vs
  let g' = substForm vxs g
  efs <- addExp br g' Neg (epIncr ep) k' p
  return $ (f, sd, ep, k, InfFaR epg k) : efs

expp br f sd ep k (FaL vxs g p) = do
  let (vs, xs) = unzip vxs
  epg <- cast $ HM.lookup (Fa vs g, Pos) br
  let g' = substForm vxs g
  efs <- addExp br g' Pos (epIncr ep) k p
  return $ (f, sd, ep, k, InfFaL epg xs) : efs

expp br f pl ep k (OrR gs gs' p) = do
  epg <- cast $ HM.lookup (Or gs, Neg) br
  expOr br k epg 0 f pl ep gs' p
  
expp br f pl ep k (AndL gs gs' p) = do
  epg <- cast $ HM.lookup (And gs, Pos) br
  expAnd br k epg 0 f pl ep gs' p

expp br f pl ep k (OrL fps) = do
  let (fs, ps) = unzip fps
  epg <- cast $ HM.lookup (Or fs, Pos) br
  (_, efss) <- mapAccumM (\ m_ (f_, p_) -> (m_ + 1,) <$> addExp br f_ Pos (epFork m_ ep) k p_) 0 fps
  let efs = L.concat efss
  return $ (f, pl, ep, k, InfOrL epg) : efs

expp br f pl ep k (AndR fps) = do
  let (fs, ps) = unzip fps
  epg <- cast $ HM.lookup (And fs, Neg) br
  (_, efss) <- mapAccumM (\ m_ (f_, p_) -> (m_ + 1,) <$> addExp br f_ Neg (epFork m_ ep) k p_) 0 fps
  let efs = L.concat efss
  return $ (f, pl, ep, k, InfAndR epg) : efs

expp br f sd ep k (Ax g) = do
  epgl <- cast $ HM.lookup (g, Pos) br
  epgr <- cast $ HM.lookup (g, Neg) br
  return [(f, sd, ep, k, InfAx epgl epgr)]

expp br f sd ep k (EqR x) = do
  epg <- cast $ HM.lookup (Eq x x, Neg) br
  return [(f, sd, ep, k, InfEqR epg)]

expp _ f b ep k p = et $ T.intercalate "\n" $ "expansion not implemented" : ppPrf 10 p

expOr :: Branch -> Int -> Text -> Int -> Form -> Pol -> EP -> [Form] -> Prf -> IO [EF]
expOr br k epg m f pl ep [] p = expp br f pl ep k p
expOr br k epg m f pl ep (g : gs) p = do
  let ep' = epIncr ep 
  let br' = HM.insert (g, Neg) (ppEP ep') br 
  efs <- expOr br' k epg (m + 1) g Neg ep' gs p
  return $ (f, pl, ep, k, InfOrR epg m) : efs

expAnd :: Branch -> Int -> Text -> Int -> Form -> Pol -> EP -> [Form] -> Prf -> IO [EF]
expAnd br k epg m f pl ep [] p = expp br f pl ep k p
expAnd br k epg m f pl ep (g : gs) p = do
  let ep' = epIncr ep 
  let br' = HM.insert (g, Pos) (ppEP ep') br 
  efs <- expAnd br' k epg (m + 1) g Pos ep' gs p
  return $ (f, pl, ep, k, InfAndL epg m) : efs

type Branch = HM.Map (Form, Pol) Text

expand :: Branch -> Form -> EP -> [Elab] -> IO [EF]
expand _ (Or []) ep [] = return [(Or [], Pos, ep, 0, InfOrL (ppEP ep))]
expand _ f ep [] = et "last added formula must be bot\n"
expand br f ep (Plab g p : els) = do
  let br' = HM.insert (f, Pos) (ppEP ep) br
  gefs <- addExp br' g Neg (epFork 1 ep) 0 p
  efs <- expand br' g (epFork 0 ep) els
  return $ (f, Pos, ep, 0, InfCut) : efs ++ gefs
expand _ f ep (e : _) = et $ "unimplemented :\n" <> ppElab e

expand0 :: Branch -> Form -> EP -> [Elab] -> IO [EF]
expand0 _ (Or []) ep [] = return [(Or [], Pos, ep, 0, InfOrL (ppEP ep))]
expand0 _ f ep [] = et "last added formula must be bot\n"
expand0 br f ep (Plab g p : els) = do
  let br' = HM.insert (f, Pos) (ppEP ep) br
  efs <- expand0 br' g (epFork 0 ep) els
  return $ (f, Pos, ep, 0, InfCut) : efs 

expand0 _ f ep (e : _) = et $ "unimplemented :\n" <> ppElab e

elaborate :: Bool -> [String] -> IO ()
elaborate full (tptp : tstp : estp : flags) = do
  tptp_afs <- parseName tptp
  tstp_afs <- sortAfs <$> parseName tstp
  let hs = L.foldl addHyp (HM.empty, S.empty) tptp_afs
  Prelude.putStr $ tptp ++ "\n"
  when ("silent" `notElem` flags) $ mapM_ putAnForm tptp_afs
  Prelude.putStr $ tstp ++ "\n"
  when ("silent" `notElem` flags) $ mapM_ putAnForm tstp_afs
  (_, es) <- mapAccumM elabIO hs tstp_afs --elabIO hs tstp_afs
  let hbr = HM.foldrWithKey (\ nm_ f_ br_ -> HM.insert (f_, Pos) nm_ br_) HM.empty (fst hs)
  efs <- if full
         then expand hbr (And []) (0, []) es
         else expand0 hbr (And []) (0, []) es
  writeElab estp efs

elaborate _ args = error "invalid arguments"


writeElab :: String -> [EF] -> IO ()
writeElab nm efs = do
  Prelude.putStrLn $ "Writing EF : " <> nm
  h <- SIO.openFile nm WriteMode
  mapM_ (TIO.hPutStrLn h . writeEF) efs
  hClose h


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
gTermToTerm _ = mzero

gTermToInf :: Gterm -> IO Inf
gTermToInf (Gfun "cut" []) = return InfCut
gTermToInf (Gfun "ax" [gt0, gt1]) = do 
  m <- gTermToText gt0
  n <- gTermToText gt1 
  return $ InfAx m n
gTermToInf (Gfun "iffl" [gt0, gt1]) = do 
  nm <- gTermToText gt0
  dr <- gTermToText gt1 >>= textToDir
  return $ InfIffL nm dr

gTermToInf (Gfun "impr" [gt0, gt1]) = do 
  nm <- gTermToText gt0
  sd <- gTermToText gt1 >>= readSide
  return $ InfImpR nm sd
gTermToInf (Gfun "impl" [gt]) = InfImpL <$> gTermToText gt 
gTermToInf (Gfun "iffr" [gt]) = InfIffR <$> gTermToText gt 
gTermToInf (Gfun "orl" [gt]) = InfOrL <$> gTermToText gt 

gTermToInf (Gfun "orr" [gt, Gnum k]) = (`InfOrR` k) <$> gTermToText gt 
gTermToInf (Gfun "andl" [gt, Gnum k]) = (`InfAndL` k) <$> gTermToText gt 

gTermToInf (Gfun "andr" [gt]) = InfAndR <$> gTermToText gt 

gTermToInf (Gfun "far" [gt, Gnum k]) = (`InfFaR` k) <$> gTermToText gt
gTermToInf (Gfun "exl" [gt, Gnum k]) = (`InfExL` k) <$> gTermToText gt

gTermToInf (Gfun "fal" [gt, Glist gts]) = do 
  nm <- gTermToText gt
  xs <- mapM gTermToTerm gts 
  return $ InfFaL nm xs
gTermToInf (Gfun "exr" [gt, Glist gts]) = do 
  nm <- gTermToText gt
  xs <- mapM gTermToTerm gts 
  return $ InfExR nm xs

gTermToInf (Gfun "notl" [gt]) = InfNotL <$> gTermToText gt 
gTermToInf (Gfun "notr" [gt]) = InfNotR <$> gTermToText gt 
gTermToInf (Gfun "eqr" [gt]) = InfEqR <$> gTermToText gt 

gTermToInf t = et $ "inf reader : " <> pack (show t)

textToDir :: Text -> IO Dir
textToDir "obv" = return Obv
textToDir "rev" = return Rev
textToDir _ = et "Cannot read direction"

textToPol :: Text -> IO Pol
textToPol "pos" = return Pos
textToPol "neg" = return Neg
textToPol _ = et "Cannot read polarity"

readSide :: Text -> IO Side
readSide "lft" = return Lft
readSide "rgt" = return Rgt
readSide _ = et "Cannot read side"

afToEf :: AnForm -> IO EF
afToEf (Af nm f (Just (Gfun "tab" [Gfun plt [], Gnum k, gt]))) = do 
  pl <- textToPol plt
  ep <- cast $ readEp nm
  i <- gTermToInf gt
  return (f, pl, ep, k, i)
afToEf af = et $ "cannot read AF into EF : " <> pack (ppAnForm af)

check :: [String] -> IO ()
check (tptp : estp : flags) = do
  pt $ "TPTP : " <> pack tptp <> "\n" 
  tptp_afs <- parseName tptp
  pt $ "ESTP : " <> pack estp <> "\n" 
  estp_afs <- parseName estp
  -- let hs = L.foldl (\ hs_ (Af nm_ fm_ _) -> HM.insert nm_ fm_ hs_) HM.empty (tptp_afs ++ estp_afs)
  efs <- mapM afToEf estp_afs
  let _bmp = L.foldl (\ mp_ (Af nm_ f_ _) -> HM.insert nm_ (f_, Pos) mp_) HM.empty tptp_afs
  -- pt "Pre-BMP :\n"
  -- mapM_ (\ (nm_, (f_, pl_)) -> pt (nm_ <> " : " <> ppPol pl_ <> " : " <> ppForm f_ <> "\n")) (HM.toList _bmp)
  -- pt "EFs to be added to BMP :\n"
  
  pt $ ppListNl writeEF efs
  let bmp = L.foldl (\ mp_ (f_, pl_, ep_, _, _) -> HM.insert (ppEP ep_) (f_, pl_) mp_) _bmp efs
  let fmp = L.foldl (\ mp_ (f_, pl_, ep_, k_, _) -> HM.insert ep_ (f_, pl_, k_) mp_) HM.empty efs
  (top, Pos, 0) <- cast $ HM.lookup (0, []) fmp
  mapM_ (checkEF' bmp fmp) efs
check _ = et "invalid args for check"

checkEF' :: HM.Map Text (Form, Pol) -> HM.Map EP (Form, Pol, Int) -> EF -> IO ()
checkEF' bm fm ef = do
  pt $ "checking EF : " <> writeEF ef <> "\n"
  checkEF bm fm ef 

checkEF :: HM.Map Text (Form, Pol) -> HM.Map EP (Form, Pol, Int) -> EF -> IO ()
checkEF bm fm (_, _, ep, k, InfCut) = do 
  let ep0 = epFork 0 ep 
  let ep1 = epFork 1 ep
  (g0, Pos, k0) <- cast $ HM.lookup ep0 fm
  (g1, Neg, k1) <- cast $ HM.lookup ep1 fm
  guardMsg (g0 == g1 && k == k0 && k == k1) "cut fail"

checkEF bm fm (_, _, ep, k, InfOrL nm) = do 
  guard $ onPath ep nm
  (Or fs, Pos) <- cast $ HM.lookup nm bm 
  guard $ checkJunct fm ep k Pos 0 fs

checkEF bm fm (_, _, ep, k, InfOrR nm m) = do 
  guard $ onPath ep nm
  (Or fs, Neg) <- cast $ HM.lookup nm bm 
  f <- cast $ nth m fs 
  guard $ checkDown fm ep 0 [(f, Neg, k)]

checkEF bm fm (_, _, ep, k, InfAndR nm) = do 
  guard $ onPath ep nm
  (And fs, Neg) <- cast $ HM.lookup nm bm 
  guard $ checkJunct fm ep k Neg 0 fs

checkEF bm fm (_, _, ep, k, InfAndL nm m) = do 
  guard $ onPath ep nm
  (And fs, Pos) <- cast $ HM.lookup nm bm 
  f <- cast $ nth m fs 
  guard $ checkDown fm ep 0 [(f, Pos, k)]

checkEF bm fm (_, _, ep, k, InfImpR nm Lft) = do 
  guard $ onPath ep nm
  (Imp f g, Neg) <- cast $ HM.lookup nm bm 
  guard $ checkDown fm ep 0 [(f, Pos, k)]

checkEF bm fm (_, _, ep, k, InfImpR nm Rgt) = do 
  guard $ onPath ep nm
  (Imp f g, Neg) <- cast $ HM.lookup nm bm 
  guard $ checkDown fm ep 0 [(g, Neg, k)]

checkEF bm fm (_, _, ep, k, InfIffL nm dr) = do 
  guard $ onPath ep nm
  (Iff f g, Pos) <- cast $ HM.lookup nm bm 
  guard $ checkJunct fm ep k Pos 0 [breakIff f g dr]

checkEF bm fm (_, _, ep, k, InfImpL nm) = do 
  guard $ onPath ep nm
  (Imp f g, Pos) <- cast $ HM.lookup nm bm 
  guard $ checkDown fm ep 0 [(f, Neg, k), (g, Pos, k)]

checkEF bm fm (_, _, ep, k, InfIffR nm) = do 
  guard $ onPath ep nm
  (Iff f g, Neg) <- cast $ HM.lookup nm bm 
  guard $ checkDown fm ep 0 [(f ==> g, Neg, k), (g ==> f, Neg, k)]

checkEF bm fm (_, _, ep, k, InfNotR nm) = do 
  guard $ onPath ep nm
  (Not f, Neg) <- cast $ HM.lookup nm bm 
  guard $ checkDown fm ep 0 [(f, Pos, k)]

checkEF bm fm (_, _, ep, k, InfNotL nm) = do 
  guard $ onPath ep nm
  (Not f, Pos) <- cast $ HM.lookup nm bm 
  guard $ checkDown fm ep 0 [(f, Neg, k)]

checkEF bm fm (_, _, ep, k, InfEqR nm) = do 
  guard $ onPath ep nm
  (Eq x y, Neg) <- cast $ HM.lookup nm bm 
  guard $ x == y

checkEF bm fm (_, _, ep, k, InfAx nm0 nm1) = do 
  guard $ onPath ep nm0
  guard $ onPath ep nm1
  (f, Pos) <- cast $ HM.lookup nm0 bm 
  (g, Neg) <- cast $ HM.lookup nm1 bm 
  guard $ f == g

checkEF bm fm (_, _, ep, k, InfFaR nm m) = do 
  guard $ onPath ep nm && k <= m
  (Fa vs f, Neg) <- cast $ HM.lookup nm bm 
  let (m', vxs) = varPars m vs
  let f' = substForm vxs f
  guard $ checkDown fm ep 0 [(f', Neg, m')]

checkEF bm fm (_, _, ep, k, InfFaL nm xs) = do 
  guard $ onPath ep nm
  (Fa vs f, Pos) <- cast $ HM.lookup nm bm 
  vxs <- zipM vs xs
  let f' = substForm vxs f
  guard $ checkDown fm ep 0 [(f', Pos, k)]

checkEF bm fm (_, _, _, _, i) = et $ "unsupported inf : " <> ppInf i

breakIff :: Form -> Form -> Dir -> Form
breakIff f g Obv = f ==> g
breakIff f g Rev = g ==> f

checkDown :: HM.Map EP (Form, Pol, Int) -> EP -> Int -> [(Form, Pol, Int)] -> Bool
checkDown fm ep k [] = True
checkDown fm ep k ((f, pl, m) : l) = do
  case HM.lookup (epFork k ep) fm of 
    Just (f', pl', m') -> f == f' && pl == pl' && m == m' && checkDown fm ep (k + 1) l
    _ -> False

checkJunct :: HM.Map EP (Form, Pol, Int) -> EP -> Int -> Pol -> Int -> [Form] -> Bool
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
    "elab" -> elaborate True args
    -- "elab0" -> elaborate False args
    "check" -> check args
    _ -> et "undefined command"