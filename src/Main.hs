{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Types
import Basic
import PP
import App
import Parse
import Check
import BT
import Sat
import Control.Monad as M (guard, foldM, foldM_, (>=>), mzero)
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative ( Alternative((<|>)) )
import System.Environment
import Data.List as L
    (filter, map, length, foldl, elem, all, any, concat, (\\), elemIndex, insert, sortBy, concatMap, unzip, nub, splitAt)
import Data.Text as T
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList, union, difference, disjoint )
import qualified Data.Text.Lazy as TL (toStrict, intercalate)
import Data.Text.IO as TIO
import Data.Text.Read as TR
import Data.Functor ((<&>))
-- import Data.HashMap.Lazy as HM ( HashMap, insert, lookup, empty, map, member, mapMaybe, toList, fromListWithKey, delete )
import Data.Map as HM ( Map, insert, lookup, empty, map, member, mapMaybe, toList, fromListWithKey, delete )
-- import Data.List.Unique (sortUniq)
import Debug.Trace (trace)
import Data.Maybe as MB ( isNothing )
-- import qualified GHC.Base as L
-- import Distribution.Simple.Program.GHC (GhcOptions(ghcOptStaticLib))

putAnForm :: AnForm -> IO ()
putAnForm i = Prelude.putStr $ ppAnForm i ++ "\n"

addHyp :: Hyps -> AnForm -> Hyps
addHyp (nsq, sq) (Af n f _) = (HM.insert n f nsq, S.insert f sq)

sortAfs :: [AnForm] -> [AnForm]
sortAfs = sortBy compareAfs

compareAfs :: AnForm -> AnForm -> Ordering
compareAfs (Af (m :> ms) _ _) (Af (n :> ns) _ _) =
  case compare m n of
    EQ -> compare (TR.decimal ms) (TR.decimal ns)
    other -> other
compareAfs _ _ = LT

tt :: Form
tt = And []

ff :: Form
ff = Or []

tfsimp :: Form -> Form
tfsimp (Rel r xs) = Rel r xs
tfsimp (Eq x y) = Eq x y
tfsimp (Not f) =
  case tfsimp f of
    And [] -> Or []
    Or [] -> And []
    g -> Not g
tfsimp (Or fs) =
  let gs = L.map tfsimp fs in
  if tt `elem` gs then tt
    else
      case L.filter (/= ff) gs of
        [g] -> g
        hs -> Or hs
tfsimp (And fs) =
  let gs = L.map tfsimp fs in
  if ff `elem` gs then ff
    else
      case L.filter (/= tt) gs of
        [g] -> g
        hs -> And hs
tfsimp (Fa vs f) =
  case tfsimp f of
    And [] -> And []
    Or [] -> Or []
    g -> Fa vs g
tfsimp (Ex vs f) =
  case tfsimp f of
    And [] -> And []
    Or [] -> Or []
    g -> Ex vs g
tfsimp (Imp f g) =
  case (tfsimp f, tfsimp g) of
    (Or [], _) -> tt
    (_, And []) -> tt
    (And [], g_) -> g_
    (f_, Or []) -> Not f_
    (f', g') -> Imp f' g'
tfsimp (Iff f g) =
  case (tfsimp f, tfsimp g) of
    (Or [], g') -> Not g'
    (f', Or []) -> Not f'
    (And [], g') -> g'
    (f', And []) -> f'
    (f', g') -> Iff f' g'

addIf :: Bool -> NSeq-> Text -> Form -> [NSeq]
addIf True c n f = return $ HM.insert n f c
addIf False _ n _ = error $ "Cannot add formula : " ++ unpack n

getHyp :: Text -> NSeq -> IO Form
getHyp n c =
  case HM.lookup n c of
    Just f -> return f
    _ -> MF.fail $ "Hypothesis does not exist : " ++ show n

appPrems :: Maybe [Term] -> [Goal] -> Ctx -> Maybe (Maybe [Term], [Goal], Ctx)
appPrems mts [] c = return (mts, [], c)
appPrems mts (gl : gls) c = do
  (mts', gls0, c') <- appPrem mts gl c
  (mts'', gls1, c'') <- appPrems mts' gls c'
  return (mts'', gls0 ++ gls1, c'')

-- zipRem :: [a] -> [b] -> Maybe ([(a, b)], [a])
-- zipRem xs [] = return ([], xs)
-- zipRem (x : xs) (y : ys) = do 
--   (xys, xs') <- zipRem xs ys 
--   return ((x, y) : xys, xs')
-- zipRem _ _ = nt

appPrem :: Maybe [Term] -> Goal -> Ctx -> Maybe (Maybe [Term], [Goal], Ctx)
appPrem mts gl@(Not (Not _), _) c = appNotNotL gl c >>= uncurry (appPrem mts)
appPrem mts gl@(Or _, _) c = do 
  (gls, c') <- appOrL gl c 
  appPrems mts gls c'
appPrem (Just xs) gl@(Fa vs _, _) c = do 
  let (xs0, xs1) = L.splitAt (L.length vs) xs 
  (gl', c') <- appFaL (Just xs0) gl c
  appPrem (Just xs1) gl' c'
appPrem Nothing gl@(Fa vs _, _) c = do 
  (gl', c') <- appFaL Nothing gl c
  appPrem Nothing gl' c'
appPrem mxs gl@(l, k) c = guard (isLit l) >> return (mxs, [gl], c)

  --case (appNotNotL gl c <|> appFaL Nothing gl c, appOrL gl c) of
  --  (Just (gl', c'), _) -> appPrem gl' c'
  --  (_, Just (gls, c')) -> appPrems gls c'
  --  _ -> return ([gl], c)
--
appConcs :: [Form] -> Int -> Ctx -> Maybe ([Form], Int, Ctx)
appConcs [] k c = return ([], k, c)
appConcs (f : fs) k c = do
  (fsl, k', c') <- appConc (f, k) c
  (fsr, k'', c'') <- appConcs fs k' c'
  return (fsl ++ fsr, k'', c'')

appConc :: Goal -> Ctx -> Maybe ([Form], Int, Ctx)
appConc (g, k) c =
  let gl = (g, k) in
  case (appNotNotR gl c, appFaR gl c, appOrR gl c) of
    (Just (gl', c'), _, _) ->    appConc gl' c'
    (_, Just (_, gl', c'), _) -> appConc gl' c'
    (_, _, Just (gs, k', c')) -> appConcs gs k' c'
    _ -> return ([g], k, c)

origAnd :: [Form] -> [Goal] -> Ctx -> BT Ctx
origAnd [] [] c = return c
origAnd fs ((g, k) : gls) c =
  ( do ((g', k'), gls') <- pluck ((g, k) : gls)
       (f', fs') <- pluck fs
       c' <- cast $ appAx Exact (f', g', k') c
       return (fs', gls', c') ) !>=
    (\ (fs', gls', c') -> origAnd fs' gls' c') $
    do (f, fs') <- pluck fs
       c' <- orig (f, g, k) c
       origAnd fs' gls c'
origAnd _ _ _ = []

origOr :: [Goal] -> [Form] -> Ctx -> BT Ctx
origOr [] [] c = return c
origOr ((f, k) : gls) gs c =
  ( do ((f', k'), gls') <- pluck ((f, k) : gls)
       (g', gs') <- pluck gs
       c' <- cast $ appAx Exact (f', g', k') c
       return (gls', gs', c') ) !>=
    (\ (gls', gs', c') -> origOr gls' gs' c') $
    do (g, gs') <- pluck gs
       c' <- orig (f, g, k) c
       origOr gls gs' c'
origOr _ _ _ = []

pairSolveAnd :: [Form] -> [Goal]  -> Ctx -> [Ctx]
pairSolveAnd [] [] c = return c
pairSolveAnd fs ((g, k) : gls) c =
  ( do (f, fs') <- pluck fs
       c' <- cast $ appAx Exact (f, g, k) c
       return (fs', c') ) !>=
    uncurry (`pairSolveAnd` gls) $
    do (f, fs') <- pluck fs
       c' <- pairSolve (f, g, k) c
       pairSolveAnd fs' gls c'
pairSolveAnd _ _ _ = []

pairSolveOr :: [Goal] -> [Form] -> Ctx -> [Ctx]
pairSolveOr [] [] c = return c
pairSolveOr ((f, k) : gls) gs c =
  ( do (g, gs') <- pluck gs
       c' <- cast $ appAx Exact (f, g, k) c
       return (gs', c') ) !>=
    uncurry (pairSolveOr gls) $
    do (g, gs') <- pluck gs
       c' <- pairSolve (f, g, k) c
       pairSolveOr gls gs' c'
pairSolveOr _ _ _ = []

isOr :: Form -> Bool
isOr (Or _) = True
isOr _ = False

isAnd :: Form -> Bool
isAnd (And _) = True
isAnd _ = False

updr :: PrvGoal -> Ctx -> BT Ctx
updr pg@(f, g, k) c =
  cast (appFaRL Same pg c) !>=
    uncurry updr $
    cast (appAx Lax pg c) <|>
    ( do ((f', k'), c') <- cast $ appIffLO (f, k) c
         updr (f', g, k') c' ) <|>
    ( do ((f', k'), c') <- cast $ appIffLR (f, k) c
         updr (f', g, k') c' )

flat :: PrvGoal -> Ctx -> BT Ctx
flat (f, g, k) c =
  let pg = (f, g, k) in
  cast (appNots pg c <|> appFaRL Same pg c <|> appExLR Same pg c) !>=
    uncurry flat $
    cast (appImpLR pg c <|> appIffRL pg c) !>=
      (\ (pg0, pg1, c') -> flat pg0 c' >>= flat pg1) $
      if isOr f
      then do let (gs, k', c') = appNestOrR (g, k) c
              let (gls, c'') = appNestOrL (f, k') c'
              pgs <- zipM gls gs <&> L.map (\ ((f_, k_), g_) -> (f_, g_, k_))
              foldM (flip flat) c'' pgs
      else
        if isAnd f
        then do let (fs, k', c') = appNestAndL (f, k) c
                let (gls, c'') = appNestAndR (g, k') c'
                pgs <- zipM fs gls <&> L.map (\ (f_, (g_, k_)) -> (f_, g_, k_))
                foldM (flip flat) c'' pgs
        else matchAtom Exact pg c

orig :: PrvGoal -> Ctx -> [Ctx]
orig (f, g, k) c =
  let pg = (f, g, k) in
  cast (appNots pg c <|> appFaRL Perm pg c <|> appExLR Perm pg c) !>=
    uncurry orig $
    cast (appImpLR pg c <|> appIffRL pg c) !>=
      (\ (pg0, pg1, c') -> orig pg0 c' >>= orig pg1) $
      cast (appOrR (g, k) c) !>=
        (\ (gs, k', c') -> do
           (gls, c'') <- cast $ appOrL (f, k') c'
           origOr gls gs c'' ) $
        cast (appAndL (f, k) c) !>=
          (\ (fs, k', c') -> do
             (gls, c'') <- cast $ appAndR (g, k') c'
             origAnd fs gls c'' ) $
          (matchAtom Exact pg c <!> matchAtom Pars pg c)

-- skolemize :: [Form] -> PrvGoal -> Ctx -> IO Ctx
-- skolemize hs pg@(f, g, k) c =
--   case ( appAx Pars pg c, 
--          appNotLR pg c <|> appFaRL Perm pg c <|> appExLR Perm pg c,
--          appImpLR pg c <|> appIffRL pg c, 
--          appOrR (g, k) c,
--          appAndL (f, k) c ) of 
--     (Just c', _, _, _, _) -> return c'
--     (_, Just (pg, c'), _, _, _) -> skolemize hs pg c'
--     (_, _, Just (pg0, pg1, c'), _, _) -> skolemize hs pg0 c' >>= skolemize hs pg1
--     (_, _, _, Just (gs, k', c'), _) -> do (gls, c'') <- cast $ appOrL (f, k') c'
--                                           glgs <- zipM gls gs
--                                           pgs <- mapM (\ ((f_, k_), g_) -> return (f_, g_, k_)) glgs
--                                           foldM (flip (skolemize hs)) c'' pgs 
--     (_, _, _, _, Just (fs, k', c')) -> do (gls, c'') <- cast $ appAndR (g, k') c'
--                                           fgls <- zipM fs gls
--                                           pgs <- mapM (\ (f_, (g_, k_)) -> return (f_, g_, k_)) fgls
--                                           foldM (flip (skolemize hs)) c'' pgs 
--     _ -> first (skolemizeAux pg c) (pluck hs)
skolemize :: [Form] -> PrvGoal -> Ctx -> IO Ctx
skolemize hs pg c =
  case ( appAx Pars pg c,
         appNotLR pg c <|> appFaRL Perm pg c <|> appExLR Perm pg c,
         appImpLR pg c <|> appIffRL pg c,
         appOrRL pg c <|> appAndLR pg c ) of
    (Just c', _, _, _) -> return c'
    (_, Just (pg, c'), _, _) -> skolemize hs pg c'
    (_, _, Just (pg0, pg1, c'), _) -> skolemize hs pg0 c' >>= skolemize hs pg1
    (_, _, _, Just (pgs, c')) -> foldM (flip (skolemize hs)) c' pgs
    _ -> first (skolemizeAux pg c) (pluck hs)

skolemizeAux :: PrvGoal -> Ctx -> (Form, [Form]) -> IO Ctx
skolemizeAux (f, g, k) c (h, hs) = do
  let ((h', k'), c0) = tryAppFaL (h, k) c
  ((f', m), (g', n), c1) <- cast $ appImpL (h', k') c0
  c2 <- cast $ appAx ParFvs (f, f', m) c1
  skolemize hs (g', g, n) c2

--   let pg = (f, g, k) in
--   cast (appAx Exact pg c) <!> (
--     cast (appNotLR pg c <|> appFaRL Perm pg c <|> appExLR Perm pg c) !>=
--       uncurry (skolemize hs) $
--       cast (appImpLR pg c <|> appIffRL pg c) !>=
--         (\ (pg0, pg1, c') -> skolemize hs pg0 c' >>= skolemize hs pg1) $
--         cast (appOrR (g, k) c) !>=
--           ( \ (gs, k', c') -> do
--               (gls, c'') <- cast $ appOrL (f, k') c'
--               glgs <- zipM gls gs
--               pgs <- mapM (\ ((f_, k_), g_) -> return (f_, g_, k_)) glgs
--               foldM (flip (skolemize hs)) c'' pgs ) $
--           cast (appAndL (f, k) c) !>=
--             ( \ (fs, k', c') -> do
--                 (gls, c'') <- cast $ appAndR (g, k') c'
--                 fgls <- zipM fs gls
--                 pgs <- mapM (\ (f_, (g_, k_)) -> return (f_, g_, k_)) fgls
--                 foldM (flip (skolemize hs)) c'' pgs ) $
--             cast (appAx Pars pg c) <|>
--             ( do (h, hs') <- pluck hs
--                  let ((h', k'), c0) = tryAppFaL (h, k) c
--                  ((f', m), (g', n), c1) <- cast $ appImpL (h', k') c0
--                  c2 <- cast $ appAx ParFvs (f, f', m) c1
--                  skolemize hs' (g', g, n) c2 )
--   )

blank :: Ctx
blank = Ctx {fresh = 1, binding = HM.empty, proofs = HM.empty }

isRdef :: Text -> Form -> Bool
isRdef r (Fa _ (Iff (Rel s _) _)) = r == s
isRdef r (Iff (Rel s _) _) = r == s
isRdef _ _ = False

checkElab :: Seq -> Form -> Elab -> IO ()
checkElab sq g (Plab p) = do
  verify 0 sq (S.singleton g) p
checkElab sq g (Nnf ex f) = do
  guard $ S.member f sq
  guard $ nnf ex f == g
checkElab sq g (Rdef r f p) = do
  guard $ isRdef r f
  verify 0 (S.singleton f) (S.singleton g) p
checkElab sq g (Tfe f) = do
  guard $ S.member f sq
  guard $ tfsimp f == g
checkElab sq g (AOC xs f p) = isAOC xs f
checkElab sq g (Lrats fs lrs) = do
  print "Verifying LRAT..."
  guard (L.all (`S.member` sq) fs)
  print "All premises present..."
  checkLrats (lratCtx 1 fs) lrs

lratCtx :: Int -> [Form] -> Map Int Form
lratCtx _ [] = HM.empty
lratCtx k (f : fs) = HM.insert k f $ lratCtx (k + 1) fs

negLit :: Form -> Form
negLit (Not f) = f
negLit f = Not f

negated :: Set Form -> Form -> Bool
negated fs (Not f) = f `elem` fs
negated fs f = Not f `elem` fs

breakSingleton :: [a] -> Maybe a
breakSingleton [x] = Just x
breakSingleton _ = Nothing

checkLrat :: Map Int Form -> Set Form -> [Int] -> IO ()
checkLrat _ _ [] = mzero
checkLrat fs fxs [k] = do
  ls <- cast $ HM.lookup k fs >>= formToLits
  guard $ L.all (negated fxs) ls
checkLrat fs fxs (k : ks) = do
  ls <- cast $ HM.lookup k fs >>= formToLits
  fx <- cast $ breakSingleton $ nub $ L.filter (not . negated fxs) ls
  checkLrat fs (S.insert fx fxs) ks

checkLrats :: Map Int Form -> [Lrat] -> IO ()
checkLrats _ [] = mzero
checkLrats fs [Add _ [] hs] = checkLrat fs S.empty hs
checkLrats _ [_] = mzero
checkLrats fs (Add k ls hs : lrs) = do
  let ns = S.fromList $ L.map negLit ls
  checkLrat fs ns hs
  checkLrats (HM.insert k (Or ls) fs) lrs
checkLrats fs (Del _ ks : lrs) = do
  checkLrats (L.foldl (flip HM.delete) fs ks) lrs

breakBv :: Term -> Maybe Text
breakBv (Bv v) = Just v
breakBv _ = Nothing

isSkolemTerm :: [Text] -> Term -> Bool
isSkolemTerm vs (Fun _ xs) =
  case mapM breakBv xs of
    Just ws -> sublist vs ws && sublist ws vs
    _ -> False
isSkolemTerm _ _ = False

isConstant :: Term -> Bool
isConstant (Fun _ []) = True
isConstant _ = False

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

elabIO :: Hyps -> AnForm -> IO Hyps
elabIO (nsq, sq) (Af n f a) = do
  print $ "Elaborating step = " <> n
  e <- elab nsq (Af n f a)
  checkElab sq f e
  return (HM.insert n f nsq, S.insert f sq)

appBndTerm :: BndMode -> Bnd -> Term -> Term
appBndTerm Mid b (Fv k) =
  case HM.lookup k b of
    Nothing -> Fv k
    Just x -> appBndTerm Mid b x
appBndTerm End b (Fv k) =
  case HM.lookup k b of
    Nothing -> Fun "" []
    Just x -> appBndTerm End b x
appBndTerm bm b (Fun f xs) = Fun f $ L.map (appBndTerm bm b) xs
appBndTerm _ _ x = x

appBndForm :: BndMode -> Bnd -> Form -> Form
appBndForm m b (Eq x y) = Eq (appBndTerm m b x) (appBndTerm m b y)
appBndForm m b (Rel r xs) = Rel r $ L.map (appBndTerm m b) xs
appBndForm m b (Not f) = Not $ appBndForm m b f
appBndForm m b (And fs) = And $ L.map (appBndForm m b) fs
appBndForm m b (Or fs) = Or $ L.map (appBndForm m b) fs
appBndForm m b (Imp f g) = Imp (appBndForm m b f) (appBndForm m b g)
appBndForm m b (Iff f g) = Iff (appBndForm m b f) (appBndForm m b g)
appBndForm m b (Fa vs f) = Fa vs $ appBndForm m b f
appBndForm m b (Ex vs f) = Ex vs $ appBndForm m b f

collectGoal :: Prfs -> Goal -> Maybe (Form, Prf)
collectGoal ps (f, k) = do
  p <- collectPrf ps k
  return (f, p)

collectEqGoal :: Prfs -> EqGoal -> Maybe (Term, Term, Prf)
collectEqGoal ps (x, y, k) = do
  p <- collectPrf ps k
  return (x, y, p)

collectPrf_ :: Prfs -> Prf_ -> Maybe Prf
collectPrf_ ps Sorry_ = return Sorry
collectPrf_ ps (Ax_ f) = return $ Ax f
collectPrf_ ps (EqR_ x) = return $ EqR x
collectPrf_ ps (EqC_ eg0 eg1) = do
  tp0 <- collectEqGoal ps eg0
  tp1 <- collectEqGoal ps eg1
  return $ EqC tp0 tp1
collectPrf_ ps (EqS_ x y) = Just $ EqS x y
collectPrf_ ps (EqT_ x y z) = Just $ EqT x y z
collectPrf_ ps (FunC_ f egs) = mapM (collectEqGoal ps) egs <&> FunC f
collectPrf_ ps (RelC_ f egs) = mapM (collectEqGoal ps) egs <&> RelC f
collectPrf_ ps (NotL_ f k) = collectPrf ps k <&> NotL f
collectPrf_ ps (NotR_ f k) = collectPrf ps k <&> NotR f
collectPrf_ ps (OrL_ gls) = mapM (collectGoal ps) gls <&> OrL
collectPrf_ ps (OrR_ fs gs k) = collectPrf ps k <&> OrR fs gs
collectPrf_ ps (AndL_ fs gs k) = collectPrf ps k <&> AndL fs gs
collectPrf_ ps (AndR_ gls) = mapM (collectGoal ps) gls <&> AndR
collectPrf_ ps (ImpL_ f g k m) = do
  p <- collectPrf ps k
  q <- collectPrf ps m
  return $ ImpL f g p q
collectPrf_ ps (IffR_ f g k m) = do
  p <- collectPrf ps k
  q <- collectPrf ps m
  return $ IffR f g p q
collectPrf_ ps (IffLO_ f g k) = collectPrf ps k <&> IffLO f g
collectPrf_ ps (IffLR_ f g k) = collectPrf ps k <&> IffLR f g
collectPrf_ ps (ImpRA_ f g k) = collectPrf ps k <&> ImpRA f g
collectPrf_ ps (ImpRC_ f g k) = collectPrf ps k <&> ImpRC f g
collectPrf_ ps (FaL_ vxs f k) = collectPrf ps k <&> FaL vxs f
collectPrf_ ps (FaR_ vs m f k) = collectPrf ps k <&> FaR vs m f
collectPrf_ ps (ExL_ vs m f k) = collectPrf ps k <&> ExL vs m f
collectPrf_ ps (ExR_ vxs f k) = collectPrf ps k <&> ExR vxs f
collectPrf_ ps (Cut_ f k m) = do
  p <- collectPrf ps k
  q <- collectPrf ps m
  return (Cut f p q)

collectPrf :: Prfs -> Int -> Maybe Prf
collectPrf ps k =
  case lookupM k ps of
    Nothing -> error ("Proof missing : " ++ show k)
    Just p_ -> collectPrf_ ps p_

appBndGoal :: Bnd -> (Form, Prf) -> (Form, Prf)
appBndGoal b (f, p) = (appBndForm End b f, appBndPrf b p)

appBndEqGoal :: Bnd -> (Term, Term, Prf) -> (Term, Term, Prf)
appBndEqGoal b (x, y, p) = (appBndTerm End b x, appBndTerm End b y, appBndPrf b p)

appBndVarTerm :: Bnd -> (Text, Term) -> (Text, Term)
appBndVarTerm b (v, x) = (v, appBndTerm End b x)

appBndPrf :: Bnd -> Prf -> Prf
appBndPrf b Sorry = Sorry
appBndPrf b (Ax f) = Ax $ appBndForm End b f
appBndPrf b (EqR x) = EqR $ appBndTerm End b x
appBndPrf b (EqC eg0 eg1) = EqC (appBndEqGoal b eg0) (appBndEqGoal b eg1)
appBndPrf b (EqS x y) = EqS (appBndTerm End b x) (appBndTerm End b y)
appBndPrf b (EqT x y z) = EqT (appBndTerm End b x) (appBndTerm End b y) (appBndTerm End b z)
appBndPrf b (FunC f egs) = FunC f $ L.map (appBndEqGoal b) egs
appBndPrf b (RelC r egs) = RelC r $ L.map (appBndEqGoal b) egs
appBndPrf b (NotL f p) = NotL (appBndForm End b f) (appBndPrf b p)
appBndPrf b (NotR f p) = NotR (appBndForm End b f) (appBndPrf b p)
appBndPrf b (OrL gls) = OrL $ L.map (appBndGoal b) gls
appBndPrf b (OrR fs gs p) = OrR (L.map (appBndForm End b) fs) (L.map (appBndForm End b) gs) (appBndPrf b p)
appBndPrf b (AndL fs gs p) = AndL (L.map (appBndForm End b) fs) (L.map (appBndForm End b) gs) (appBndPrf b p)
appBndPrf b (AndR gls) = AndR $ L.map (appBndGoal b) gls
appBndPrf b (ImpL f g p q) = ImpL (appBndForm End b f) (appBndForm End b g) (appBndPrf b p) (appBndPrf b q)
appBndPrf b (IffR f g p q) = IffR (appBndForm End b f) (appBndForm End b g) (appBndPrf b p) (appBndPrf b q)
appBndPrf b (IffLO f g p) = IffLO (appBndForm End b f) (appBndForm End b g) (appBndPrf b p)
appBndPrf b (IffLR f g p) = IffLR (appBndForm End b f) (appBndForm End b g) (appBndPrf b p)
appBndPrf b (ImpRA f g p) = ImpRA (appBndForm End b f) (appBndForm End b g) (appBndPrf b p)
appBndPrf b (ImpRC f g p) = ImpRC (appBndForm End b f) (appBndForm End b g) (appBndPrf b p)
appBndPrf b (FaL vxs f p) = FaL (L.map (appBndVarTerm b) vxs) (appBndForm End b f) (appBndPrf b p)
appBndPrf b (FaR vs k f p) = FaR vs k (appBndForm End b f) (appBndPrf b p)
appBndPrf b (ExL vs k f p) = ExL vs k (appBndForm End b f) (appBndPrf b p)
appBndPrf b (ExR vxs f p) = ExR (L.map (appBndVarTerm b) vxs) (appBndForm End b f) (appBndPrf b p)
appBndPrf b (Cut f p q) = Cut (appBndForm End b f) (appBndPrf b p) (appBndPrf b q)

extractPrf :: Ctx -> Maybe Prf
extractPrf c = do
  p <- collectPrf (proofs c) 0
  return $ appBndPrf (binding c) p

nnf :: Bool -> Form -> Form
nnf ex (Not (Rel r xs)) = Not $ Rel r xs
nnf ex (Not (Eq x y)) = Not $ Eq x y
nnf ex (Not (Not f)) = nnf ex f
nnf ex (Not (Or fs)) = And $ L.map (nnf ex . Not) fs
nnf ex (Not (And fs)) = Or $ L.map (nnf ex . Not) fs
nnf ex (Not (Imp f g)) = nnf ex (And [Not g, f])
nnf True  (Not (Iff f g)) = Not $ Iff (nnf True f) (nnf True g)
nnf False (Not (Iff f g)) =
  let nn = nnf False (Not g) in
  let nm = nnf False (Not f) in
  let pn = nnf False g in
  let pm = nnf False f in
  And [Or [nn, nm], Or [pn, pm]]
nnf ex (Not (Fa vs f)) = Ex vs (nnf ex $ Not f)
nnf ex (Not (Ex vs f)) = Fa vs (nnf ex $ Not f)
nnf ex (Or fs)  = Or  $ L.map (nnf ex) fs
nnf ex (And fs) = And $ L.map (nnf ex) fs
nnf ex (Imp f g) = Or [nnf ex g, nnf ex $ Not f]
nnf True  (Iff f g) = Iff (nnf True f) (nnf True g)
nnf False (Iff f g) =
  let nn = nnf False (Not g) in
  let nm = nnf False (Not f) in
  let pn = nnf False g in
  let pm = nnf False f in
  And [Or [pm, nn], Or [pn, nm]]
nnf ex (Fa vs f) = Fa vs $ nnf ex f
nnf ex (Ex vs f) = Ex vs $ nnf ex f
nnf ex (Rel r xs) = Rel r xs
nnf ex (Eq x y) = Eq x y

shuffle :: a -> a -> BT (a, a)
shuffle x y = return (x, y) <|> return (y, x)

factor :: Form -> Form -> IO Ctx
factor f g = do
  (gs, k, c) <- cast $ appConc (g, 0) blank
  (_, gls, c) <- cast $ appPrem nt (f, k) c
  cast $ litsMap Lax gls gs c

gFunFunctor :: Gterm -> Maybe Text
gFunFunctor (Gfun t []) = return t
gFunFunctor _ = Nothing

infer :: Text -> [Form] -> Form -> IO Ctx
infer "equality_resolution" [f] g = factor f g
infer "equality_factoring" [f] g = eqf f g
infer "subsumption_resolution" [f, g] h = cast $ resolve f g h
infer "superposition" [f, g] h         = superposition f g h
infer "forward_demodulation" [f, g] h  = superposition f g h
infer "backward_demodulation" [f, g] h = superposition f g h
infer "resolution" [f, g] h = cast $ resolve f g h
infer "rectify" [f] g            = cast $ pairSolve (f, g, 0) blank
infer "negated_conjecture" [f] g = cast $ appAx Exact (f, g, 0) blank
infer "skolemisation" (f : fs) g = skolemize fs (f, g, 0) blank
infer "cnf_transformation" [f] g = cast $ cnfTrans f g
infer "factoring" [f] g = factor f g
infer "trivial_inequality_removal" [f] g = factor f g
infer "duplicate_literal_removal" [f] g  = factor f g
infer "unused_predicate_definition_removal" [f] g = cast $ updr (f, g, 0) blank
infer "avatar_split_clause" (f : fs) g   = avatarSplit fs f g
infer "avatar_component_clause" [f] g = cast $ avatarComp f g
infer "avatar_contradiction_clause" [f] g = avatarContra f g
infer "flattening" [f] g = cast $ flat (f, g, 0) blank
infer "definition_unfolding" (f : fs) g = unfold fs f g
infer "definition_folding" (f : fs) g = defFold fs f g
infer "pure_predicate_removal" [f] g = ppr f g
infer r fs g = MF.fail "unimplemented"

defFold :: [Form] -> Form -> Form -> IO Ctx
defFold ds f g = do
  mark 0
  fd <- findFD ds f g
  mark 1
  (k, m, c) <- cast $ appCut Nothing (f <=> g, 0) blank
  mark 2
  c' <- useFD fd f g k c
  mark 3
  ((_, m'), c'') <- cast $ appIffLO (f <=> g, m) c'
  mark 4
  cast $ appMP (f ==> g) (f, g, m') c''


findFDRW :: Form -> Form -> Form -> IO FD
findFDRW f g (Iff p q) =
  (guard (f == p && g == q) >> return (FDRW Obv)) <|>
  (guard (f == q && g == p) >> return (FDRW Rev))
findFDRW _ _ _ = mzero

findFD :: [Form] -> Form -> Form -> IO FD
findFD ds (Not f) (Not g) = findFD ds f g <&> FDNot
findFD ds (Iff f0 f1) (Iff g0 g1) = do
  d0 <- findFD ds f0 g0
  d1 <- findFD ds f1 g1
  return $ FDIff d0 d1
findFD ds (And fs) (And gs) = do
  fgs <- zipM fs gs
  fds <- mapM (uncurry $ findFD ds) fgs
  return $ FDAnd fds
findFD ds f g =
  (guard (f == g) >> return FDId) <|>
  first (findFDRW f g) ds

data FD =
    FDId
  | FDNot FD
  | FDRW Dir
  | FDIff FD FD
  | FDAnd [FD]
  | FDOr [FD]
  deriving (Show)

data Diff =
    Id
  | OrDiff [Maybe Diff]
  | AndDiff [Maybe Diff]
  | QntDiff [Text] Diff
  | ImpDiff Diff Diff
  | AnteD Diff
  | ConsDiff Diff
  | NotDiff Diff
  | TransDiff Diff Form Diff
  | CleanDiff
  deriving (Show)

findDiff' :: Set Text -> Form -> Form -> BT Diff
findDiff' rs f g = do
  (g', d) <- findDiff rs Obv f
  guard (g == g')
  return d

revDir :: Dir -> Dir
revDir Obv = Rev
revDir Rev = Obv

formPreds :: Form -> Set Text
formPreds (Eq _ _) = S.empty
formPreds (Rel r _) = S.singleton r
formPreds (Not f) = formPreds f
formPreds (Or fs) = L.foldl S.union S.empty $ L.map formPreds fs
formPreds (And fs) = L.foldl S.union S.empty $ L.map formPreds fs
formPreds (Imp f g) = S.union (formPreds f) (formPreds g)
formPreds (Iff f g) = S.union (formPreds f) (formPreds g)
formPreds (Fa _ f) = formPreds f
formPreds (Ex _ f) = formPreds f

diffPreds :: Form -> Form -> Set Text
diffPreds f g = S.difference (formPreds f) (formPreds g)

findDiff :: Set Text -> Dir -> Form -> BT (Form, Diff)
findDiff rs dr (Not f) = do
  (f', d) <- findDiff rs (revDir dr) f
  return (Not f', NotDiff d)
findDiff rs dr (Imp f g) = do
  (f', df) <- findDiff rs (revDir dr) f
  (g', dg) <- findDiff rs dr g
  return (f' ==> g', ImpDiff df dg) <|>
    (guard (dr == Rev && removeTarget rs f') >> return (g', ConsDiff dg)) <|>
    (guard (dr == Rev && removeTarget rs g') >> return (Not f', AnteD df))
findDiff rs dr (Fa vs f) = do
  (f', d) <- findDiff rs dr f
  return (Fa vs f', QntDiff vs d)
findDiff rs dr (Ex vs f) = do
  (f', d) <- findDiff rs dr f
  return (Ex vs f', QntDiff vs d)
findDiff rs Obv (Or fs) = do
  (fs', ds) <- mapM (findDiff rs Obv) fs <&> L.unzip
  let f' = cleanOr fs'
  return (f', TransDiff (OrDiff $ L.map Just ds) (Or fs') CleanDiff)
findDiff rs Rev (Or fs) = do
  (fs', mds) <- mapM (findDiff rs Rev) fs >>= removeByPreds rs
  let f' = cleanOr fs'
  return (f', TransDiff (OrDiff mds) (Or fs') CleanDiff)
findDiff rs Obv (And fs) = do
  (fs', mds) <- mapM (findDiff rs Obv) fs >>= removeByPreds rs
  let f' = cleanAnd fs'
  return (f', TransDiff (AndDiff mds) (And fs') CleanDiff)
findDiff rs Rev (And fs) = do
  (fs', ds) <- mapM (findDiff rs Rev) fs <&> L.unzip
  let f' = cleanAnd fs'
  return (f', TransDiff (AndDiff $ L.map Just ds) (And fs') CleanDiff)
findDiff _ _ f = return (f, Id)

removeByPreds :: Set Text -> [(Form, Diff)] -> BT ([Form], [Maybe Diff])
removeByPreds rs [] = return ([], [])
removeByPreds rs ((f, d) : fds) = do
  (fs, mds) <- removeByPreds rs fds
  return (f : fs, Just d : mds) <|>
    (guard (removeTarget rs f) >> return (fs, Nothing : mds))

removeTarget :: Set Text -> Form -> Bool
removeTarget rs f = not $ S.disjoint rs $ formPreds f

cleanAnd :: [Form] -> Form
cleanAnd fs =
  if ff `elem` fs
  then ff
  else case cleanAnd' fs of
         [f] -> f
         fs' -> And fs'

cleanOr :: [Form] -> Form
cleanOr fs =
  if tt `elem` fs
  then tt
  else case cleanOr' fs of
         [f] -> f
         fs' -> Or fs'

cleanAnd' :: [Form] -> [Form]
cleanAnd' [] = []
cleanAnd' (Or [] : fs) = cleanAnd' fs
cleanAnd' (And fs : gs) = fs ++ cleanAnd' gs
cleanAnd' (f : fs) = f : cleanAnd' fs

cleanOr' :: [Form] -> [Form]
cleanOr' [] = []
cleanOr' (And [] : fs) = cleanOr' fs
cleanOr' (Or fs : gs) = fs ++ cleanOr' gs
cleanOr' (f : fs) = f : cleanOr' fs

ppr :: Form -> Form -> IO Ctx
ppr f g = do
  let rs = diffPreds f g
  d <- cast $ findDiff' rs f g
  useDiff Obv d (f, g, 0) blank

substDiff :: [(Text, Term)] -> Diff -> Diff
substDiff _ Id = Id
substDiff _ CleanDiff = CleanDiff
substDiff vxs (NotDiff d) = NotDiff $ substDiff vxs d
substDiff vxs (OrDiff mds) = OrDiff $ L.map (fmap $ substDiff vxs) mds
substDiff vxs (AndDiff mds) = AndDiff $ L.map (fmap $ substDiff vxs) mds
substDiff vxs (ImpDiff d0 d1) = ImpDiff (substDiff vxs d0) (substDiff vxs d1)
substDiff vxs (AnteD d) = AnteD (substDiff vxs d)
substDiff vxs (ConsDiff d) = ConsDiff (substDiff vxs d)
substDiff vxs (TransDiff d0 f d1) = TransDiff (substDiff vxs d0) (substForm vxs f) (substDiff vxs d1)
substDiff vxs (QntDiff vs d) =
  let vxs' = L.filter (\ (v, _) -> v `notElem` vs) vxs in
  QntDiff vs $ substDiff vxs' d

useFDobv :: Ctx -> (FD, PrvGoal) -> IO Ctx
useFDobv c0 (fd, (f, g, k)) = do 
  (m, n, c1) <- cast $ appCut nt (f <=> g, k) c0
  c2 <- useFD fd f g m c1
  ((fg, n'), c3) <- cast $ appIffLO (f <=> g, n) c2
  cast $ appMP fg (f, g, n') c3

useFDrev :: Ctx -> (FD, PrvGoal) -> IO Ctx
useFDrev c0 (fd, (g, f, k)) = do 
  (m, n, c1) <- cast $ appCut nt (f <=> g, k) c0
  c2 <- useFD fd f g m c1
  ((gf, n'), c3) <- cast $ appIffLR (f <=> g, n) c2
  cast $ appMP gf (g, f, n') c3

useFD :: FD -> Form -> Form -> Int -> Ctx -> IO Ctx
useFD FDId f g k c = do
  (gl0, gl1, c0) <- cast $ appIffR (f <=> g, k) c
  c1 <- cast $ appImpRAC gl0 c0 >>= uncurry (appAx Exact)
  cast $ appImpRAC gl1 c1 >>= uncurry (appAx Exact)
useFD (FDNot d) (Not f) (Not g) k c = do
  (k', k'', c0) <- cast $ appCut Nothing (f <=> g, k) c
  ((nfng, m0), (ngnf, n0), c1) <-  cast $ appIffR (Not f <=> Not g, k'') c0
  ((fg, n1), c2) <- cast $ appIffLO (f <=> g, n0) c1
  c3 <- cast $ appContra (fg, ngnf, n1) c2
  ((gf, m1), c4) <- cast $ appIffLR (f <=> g, m0) c3
  c5 <- cast $ appContra (gf, nfng, m1) c4
  useFD d f g k' c5
useFD (FDNot _) _ _ _ _ = mzero
useFD (FDIff d0 d1) (Iff f0 f1) (Iff g0 g1) k0 z0 = do
  (m0, k1, z1) <- cast $ appCut Nothing (f0 <=> g0, k0) z0 -- k1 : f0 <=> g0 |- 
  z2 <- useFD d0 f0 g0 m0 z1
  (m1, k2, z3) <- cast $ appCut Nothing (f1 <=> g1, k1) z2
  z4 <- useFD d1 f1 g1 m1 z3 -- k2 : f0 <=> g0, f1 <=> g1 |- (f0 <=> f1) <=> (g0 <=> g1)
  (m2, k3, z5) <- cast $ appCut Nothing (g0 <=> f0, k2) z4
  z6 <- cast $ appIffSym (f0 <=> g0) (g0 <=> f0) m2 z5
  (m3, k4, z7) <- cast $ appCut Nothing (g1 <=> f1, k3) z6
  z8 <- cast $ appIffSym (f1 <=> g1) (g1 <=> f1) m3 z7
  ((_, m4), (_, k5), z9) <- cast $ appIffR ((f0 <=> f1) <=> (g0 <=> g1), k4) z8
  ((_, m5), z10) <- cast $ appImpRA ((f0 <=> f1) ==> (g0 <=> g1), m4) z9
  ((_, m6), z11) <- cast $ appImpRC ((f0 <=> f1) ==> (g0 <=> g1), m5) z10
  z12 <- cast $ appIffTrans2 (g0 <=> f0) (f0 <=> f1) (f1 <=> g1) (g0 <=> g1) m6 z11
  ((_, k6), z13) <- cast $ appImpRA ((g0 <=> g1) ==> (f0 <=> f1), k5) z12
  ((_, k7), z14) <- cast $ appImpRC ((g0 <=> g1) ==> (f0 <=> f1), k6) z13
  cast $ appIffTrans2 (f0 <=> g0) (g0 <=> g1) (g1 <=> f1) (f0 <=> f1) k7 z14
useFD (FDAnd ds) f@(And fs) g@(And gs) k0 c0 = do 
  (o0, o1, c1) <- cast $ appIffR (f <=> g, k0) c0
  (os0, c2) <- cast $ appImpR o0 c1 >>= uncurry appAndLR
  dos0 <- zipM ds os0
  c3 <- foldM useFDobv c2 dos0
  (os1, c4) <- cast $ appImpR o1 c3 >>= uncurry appAndLR
  dos1 <- zipM ds os1
  foldM useFDrev c4 dos1
useFD (FDAnd _) _ _ _ _ = mzero 
useFD (FDOr ds) f@(Or fs) g@(Or gs) k0 c0 = do 
  (o0, o1, c1) <- cast $ appIffR (f <=> g, k0) c0
  (os0, c2) <- cast $ appImpR o0 c1 >>= uncurry appOrRL
  dos0 <- zipM ds os0
  c3 <- foldM useFDobv c2 dos0
  (os1, c4) <- cast $ appImpR o1 c3 >>= uncurry appOrRL
  dos1 <- zipM ds os1
  foldM useFDrev c4 dos1
useFD (FDOr _) _ _ _ _ = mzero 
useFD (FDIff _ _) _ _ _ _ = mzero
useFD (FDRW Obv) f g k c = cast $ appAx Exact (f <=> g, f <=> g, k) c
useFD (FDRW Rev) f g k c = cast $ appIffSym (g <=> f) (f <=> g) k c

useDiff :: Dir -> Diff -> PrvGoal -> Ctx -> IO Ctx
useDiff dr Id pg c = cast $ appAx Exact pg c
useDiff dr (NotDiff d) pg c = cast (appNotLR pg c) >>= uncurry (useDiff (revDir dr) d)
useDiff Obv (TransDiff d0 g d1) (f, h, k) c0 = do
  (k0, k1, c1) <- cast $ appCut nt (g, k) c0
  useDiff Obv d0 (f, g, k0) c1 >>= useDiff Obv d1 (g, h, k1)
useDiff Rev (TransDiff d0 g d1) (h, f, k) c0 = do
  (k0, k1, c1) <- cast $ appCut nt (g, k) c0
  useDiff Rev d0 (g, f, k1) c1 >>= useDiff Rev d1 (h, g, k0)
useDiff Rev (AnteD d) (Not f, Imp g h, k) c = do
  ((_, k'), c') <- cast $ appNotL (Not f, k) c
  ((_, k''), c'') <- cast $ appImpRA (Imp g h, k') c'
  useDiff Obv d (g, f, k'') c''
useDiff Rev (ConsDiff d) (f, Imp g h, k) c = do
  ((_, k'), c') <- cast $ appImpRC (Imp g h, k) c
  useDiff Rev d (f, h, k') c'
useDiff _ (AnteD _) _ _ = mzero
useDiff _ (ConsDiff _) _ _ = mzero
useDiff dr (ImpDiff d0 d1) pg c = do
  (pg0, pg1, c') <- cast $ appImpLR pg c
  useDiff (revDir dr) d0 pg0 c' >>= useDiff dr d1 pg1
useDiff Obv (OrDiff mds) (f, g, k) c = do
  let (gs, k', c') = tryAppOrR (g, k) c
  let (fks, c'') = tryAppOrL (f, k') c'
  fkmds <- zipM fks mds
  let fkds = mapFilter (\ ((f_, k_), md_) -> md_ >>= \ d_ -> return (f_, k_, d_) ) fkmds
  gfkds <- zipM gs fkds
  let dpgs = L.map (\ (g_, (f_, k_, d_)) -> (d_, (f_, g_, k_))) gfkds
  foldM (\ c_ (d_, pg_) -> useDiff Obv d_ pg_ c_) c'' dpgs
useDiff Rev (OrDiff mds) (f, g, k) c = do
  let (gs, k', c') = tryAppOrR (g, k) c
  let (fks, c'') = tryAppOrL (f, k') c'
  mdgs <- zipM mds gs
  let dgs = mapFilter (\ (md_, g_) -> md_ >>= \ d_ -> return (d_, g_) ) mdgs
  fkdgs <- zipM fks dgs
  let dpgs = L.map (\ ((f_, k_), (d_, g_)) -> (d_, (f_, g_, k_))) fkdgs
  foldM (\ c_ (d_, pg_) -> useDiff Rev d_ pg_ c_) c'' dpgs
useDiff Obv (AndDiff mds) (f, g, k) c = do
  let (fs, k', c') = tryAppAndL (f, k) c
  let (gks, c'') = tryAppAndR (g, k') c'
  mdfs <- zipM mds fs
  let dfs = mapFilter (\ (md_, f_) -> md_ >>= \ d_ -> return (d_, f_) ) mdfs
  gkdfs <- zipM gks dfs
  let dpgs = L.map (\ ((g_, k_), (d_, f_)) -> (d_, (f_, g_, k_))) gkdfs
  foldM (\ c_ (d_, pg_) -> useDiff Obv d_ pg_ c_) c'' dpgs
useDiff Rev (AndDiff mds) (f, g, k) c = do
  let (fs, k', c') = tryAppAndL (f, k) c
  let (gks, c'') = tryAppAndR (g, k') c'
  gkmds <- zipM gks mds
  let gkds = mapFilter (\ ((g_, k_), md_) -> md_ >>= \ d_ -> return (g_, k_, d_) ) gkmds
  fgkds <- zipM fs gkds
  let dpgs = L.map (\ (f_, (g_, k_, d_)) -> (d_, (f_, g_, k_))) fgkds
  foldM (\ c_ (d_, pg_) -> useDiff Rev d_ pg_ c_) c'' dpgs
useDiff dr (QntDiff us d) (Fa vs f, Fa ws g, k) c = do
  (xs, (g', k'), c') <- cast $ appFaR (Fa ws g, k) c
  ((f', k''), c'') <- cast $ appFaL (Just xs) (Fa vs f, k') c'
  uxs <- zipM us xs
  let d' = substDiff uxs d
  useDiff dr d' (f', g', k'') c''
useDiff dr (QntDiff us d) (Ex vs f, Ex ws g, k) c = do
  (xs, (f', k'), c') <- cast $ appExL (Ex vs f, k) c
  ((g', k''), c'') <- cast $ appExR (Just xs) (Ex ws g, k') c'
  uxs <- zipM us xs
  let d' = substDiff uxs d
  useDiff dr d' (f', g', k'') c''
useDiff _ (QntDiff _ _) _ _ = mzero
useDiff Obv CleanDiff (Or fs, g, k) c = do
  let (gs, k', c') = appNestOrR (g, k) c
  let (gls, c'') = appNestOrL (Or fs, k') c'
  cast $ litsMap Exact gls gs c''
useDiff Rev CleanDiff (f, Or gs, k) c = do
  let (_, k', c') = appNestOrR (Or gs, k) c
  let (gls, c'') = appNestOrL (f, k') c'
  cast $ litsMap Exact gls gs c''
useDiff Obv CleanDiff (And fs, g, k) c = do
  let (_, k', c') = appNestAndL (And fs, k) c
  let (gls, c'') = appNestAndR (g, k') c'
  cast $ conjMap fs gls c''
useDiff Rev CleanDiff (f, And gs, k) c = do
  let (fs, k', c') = appNestAndL (f, k) c
  let (gls, c'') = appNestAndR (And gs, k') c'
  cast $ conjMap fs gls c''
useDiff _ CleanDiff _ _ = mzero


-- ppr :: Dir -> PrvGoal -> Ctx -> BT Ctx
-- ppr dr pg@(Not _, Not _, _) c = do 
--   (pg', c') <- cast $ appNotLR pg c
--   ppr (revDir dr) pg' c'
-- ppr dr pg@(Imp _ _, Imp _ _, _) c = do 
--   (pg0, pg1, c') <- cast $ appImpLR pg c
--   ppr (revDir dr) pg0 c' >>= ppr dr pg1
-- ppr dr pg@(Fa _ _, Fa _ _, _) c = do 
--   (pg', c') <- cast $ appFaRL Same pg c
--   ppr dr pg' c'
-- ppr dr pg@(Ex _ _, Ex _ _, _) c = do 
--   (pg', c') <- cast $ appExLR Same pg c
--   ppr dr pg' c'
-- ppr _ pg c = cast $ appAx Exact pg c



markBT :: Int -> BT ()
markBT k = trace (unpack $ "Marking checkpoint " <> ppInt k <> "\n") (return ())

avatarContra :: Form -> Form -> IO Ctx
avatarContra f g = do
  (gs, k, c) <- cast $ appConc (g, 0) blank
  (_, gls, c') <- cast $ appPrem Nothing (f, k) c
  cast $ litsMap Exact gls gs c'

avatarComp :: Form -> Form -> BT Ctx
avatarComp f g = do
  (gs, k0, c0) <- cast $ appOrR (g, 0) blank
  (Not g, [conc]) <- pluck gs
  ((_, k1), c1) <- cast $ appNotR (Not g, k0) c0
  (prem, k2, c2) <- cast (rwlo f g k1 c1) <|> cast (rwlr f g k1 c1)
  pairSolve (prem, conc, k2) c2

unfold :: [Form] -> Form -> Form -> IO Ctx
unfold ds f g = do
  (gs, k, c0) <- cast $ appConc (g, 0) blank
  (_, gls, c1) <- cast $ appPrem Nothing (f, k) c0
  cast $ unfolds ds gls gs c1

unfolds :: [Form] -> [Goal] -> [Form] -> Ctx -> BT Ctx
unfolds _ [] _ c = return c
unfolds ds ((f, k) : gls) gs c = do
  trace (unpack $ "unfold " <> ppForm f <> " ?\n") (return ())
  (g, gs') <- pluck gs
  c' <- unfoldLit ds (f, g, k) c
  trace (unpack $ "unfolded " <> ppForm f <> " !\n") (return ())
  unfolds ds gls gs' c'

unfoldLit :: [Form] -> PrvGoal -> Ctx -> BT Ctx
unfoldLit ds pg c = do
  (pg', c') <- cast (appNotLR pg c <|> return (pg, c))
  unfoldAtom ds pg' c'

notFun :: Term -> Bool
notFun (Fun _ _ ) = False
notFun _ = True

unfoldEqn :: ([Form], Ctx) -> EqGoal -> BT ([Form], Ctx)
unfoldEqn (ds, c) eg_@(x_, z_, k) =
  let x = appBndTerm Mid (binding c) x_ in
  let z = appBndTerm Mid (binding c) z_ in
  let eg = (x, z, k) in
  ( do guard $ notFun x || notFun z
       c' <- cast $ appEqR Lax (Eq x z, k) c
       return (ds, c') ) <|>
  ( do -- traceBT $ "Try break : " <> ppEqGoal eg
       (egs, c') <- cast $ appFunC eg c
       foldM unfoldEqn (ds, c') egs ) <|>
  ( do (d, ds') <- pluck ds
       ((d', k1), c1) <- uncurry tryFlipEqL $ tryAppFaL (d, k) c
       ((_, _, k2), eg1, y, c2) <- cast $ interEq (x, z, k1) c1
       c3 <- cast $ appAx Lax (d', Eq x y, k2) c2
       unfoldEqn (ds', c3) eg1 )

unfoldAtom :: [Form] -> PrvGoal -> Ctx -> BT Ctx
unfoldAtom ds (Eq x0 x1, Eq y0 y1, k) c = do
  (eg0, eg1, c') <- ( do ((_, k_), c_) <- cast $ flipEqR (Eq y0 y1, k) c
                         cast $ appEqC (Eq x0 x1, Eq y1 y0, k_) c_ ) <|>
                    cast (appEqC (Eq x0 x1, Eq y0 y1, k) c)
  foldM unfoldEqn (ds, c') [eg0, eg1] <&> snd
unfoldAtom ds pg c = do
  (egs, c') <- cast $ appRelC pg c
  foldM unfoldEqn (ds, c') egs <&> snd

expandResult :: Form -> Form -> Ctx -> Maybe (Form, Ctx)
expandResult (Iff f g) h c =
  ( do b <- uniForm Lax (binding c) (f, h)
       return (g, c {binding = b}) ) <|>
  ( do b <- uniForm Lax (binding c) (g, h)
       return (f, c {binding = b}) )
expandResult _ _ _ = Nothing

expandSplit :: [Form] -> ([Form], Int, Ctx) -> Form -> BT ([Form], Int, Ctx)
expandSplit ds (hs, k, c0) (Not g) = do
  d <- pick ds
  (f, c1) <- cast $ expandResult d g c0
  (m, n0, c2) <- cast $ appCut nt (Not f, k) c1
  ((_, _, n1), c3) <- cast $ appNotLR (Not f, Not g, n0) c2
  ((gf, n2), c4) <- cast (appIffLO (d, n1) c3) <|> cast (appIffLR (d, n1) c3)
  c5 <- cast $ appMP gf (g, f, n2) c4
  return (Not f : hs, m, c5)
expandSplit ds (hs, k, c) g = do
  d <- pick ds
  (g', k', c') <- cast $ rwro d g k c <|> rwrr d g k c
  (hs', k'', c'') <- cast $ appConc (g', k') c'
  return (hs' ++ hs, k'', c'')

avatarSplit :: [Form] -> Form -> Form -> IO Ctx
avatarSplit ds f g = do
  (gs, k, c) <- cast $ appConc (g, 0) blank
  (hss, k', c') <- cast $ foldM (expandSplit ds) ([], k, c) gs
  (_, gls, c'') <- cast $ appPrem Nothing (f, k') c'
  cast $ litsMap Pars gls (gs ++ hss) c''

breakFv :: Term -> BT Int
breakFv (Fv k) = return k
breakFv _ = []

instTerm :: Bnd -> (Term, Term) -> BT Bnd
instTerm b (Fv k, x) =
  case HM.lookup k b of
    Nothing -> return $ HM.insert k x b
    Just y -> guard (x == y) >> return b
instTerm b (Fun f xs, Fun g ys) =
  guard (f == g) >> zipM xs ys >>= foldM instTerm b
instTerm b (x, y) = guard (x == y) >> return b

instForm :: Bnd -> (Form , Form) -> BT Bnd
instForm b (Eq x0 x1, Eq y0 y1) = foldM instTerm b [(x0, y0), (x1, y1)] <|> foldM instTerm b [(x0, y1), (x1, y0)]
instForm b (Rel r xs, Rel s ys) = guard (r == s) >> zipM xs ys >>= foldM instTerm b
instForm b (Not f, Not g) = instForm b (f, g)
instForm b (And fs, And gs) = zipM fs gs >>= foldM instForm b
instForm b (Or fs, Or gs)   = zipM fs gs >>= foldM instForm b
instForm b (Imp f0 f1, Imp g0 g1) = foldM instForm b [(f0, g0), (f1, g1)]
instForm b (Iff f0 f1, Imp g0 g1) = foldM instForm b [(f0, g0), (f1, g1)]
instForm b (Fa vs f, Fa ws g) = guard (isPerm vs ws) >> instForm b (f, g)
instForm b (Ex vs f, Ex ws g) = guard (isPerm vs ws) >> instForm b (f, g)
instForm _ _ = []

normalizeAOC :: Form -> IO ([Term], Form)
normalizeAOC (Fa vs (Imp (Ex ws f) g)) = do
  let (_, xs) = listFvs 0 ws
  wxs <- zipM ws xs
  let f' = substForm wxs f
  ks <- cast $ mapM breakFv xs
  b <- cast $ instForm HM.empty (f', g)
  xs' <- mapM (`lookupM` b) ks
  return (xs', Fa vs (Imp (Ex ws f) (appBndForm End b f')))
normalizeAOC (Imp (Ex ws f) g) = do
  let (_, xs) = listFvs 0 ws
  wxs <- zipM ws xs
  let f' = substForm wxs f
  ks <- cast $ mapM breakFv xs
  b <- cast $ instForm HM.empty (f', g)
  xs' <- mapM (`lookupM` b) ks
  return (xs', Imp (Ex ws f) (appBndForm End b f'))
normalizeAOC _ = mzero

breakGfun :: Gterm -> BT Text
breakGfun (Gfun t []) = return t
breakGfun _ = []

mkRdef :: Text -> Form -> Maybe Form
mkRdef r (Fa vs g) = do
  f <- mkRdef r g
  return (Fa vs f)
mkRdef r (Iff (Rel s xs) f) = guard (r == s) >> return (Iff (Rel s xs) f)
mkRdef r (Iff f (Rel s xs)) = guard (r == s) >> return (Iff (Rel s xs) f)
mkRdef _ _ = Nothing

proveRdef :: PrvGoal -> Ctx -> Maybe Ctx
proveRdef pg@(f, g, k) c0 =
  case appFaRL Same pg c0 of
    Just (pg', c') -> proveRdef pg' c'
    _ ->
      appAx Pars pg c0 <|>
      do ((go, ko), (gr, kr), c1) <- appIffR (g, k) c0
         ((fr, ko'), c2) <- appIffLR (f, ko) c1
         c3 <- appAx Pars (fr, go, ko') c2
         ((fo, kr'), c4) <- appIffLO (f, kr) c3
         appAx Pars (fo, gr, kr') c4

relDef :: Text -> Form -> IO Elab
relDef r g = do
  f <- cast $ mkRdef r g
  c <- cast $ proveRdef (f, g, 0) blank
  p <- cast $ extractPrf c
  return $ Rdef r f p

elab :: NSeq -> AnForm -> IO Elab
elab s (Af n g (Just (Gfun "file" [_, Gfun m []]))) = do
  f <- getHyp m s
  c <- cast (orig (f, g, 0) blank)
  p <- cast $ extractPrf c
  return $ Plab p

elab _ (Af n g (Just (Gfun "introduced" [Gfun "predicate_definition_introduction" [],Glist [Gfun "new_symbols" [Gfun "naming" [],Glist [Gfun r []]]]]))) = relDef r g
elab _ (Af n g (Just (Gfun "introduced" [Gfun "avatar_definition" [], Glist [Gfun "new_symbols" [Gfun "naming" [], Glist [Gfun r []]]]]))) = relDef r g
elab s (Af n g (Just (Gfun "introduced" [Gfun "choice_axiom" [], Glist []]))) = do
  (xs, f) <- normalizeAOC g
  c <- cast$ pairSolve (f, g, 0) blank
  p <- cast $ extractPrf c
  return $ AOC xs f p
elab c (Af m g (Just (Gfun "inference" [Gfun "true_and_false_elimination" [], _, Glist [Gfun n []]]))) =
  do f <- getHyp n c
     -- guard (tfsimp f == g)
     -- return $ Tfe f
     if tfsimp f == g 
     then return $ Tfe f
     else error (unpack $ "TF-fail:\n" <> ppForm (tfsimp f) <> "\n" <> ppForm g)
elab c (Af m g (Just (Gfun "inference" [Gfun "ennf_transformation" [], _, Glist [Gfun n []]]))) =
  do f <- getHyp n c
     return $ Nnf True f
elab c (Af m g (Just (Gfun "inference" [Gfun "nnf_transformation" [], _, Glist [Gfun n []]]))) =
  do f <- getHyp n c
     return $ Nnf False f
elab s (Af _ g (Just (Gfun "inference" [Gfun "avatar_sat_refutation" [], _, Glist l]))) = do
  fs <- cast (mapM gFunFunctor l) >>= mapM (`lookupM` s)
  sat fs <&> Lrats fs
elab s (Af _ g (Just (Gfun "inference" [Gfun r [], _, Glist l]))) = do
  fs <- cast (mapM gFunFunctor l) >>= mapM (`lookupM` s)
  c <- infer r fs g
  p <- cast $ extractPrf c
  return $ Plab p
elab _ (Af _ _ a) = error $ "Unimplemented case : " ++ show a

cnfCore :: Goal -> ([Form], Ctx) -> BT ([Form], Ctx)
cnfCore (f, k) (gs, c) =
  let gl = (f, k) in
  case (appFaL Nothing gl c, appAndL gl c, appOrL gl c) of
    (Just (gl', c'), _, _) -> cnfCore gl' (gs, c')
    (_, Just (fs, k', c'), _) -> do f <- pick fs
                                    cnfCore (f, k') (gs, c')
    (_, _, Just (gls, c')) -> foldM (flip cnfCore) (gs, c') gls
    _ -> do (g, gs') <- pluck gs
            c' <- cast $ appAx Pars (f, g, k) c
            return (gs', c')

cnfTrans :: Form -> Form -> BT Ctx
cnfTrans f g = do
  (gs, k, c) <- cast $ appConc (g, 0) blank
  cnfCore (f, k) (gs, c) <&> snd

tryFlipEqL :: Goal -> Ctx -> BT (Goal, Ctx)
tryFlipEqL (Eq x y, k) c = return ((Eq x y, k), c) <|> cast (flipEqL (Eq x y, k) c)
tryFlipEqL _ _ = []

rewrite :: Term -> Term -> (Term, Term) -> Bnd -> Maybe Bnd
rewrite x y (x', y') b = uniTerm Lax b (x, x') >>= flip (uniTerm Lax) (y, y')

eqnToEqns :: Eqn -> Maybe [Eqn]
eqnToEqns (Fun f xs, Fun g ys) = guard (f == g) >> zipM xs ys
eqnToEqns _ = Nothing

atomsToEqs :: Form -> Form -> Maybe [(Term, Term)]
atomsToEqs (Rel r xs) (Rel s ys) = guard (r == s) >> zipM xs ys
atomsToEqs (Eq x0 x1) (Eq y0 y1) = return [(x0, y0), (x1, y1)]
atomsToEqs _ _ = Nothing

litsToEqns :: Form -> Form -> Maybe [Eqn]
litsToEqns (Not f) (Not g) = atomsToEqs f g
litsToEqns f g = atomsToEqs f g

breakList :: [a] -> BT (a, [a])
breakList [] = []
breakList (x : xs) = return (x, xs)

fForced2 :: Bnd -> Form -> (Form, [Form]) -> Maybe Bnd
fForced2 b f (h, hs) = do
  guard $ L.all (\ h_ -> isNothing (uniForm Lax b (f, h_))) hs
  uniForm Lax b (f, h)

gForced2 :: Form -> (Form, [Form]) -> Maybe [(Term, Term)]
gForced2 g (h, hs) = do
  guard $ L.all (isNothing . litsToEqns g) hs
  litsToEqns g h

fForced1 :: [Form] -> Bnd -> (Form, [Form]) -> Maybe ([Form], Bnd)
fForced1 hs b (f, fs) = do
  let hhs = pluck hs
  b' <- first (fForced2 b f) hhs
  return (fs, b')

gBranch :: [Form] -> Sst -> (Form, [Form]) -> [Sst]
gBranch hs sst (g, gs) = do
  mapFilter
    ( \ h_ -> do
        eqs <- litsToEqns g h_
        return sst {glits = gs, eqns = insertAll eqs $ eqns sst} )
    hs

fBranch :: [Form] -> Sst -> (Form, [Form]) -> [Sst]
fBranch hs sst (f, fs) = do
  mapFilter
    ( \ h_ -> do
        b <- uniForm Lax (sbnd sst) (f, h_)
        return sst {flits = fs, sbnd = b} )
    hs

fForced :: [Form] -> Sst -> Maybe Sst
fForced hs sst = first (breakSingleton . fBranch hs sst) $ pluck $ flits sst

gForced :: [Form] -> Sst -> Maybe Sst
gForced hs sst = first (breakSingleton . gBranch hs sst) $ pluck $ flits sst

eqBranch :: Term -> Term -> Sst -> (Eqn, [Eqn]) -> [Sst]
eqBranch x y sst (eq, eqs) =
  cast (eqnToEqns eq >>= \ eqs' -> Just (sst {eqns = insertAll eqs' eqs})) ++
  cast (rewrite x y eq (sbnd sst) >>= \ b -> Just (sst {eqns = eqs, sbnd = b})) ++
  cast (uniNonFun Lax (sbnd sst) eq >>= \ b -> Just (sst {eqns = eqs, sbnd = b}))

uniEqnsMod :: Term -> Term -> [Eqn] -> Bnd -> BT Bnd
uniEqnsMod x y (eq : eqs) b = 
  ( do eqs' <- cast $ eqnToEqns eq 
       uniEqnsMod x y (insertAll eqs' eqs) b ) <|> 
  ( do b' <- cast $ rewrite x y eq b 
       uniEqnsMod x y eqs b' ) <|> 
  ( do b' <- cast $ uniNonFun Lax b eq 
       uniEqnsMod x y eqs b' ) 
  

uniEqnsMod x y [] b = return b
  
eqForced :: Term -> Term -> Sst -> Maybe Sst
eqForced x y sst = first (breakSingleton . eqBranch x y sst) $ pluck $ eqns sst

type Eqn = (Term, Term)

fAction :: [Form] -> Sst -> [Sst]
fAction hs sst =
  case flits sst of
    [] -> []
    (f : fs) -> fBranch hs sst (f, fs)

gAction :: [Form] -> Sst -> [Sst]
gAction hs sst =
  case glits sst of
    [] -> []
    (g : gs) -> gBranch hs sst (g, gs)

eqAction :: Term -> Term -> Sst -> [Sst]
eqAction x y sst =
  case eqns sst of
    [] -> []
    (eq : eqs) -> eqBranch x y sst (eq, eqs)

superaux :: Term -> Term -> [Form] -> Sst -> IO (Term, Term, Bnd)
superaux x y hs Sst {flits = [], glits = [], eqns = [], sbnd = b} = return (x, y, b)
superaux x y hs sst = do
  -- case (fForced hs sst, gForced hs sst, eqForced x y sst, fAction hs sst, gAction hs sst, eqAction x y sst) of
  --   (Just sst', _, _, _, _, _) -> superaux x y hs sst'
  --   (_, Just sst', _, _, _, _) -> superaux x y hs sst'
  --   (_, _, Just sst', _, _, _) -> superaux x y hs sst'
  --   (_, _, _, [], [], [])      -> MF.fail ""
  --   (_, _, _, [], [], ssts)    -> first (superaux x y hs) ssts
  --   (_, _, _, [], ssts, _)     -> first (superaux x y hs) ssts
  --   (_, _, _, ssts, _, _)      -> first (superaux x y hs) ssts
  case (fAction hs sst, gAction hs sst, eqAction x y sst) of
    ([], [], ssts)    -> first (superaux x y hs) ssts
    ([], ssts, _)     -> first (superaux x y hs) ssts
    (ssts, _, _)      -> first (superaux x y hs) ssts

uniNonFun :: UniMode -> Bnd -> (Term, Term) -> Maybe Bnd
uniNonFun um b (Fun _ _, Fun _ _) = Nothing
uniNonFun um b xy = uniTerm um b xy

first :: (MonadFail m, Alternative m) => (a -> m b) -> [a] -> m b
first f [] = MF.fail ""
first f (x : xs) = f x <|> first f xs

breakEq :: Form -> Maybe (Term, Term)
breakEq (Eq x y) = Just (x, y)
breakEq _ = Nothing

superposition :: Form -> Form -> Form -> IO Ctx
superposition f g h = super f g h <|> super g f h

eqf :: Form -> Form -> IO Ctx
eqf f g = do
  (gs'', k0, c0) <- cast $ appConc (g, 0) blank
  let (gs', k1, c1) = flipLitsR gs'' k0 c0
  let gs = nub gs'
  (_, xs, fs) <- cast $ breakPrem 0 f
  (x, y, b) <- cast $ eqfAux fs gs
  let x' = appBndTerm Mid b x
  let y' = appBndTerm Mid b y
  -- let lf' = appBndForm Mid b lf
  -- let lg' = appBndForm Mid b lg
  let xs' = L.map (appBndTerm Mid b) xs
  ((_, k2), c2) <- cast $ appNotR (Not (x' === y'), k1) c1
  (_, gls, c3) <- cast $ appPrem (Just xs') (f, k2) c2
  -- cast $ eqfFinish x' y' lf' lg' gls gs c3
  cast $ foldM (pickMatchMod x' y' gs) c3 gls

-- eqfFinish :: Term -> Term -> Form -> Form -> [Goal] -> [Form] -> Ctx -> BT Ctx
-- eqfFinish x y lf lg ((f, k) : gls) gs c = 
--   if lf == f 
--   then cast (matchMod x y (f, lg, k) c) >>= eqfFinish x y lf lg gls gs 
--   else do g <- pick gs 
--           c' <- cast $ appAx Lax (f, g, k) c
--           eqfFinish x y lf lg gls gs c'
-- eqfFinish x y lf lg [] gs c = return c
-- 
-- orient :: Form -> [Form]
-- orient (Eq x y) = [x === y, y === x]
-- orient f = [f]

eqfAux :: [Form] -> [Form] -> BT (Term, Term, Bnd)
eqfAux fs gs = do 
  (Not (Eq x y), gs') <- pluck gs 
  -- trace (unpack $ "Eq found : " <> ppForm (x === y)) (return ())
  g <- pick gs' 
  -- trace (unpack $ "g = " <> ppForm g) (return ())
  (f, fs') <- pluck fs
  -- trace (unpack $ "f = " <> ppForm f) (return ())
  eqs <- cast $ litsToEqns f g
  -- trace (unpack $ "eqs = " <> ppList ppEq eqs) (return ())
  b <- uniEqnsMod x y eqs HM.empty
  -- trace ("b = " ++ show b) (return ())
  -- trace (unpack $ "fs' = " <> ppList ppForm fs') (return ())
  -- trace (unpack $ "gs'' = " <> ppList ppForm gs'') (return ())
  b' <- uniLits fs' gs' b 
  -- trace ("b' = " ++ show b') (return ())
  return (x, y, b')

uniLits :: [Form] -> [Form] -> Bnd -> BT Bnd 
uniLits [] _ b = return b
uniLits (f : fs) gs b = do 
  g <- pick gs 
  -- trace (unpack $ "picked g = " <> ppForm g) (return ())
  b' <- cast $ uniForm Lax b (f, g)
  uniLits fs gs b'

super :: Form -> Form -> Form -> IO Ctx
super f g h = do
  (hs, k0, c0) <- cast $ appConc (h, 0) blank
  let (hs', k1, c1) = flipLitsR hs k0 c0
  let hs'' = nub hs'
  (m, xs, fs) <- cast $ breakPrem 0 f
  (_, ys, gs) <- cast $ breakPrem m g
  let eqfss = L.concatMap
                ( \case
                    (Eq x y, fs) -> [(x, y, fs), (y, x, fs)]
                    _ -> [] ) (pluck fs)
  (x, y, b) <- first
    ( \ (x_, y_, fs_) -> superaux x_ y_ hs'' Sst {flits = fs_, glits = gs, eqns = [], sbnd = HM.empty} )
    eqfss
  let x' = appBndTerm Mid b x
  let y' = appBndTerm Mid b y
  let xs' = L.map (appBndTerm Mid b) xs
  let ys' = L.map (appBndTerm Mid b) ys
  (m, n, c2) <- cast $ appCut nt (Eq x' y', k1) (c1 {binding = b})
  (m, n, c2) <- cast $ appCut nt (Eq x' y', k1) (c1 {binding = b})
  ((_, m'), c3) <- cast $ flipEqR (Eq x' y', m) c2
  (_, fgls, c4) <- cast $ appPrem (Just xs') (f, m') c3 
  c5 <- cast $ foldM (pickMatch (Eq x' y' : Eq y' x' : hs'')) c4 fgls
  ((_, n'), c6) <- cast $ flipEqL (Eq x' y', n) c5
  (_, ggls, c7) <- cast $ appPrem (Just ys') (g, n') c6 
  cast $ foldM (pickMatchMod x' y' hs'') c7 ggls

pickMatch :: [Form] -> Ctx -> Goal -> Maybe Ctx
pickMatch gs c (f, k) = first (\ g -> appAx Exact (f, g, k) c) gs

pickMatchMod :: Term -> Term -> [Form] -> Ctx -> Goal -> Maybe Ctx
pickMatchMod x y gs c (f, k) = first (\ g -> matchMod x y (f, g, k) c) gs

equateMod :: Term -> Term -> EqGoal -> Ctx -> Maybe Ctx
equateMod x y (s, t, k) c =
  appEqR Exact (Eq s t, k) c <|>
  appAx Exact (Eq x y, Eq s t, k) c <|>
  do (egs, c') <- appFunC (s, t, k) c
     foldM (flip $ equateMod x y) c' egs

matchMod :: Term -> Term -> PrvGoal -> Ctx -> Maybe Ctx
matchMod x y pg c =
  ( do (egs, c') <- appRelC pg c
       foldM (flip $ equateMod x y) c' egs ) <|>
  ( do (eg0, eg1, c') <- appEqC pg c
       equateMod x y eg0 c' >>= equateMod x y eg1 ) <|>
  (appNotLR pg c >>= uncurry (matchMod y x))

matchModSym :: Term -> Term -> PrvGoal -> Ctx -> Maybe Ctx
matchModSym x y pg@(f, g, k) c =
  ( do (egs, c') <- appRelC pg c
       foldM (flip $ equateMod x y) c' egs ) <|>
  ( do (eg0, eg1, c') <- appEqC pg c
       equateMod x y eg0 c' >>= equateMod x y eg1 ) <|>
  ( do ((f', k'), c') <- appEqSL (f, k) c
       (eg0, eg1, c'') <- appEqC (f', g, k') c'
       equateMod x y eg0 c'' >>= equateMod x y eg1 ) <|>
  (appNotLR pg c >>= uncurry (matchMod y x))


resolve :: Form -> Form -> Form -> BT Ctx
resolve pr0 pr1 h = do
   (f, g) <- shuffle pr0 pr1
   (hs, k, c) <- cast $ appConc (h, 0) blank
   (i_, j_, c0_) <- cast $ appCut nt (Rel "" [], k) c
   (_, gls0_, c1_) <- cast $ appPrem Nothing (f, i_) c0_
   (_, gls1_, c2_) <- cast $ appPrem Nothing (g, j_) c1_
   a <- pluck (L.map fst gls0_) >>= breakNot . fst
   (i, j, c0) <- cast $ appCut nt (Not a, k) c
   (_, gls0, c1) <- cast $ appPrem nt (f, i) c0
   c2 <- litsMap Lax gls0 (Not a : hs) c1
   ((_, j'), c3) <- cast $ appNotL (Not a, j) c2
   (_, gls1, c4) <- cast $ appPrem nt (g, j') c3
   litsMap Lax gls1 (a : hs) c4

bvsOccurTerm :: [Text] -> Term -> Bool
bvsOccurTerm vs (Bv v) = v `elem` vs
bvsOccurTerm vs _ = False

bvsOccurForm :: [Text] -> Form -> Bool
bvsOccurForm vs (Eq x y) = bvsOccurTerm vs x || bvsOccurTerm vs y
bvsOccurForm vs (Rel _ xs) = L.any (bvsOccurTerm vs) xs
bvsOccurForm vs (Not f) = bvsOccurForm vs f
bvsOccurForm vs (And fs) = L.any (bvsOccurForm vs) fs
bvsOccurForm vs (Or fs)  = L.any (bvsOccurForm vs) fs
bvsOccurForm vs (Imp f g) = bvsOccurForm vs f || bvsOccurForm vs g
bvsOccurForm vs (Iff f g) = bvsOccurForm vs f || bvsOccurForm vs g
bvsOccurForm vs (Fa ws f) = bvsOccurForm (vs \\ ws) f
bvsOccurForm vs (Ex ws f) = bvsOccurForm (vs \\ ws) f

pointLessQuant :: Form -> Bool
pointLessQuant (Fa vs f) = not $ bvsOccurForm vs f
pointLessQuant (Ex vs f) = not $ bvsOccurForm vs f
pointLessQuant _ = False

pairSolve :: PrvGoal -> Ctx -> BT Ctx
pairSolve (f, g, k) c =
  let pg = (f, g, k) in
  if pointLessQuant f
  then ( do ((f', k'), c') <- cast $ appFaL Nothing (f, k) c
            pairSolve (f', g, k') c' ) <|>
       ( do (_, (f', k'), c') <- cast $ appExL (f, k) c
            pairSolve (f', g, k') c' )
  else
    if pointLessQuant g
    then ( do (_, (g', k'), c') <- cast $ appFaR (g, k) c
              pairSolve (f, g', k') c' ) <|>
         ( do ((g', k'), c') <- cast $ appExR Nothing (g, k) c
              pairSolve (f, g', k') c' )
    else
      cast (appNots pg c <|> appFaRL Perm pg c <|> appExLR Perm pg c) !>=
        uncurry pairSolve $
        cast (appImpLR pg c <|> appIffRL pg c) !>=
          (\ (pg0, pg1, c') -> pairSolve pg0 c' >>= pairSolve pg1) $
          cast (appOrR (g, k) c) !>=
            (\ (gs, k', c') -> do
               (gls, c'') <- cast $appOrL (f, k') c'
               pairSolveOr gls gs c'' ) $
            cast (appAndL (f, k) c) !>=
              (\ (fs, k', c') -> do
                 (gls, c'') <- cast $ appAndR (g, k') c'
                 pairSolveAnd fs gls c'' ) $
              (matchAtom Exact pg c <!> matchAtom Pars pg c)

breakNot :: Form -> BT Form
breakNot (Not f) = return f
breakNot _ = []

isNegRefl :: Form -> Bool
isNegRefl (Not (Eq x y)) = x == y
isNegRefl _ = False

pluckBy :: (a -> Bool) -> [a] -> BT [a]
pluckBy p [] = []
pluckBy p (x : xs) =
  if p x
    then return xs
    else do xs' <- pluckBy p xs
            return (x : xs')

matchLit :: UniMode -> PrvGoal -> Ctx -> BT Ctx
matchLit um pg c =
  cast (appNotLR pg c) !>=
    uncurry (matchAtom um) $
    matchAtom um pg c

matchAtom :: UniMode -> PrvGoal -> Ctx -> BT Ctx
matchAtom um (f, g, k) c =
  cast (appAx um (f, g, k) c) <|>
  cast ( do ((f', k'), c') <- flipEqL (f, k) c
            appAx um (f', g, k') c' )

flipEqL :: Goal -> Ctx -> Maybe (Goal, Ctx)
flipEqL (Eq x y, k) c0 = do
  (m, n, c1) <- appCut nt (Eq y x, k) c0
  c2 <- appEqS Exact (Eq x y, Eq y x, m) c1
  return ((Eq y x, n), c2)
flipEqL _ _ = Nothing

flipEqR :: Goal -> Ctx -> Maybe (Goal, Ctx)
flipEqR (Eq x y, k) c0 = do
  (m, n, c1) <- appCut nt (Eq y x, k) c0
  c2 <- appEqS Exact (Eq y x, Eq x y, n) c1
  return ((y === x, m), c2)
flipEqR _ _ = Nothing

flipLitR :: Goal -> Ctx -> Maybe (Goal, Ctx)
flipLitR (Not (Eq x y), k) c0 = do
  (m, n, c1) <- appCut nt (Not (y === x), k) c0
  c2 <- appNotLR (Not (y === x), Not (x === y), n) c1 >>= uncurry (appEqS Exact)
  return ((Not (y === x), m), c2)
flipLitR (l, k) c = flipEqR (l, k) c

flipLitsR :: [Form] -> Int -> Ctx -> ([Form], Int, Ctx)
flipLitsR [] k c = ([], k, c)
flipLitsR (l : ls) k c =
  let (ls', k', c') = flipLitsR ls k c in
  case flipLitR (l, k') c' of
    Just ((l', k''), c'') -> (l : l' : ls', k'', c'')
    _ -> (l : ls', k', c')

-- try :: (MonadCast m) => m a -> a -> a
-- try mx y = 
--   case cast mx of
--     Just x -> x 
--     _ -> y
-- 
tryAppOrR :: Goal -> Ctx -> ([Form], Int, Ctx)
tryAppOrR gl@(f, k) c = appOrR gl c ?> id $ ([f], k, c)

tryAppAndL :: Goal -> Ctx -> ([Form], Int, Ctx)
tryAppAndL gl@(f, k) c = appAndL gl c ?> id $ ([f], k, c)

tryAppOrL :: Goal -> Ctx -> ([Goal], Ctx)
tryAppOrL gl c = appOrL gl c ?> id $ ([gl], c)

tryAppAndR :: Goal -> Ctx -> ([Goal], Ctx)
tryAppAndR gl c = appAndR gl c ?> id $ ([gl], c)

tryAppFaL :: Goal -> Ctx -> (Goal, Ctx)
tryAppFaL gl c = appFaL Nothing gl c ?> id $ (gl, c)

maybeAppFaL :: Maybe [Term] -> Goal -> Ctx -> Maybe (Goal, Ctx)
maybeAppFaL Nothing gl c = return (gl, c)
maybeAppFaL (Just xs) gl c = appFaL (Just xs) gl c

isAtom :: Form -> Bool
isAtom (Rel _ _) = True
isAtom (Eq _ _) = True
isAtom _ = False

isLit :: Form -> Bool
isLit (Not f) = isAtom f
isLit f = isAtom f

breakPrems :: Int -> [Form] -> Maybe (Int, [Term], [Form])
breakPrems k [] = return (k, [], [])
breakPrems k (f : fs) = do 
  (m, xs, gs) <- breakPrem k f 
  (n, ys, hs) <- breakPrems m fs
  return (n, xs ++ ys, gs ++ hs)

breakPrem :: Int -> Form -> Maybe (Int, [Term], [Form])
breakPrem k (Or ls) = breakPrems k ls
breakPrem k (Fa vs f) = do
  let (k', xs) = listFvs k vs
  vxs <- zipM vs xs
  (k'', ys, fs) <- breakPrem k' (substForm vxs f)
  return (k'', xs ++ ys, fs)
breakPrem k l = guard (isLit l) >> return (k, [], [l])

breakFa :: Int -> Form -> Maybe (Int, Maybe [Term], Form)
breakFa k (Fa vs f) = do
  let (k', xs) = listFvs k vs
  vxs <- zipM vs xs
  return (k', Just xs, substForm vxs f)
breakFa k f = return (k, Nothing, f)

breakOr :: Form -> Maybe [Form]
breakOr (Or ls) = do
  -- guard $ L.all isLit ls
  -- return ls
  mapM breakOr ls <&> L.concat
breakOr l = guard (isLit l) >> return [l]

filterExcept :: (a -> Bool) -> [a] -> Maybe (a, [a])
filterExcept p [] = nt
filterExcept _ [x] = return (x, [])
filterExcept p (x : xs) =
  if p x
  then do (y, ys) <- filterExcept p xs
          return (y, x : ys)
  else guard (L.all p xs) >> return (x, xs)

breakFaOr :: Int -> Form -> Maybe (Int, Maybe [Term], [Form])
breakFaOr k f = do
  (k', xs, f') <- breakFa k f
  ls <- breakOr f'
  return (k', xs, ls)


conjMap :: [Form] -> [Goal] -> Ctx -> Maybe Ctx
conjMap _ [] c = return c
conjMap fs ((g, k) : gks) c = do
  c' <- appTopR (g, k) c <|> first (\ f -> appAx Exact (f, g, k) c) fs
  conjMap fs gks c'

litsMap :: UniMode -> [Goal] -> [Form] -> Ctx -> BT Ctx
litsMap _ [] _ c = return c
litsMap um ((f, k) : gls) gs c =
  let gl = (f, k) in
  ( cast (appBotL gl c) <|>
    cast (appNotL gl c >>= uncurry (appEqR um)) <|>
    ( do g <- pick gs
         matchLit um (f, g, k) c ) ) >>=
    litsMap um gls gs

main :: IO ()
main = do
  (tptp : tstp : _) <- getArgs
  tptp_afs <- parseName tptp
  tstp_afs <- sortAfs <$> parseName tstp
  let hs = L.foldl addHyp (HM.empty, S.empty) tptp_afs
  Prelude.putStr $ tptp ++ "\n"
  mapM_ putAnForm tptp_afs
  Prelude.putStr $ tstp ++ "\n"
  mapM_ putAnForm tstp_afs
  foldM_ elabIO hs tstp_afs
  Prelude.putStr "Elab finished.\n\n"
