{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module Main where

import Types
import Basic
import PP
import Parse
import Check
import Elab
import Data.Char
import Control.Monad as M (guard, foldM, foldM_ ,(>=>))
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative hiding (some, star)
import System.Environment
import System.Process
import Text.Printf
import Data.List as L
    ( filter, map, length, foldl, elem, all, any, concat, (\\), elemIndex, insert, sortBy )
import Data.Text as T
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList )
import qualified Data.Text.Lazy as TL (toStrict, intercalate)
import Data.Text.IO as TIO
import Data.Foldable (Foldable, toList)
import Data.Text.Read as TR
import Data.Hashable (Hashable)
import Debug.Trace
import Data.Functor ((<&>))
import Data.HashMap.Lazy as HM ( HashMap, insert, lookup, empty, map, member, mapMaybe, toList, fromListWithKey, delete )
import Data.List.Unique as LU
import qualified Data.Maybe as L
import qualified GHC.Base as L

pattern x :> xs <- (T.uncons -> Just (x, xs))

data BT a =
    End [Text]
  | Pass [Text] a (BT a)

appMsg :: [Text] -> BT a -> BT a
appMsg ts (End ss) = End $ ts ++ ss
appMsg ts (Pass ss x xs) = Pass (ts ++ ss) x xs

altBT :: BT a -> BT a -> BT a
altBT (End ts) ys = appMsg ts ys
altBT (Pass ts x xs) ys = Pass ts x $ altBT xs ys

bindBT :: BT a -> (a -> BT b) -> BT b
bindBT (End ts) _ = End ts
bindBT (Pass ts x xs) g = appMsg ts $ altBT (g x) (bindBT xs g)

mapBT :: (a -> b) -> BT a -> BT b
mapBT f (End ts) = End ts
mapBT f (Pass ts x xs) = Pass ts (f x) $ mapBT f xs

seqBT :: BT (a -> b) -> BT a -> BT b
seqBT (End ts) _ = End ts
seqBT (Pass ts f fs) xs = appMsg ts $ altBT (mapBT f xs) $ seqBT fs xs

(!>=) :: BT a -> (a -> BT b) -> BT b -> BT b
(!>=) xs ys zs =
  case xs of
    Pass ts x _ -> appMsg ts $ ys x
    End ts -> appMsg ts zs

(!>) :: BT a -> BT b -> BT b -> BT b
(!>) xs ys = xs !>= const ys

instance Alternative BT where
  empty = End []
  (<|>) = altBT

instance Functor BT where
  fmap = mapBT

instance Applicative BT where
  pure x = Pass [] x (End [])
  f <*> g = seqBT f g

instance Monad BT where
  return x = Pass [] x (End [])
  x >>= y = bindBT x y

instance MonadFail BT where
  fail s = End [pack s]

b2i :: BT a -> IO a
b2i (End _) = MF.fail ""
b2i (Pass _ x _) = return x

m2i :: Maybe a -> IO a
m2i Nothing = MF.fail ""
m2i (Just x) = return x

putText :: Text -> IO ()
putText = TIO.putStr

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

ff :: Form
ff = And []

tt :: Form
tt = Or []

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

addIf :: Bool -> NSeq-> Text -> Form -> BT NSeq
addIf True c n f = return $ HM.insert n f c
addIf False _ n _ = error $ "Cannot add formula : " ++ unpack n

getHyp :: Text -> NSeq -> IO Form
getHyp n c =
  case HM.lookup n c of
    Just f -> return f
    _ -> MF.fail $ "Hypothesis does not exist : " ++ show n

type Bnd = HashMap Int Term
type Prfs = HashMap Int Prf_

data Ctx = Ctx {fresh :: Int, binding :: Bnd, proofs :: Prfs}
data Ctx_ = Ctx_ {fresh_ :: Int, binding_ :: Bnd}

posMap :: (Int -> a -> b) -> Int -> [a] -> [b]
posMap _ _ [] = []
posMap f k (x : xs) = f k x : posMap f (k + 1) xs

getEqGoals :: Int -> [Term] -> [Term] -> BT (Int, [EqGoal])
getEqGoals k [] [] = return (k, [])
getEqGoals k (x : xs) (y : ys) = do
  (m, xyks) <- getEqGoals (k + 1) xs ys
  return (m, (x, y, k) : xyks)
getEqGoals _ _ _ = End ["term-term-ints"]

getGoals :: Int -> [Form] -> (Int, [Goal])
getGoals k [] = (k, [])
getGoals k (f : fs) =
  let (m, pfs) = getGoals (k + 1) fs in
  (m, (f, k) : pfs)

breakPrems :: [Goal] -> Ctx -> BT ([Goal], Ctx)
breakPrems [] c = return ([], c)
breakPrems (gl : gls) c = do
  (gls0, c') <- breakPrem gl c
  (gls1, c'') <- breakPrems gls c'
  return (gls0 ++ gls1, c'')

papp :: ([a] -> b) -> a -> [a] -> b
papp f x xs = f $ x : xs

funnel :: ([Prf] -> Prf) -> [Goals_] -> [Prf] -> Prf
funnel pp [] [] = pp []
funnel pp [] (_ : _) = error "too many funnel args"
funnel pp (([], pp') : glss) ps = funnel (papp pp $ pp' []) glss ps
funnel pp ((_ : _, pp') : glss) [] = error "too few funnel args"
funnel pp ((_ : fs, pp') : glss) (p : ps) = funnel pp ((fs, papp pp' p) : glss) ps

ph :: Prf
ph = EqR $ Fun "" []

headPrf :: [Prf] -> Prf
headPrf [] = ph
headPrf (p : ps) = p

breakPrem :: Goal -> Ctx -> BT ([Goal], Ctx)
breakPrem gl c =
  (appNotNotL gl c <|> appFaL Nothing gl c) !>=
    uncurry breakPrem $
    appOrL gl c !>=
      uncurry breakPrems $
      return ([gl], c)

breakConcs :: [Form] -> Int -> Ctx -> BT ([Form], Int, Ctx)
breakConcs [] k c = return ([], k, c)
breakConcs (f : fs) k c = do
  (fsl, k', c') <- breakConc (f, k) c
  (fsr, k'', c'') <- breakConcs fs k' c'
  return (fsl ++ fsr, k'', c'')

breakConc :: Goal -> Ctx -> BT ([Form], Int, Ctx)
breakConc (g, k) c =
  let gl = (g, k) in
  appNotNotR gl c !>=
    uncurry breakConc $
    appFaR gl c !>= 
      (\ (_, gl', c') -> breakConc gl' c') $
      appOrR gl c !>=
      (\ (gs, k', c') -> breakConcs gs k' c') $
      return ([g], k, c)

appNestAndR' :: ([Goal], Ctx) -> BT ([Goal], Ctx)
appNestAndR' ([], c) = return ([], c)
appNestAndR' (gl : gls, c0) = do
  (gls1, c1) <- appNestAndR gl c0
  (gls2, c2) <- appNestAndR' (gls, c1)
  return (gls1 ++ gls2, c2)

appNestAndR :: Goal -> Ctx -> BT ([Goal], Ctx)
appNestAndR (f, k) c = appAndR (f, k) c !>= appNestAndR' $ return ([(f, k)], c)

appNestOrL' :: ([Goal], Ctx) -> BT ([Goal], Ctx)
appNestOrL' ([], c) = return ([], c)
appNestOrL' (gl : gls, c0) = do
  (gls1, c1) <- appNestOrL gl c0
  (gls2, c2) <- appNestOrL' (gls, c1)
  return (gls1 ++ gls2, c2)

appNestOrL :: Goal -> Ctx -> BT ([Goal], Ctx)
appNestOrL (f, k) c = appOrL (f, k) c !>= appNestOrL' $ return ([(f, k)], c)

appNestAndL' :: ([Form], Int, Ctx) -> BT ([Form], Int, Ctx)
appNestAndL' ([], k, c) = return ([], k, c)
appNestAndL' (g : gs, k0, c0) = do 
  (gs1, k1, c1) <- appNestAndL (g, k0) c0
  (gs2, k2, c2) <- appNestAndL' (gs, k1, c1)
  return (gs1 ++ gs2, k2, c2)

appNestAndL :: Goal -> Ctx -> BT ([Form], Int, Ctx)
appNestAndL (g, k) c = appAndL (g, k) c !>= appNestAndL' $ return ([g], k, c)

appNestOrR' :: ([Form], Int, Ctx) -> BT ([Form], Int, Ctx)
appNestOrR' ([], k, c) = return ([], k, c)
appNestOrR' (g : gs, k0, c0) = do 
  (gs1, k1, c1) <- appNestOrR (g, k0) c0
  (gs2, k2, c2) <- appNestOrR' (gs, k1, c1)
  return (gs1 ++ gs2, k2, c2)

appNestOrR :: Goal -> Ctx -> BT ([Form], Int, Ctx)
appNestOrR (g, k) c = appOrR (g, k) c !>= appNestOrR' $ return ([g], k, c)

bindPrf :: Int -> Prf_ -> Prfs -> BT Prfs
bindPrf k p ps =
  if HM.member k ps
  then End ["Proof ID already bound"]
  else return $ HM.insert k p ps

appNotL :: Goal -> Ctx -> BT (Goal, Ctx)
appNotL (Not f, p) c =
  let k = fresh c in
  do ps <- bindPrf p (NotL_ f k) (proofs c)
     let c' = c {fresh = k + 1, proofs = ps} in
       return ((f, k), c')
appNotL (f, _) _ = End ["app-not-L : " <> ppForm f]

appNotR :: Goal -> Ctx -> BT (Goal, Ctx)
appNotR (Not f, p) c =
  let k = fresh c in
  do ps' <- bindPrf p (NotR_ f k) (proofs c)
     let c' = c {fresh = k + 1, proofs = ps'} in
       return ((f, k), c')
appNotR (f, k) _ = End ["app-not-R : " <> ppForm f]

appNotNotL :: Goal -> Ctx -> BT (Goal, Ctx)
appNotNotL gl c = appNotL gl c >>= uncurry appNotR

appNotNotR :: Goal -> Ctx -> BT (Goal, Ctx)
appNotNotR gl c = appNotR gl c >>= uncurry appNotL

appNotLR :: PrvGoal -> Ctx -> BT (PrvGoal, Ctx)
appNotLR (f, g, k) c = do
  ((f', k0), c0) <- appNotL (f, k) c
  ((g', k1), c1) <- appNotR (g, k0) c0
  return ((g', f', k1), c1)

appNots :: PrvGoal -> Ctx -> BT (PrvGoal, Ctx)
appNots (f, g, k) c =
  ( do ((f', k'), c') <- appNotNotL (f, k) c
       return ((f', g, k'), c') ) <!>
  ( do ((g', k'), c') <- appNotNotR (g, k) c
       return ((f, g', k'), c') ) <!>
  appNotLR (f, g, k) c


appBnd :: Bnd -> Term -> Term
appBnd b (Fv k) =
  case HM.lookup k b of
    Just x -> x
    Nothing -> Fv k
appBnd b (Fun f xs) = Fun f $ L.map (appBnd b) xs
appBnd _ x = x

hasFv :: Int -> Term -> Bool
hasFv k (Fv m) = k == m
hasFv k (Fun _ xs) = L.any (hasFv k) xs
hasFv _ _ = False

substFv :: Int -> Term -> Term -> Term
substFv k x (Bv v) = Bv v
substFv k x (Par m) = Par m
substFv k x (Fv m) = if k == m then x else Fv m
substFv k x (Fun f xs) = Fun f $ L.map (substFv k x) xs

breakFvLookup :: Term -> Bnd -> Maybe Term
breakFvLookup (Fv k) b = HM.lookup k b
breakFvLookup _ _ = Nothing

uniFv :: UniMode -> Bnd -> Int -> Term -> BT Bnd
-- scheme  : uniFv um b k x
-- assumes : k is unbound in b
-- assumes : if x is a FV, it is also unbound in b
uniFv Exact b k (Fv m) = guard (k == m) >> return b
uniFv Exact _ _ _  = End []
uniFv Pars b k (Par m) =
  let b' = HM.map (substFv k (Par m)) b in
  return $ HM.insert k (Par m) b'
uniFv Pars _ _ _ = End []
uniFv ParFvs b k x =
  let x' = appBnd b x in
  let b' = HM.map (substFv k x') b in
  do guard $ isPar x || isFv x
     guard $ not $ hasFv k x'
     return $ HM.insert k x' b'
uniFv Lax b k x =
  let x' = appBnd b x in
  let b' = HM.map (substFv k x') b in
  do guard $ not $ hasFv k x'
     return $ HM.insert k x' b'

isPar :: Term -> Bool
isPar (Par _) = True
isPar _ = False

isFv :: Term -> Bool
isFv (Fv _) = True
isFv _ = False

uniTerm :: UniMode -> Bnd -> (Term, Term) -> BT Bnd
uniTerm s b (Bv v, Bv w)   = guard (v == w) >> return b
uniTerm s b (Par k, Par m) = guard (k == m) >> return b
uniTerm s b (Fun f xs, Fun g ys) = do
  xys <- zipM xs ys
  guardBT "unify-funs" (f == g)
  foldM (uniTerm s) b xys
uniTerm s b (x, y) =
  if x == y
  then return b
  else
    case (breakFvLookup x b, breakFvLookup y b, x, y) of
       (Just x', _, _, _) -> uniTerm s b (x', y)
       (_, Just y', _, _) -> uniTerm s b (x, y')
       (_, _, Fv k, _) -> uniFv s b k y
       (_, _, _, Fv m) -> uniFv s b m x
       (_, _, _, _) -> End ["unify-term-mismatch"]

uniForm :: UniMode -> Bnd -> (Form, Form) -> BT Bnd
uniForm s b (Eq x0 x1, Eq y0 y1) = foldM (uniTerm s) b [(x0, y0), (x1, y1)]
uniForm s b (Rel p xs, Rel q ys) = guard (p == q) >> zipM xs ys >>= foldM (uniTerm s) b
uniForm s b (Not f, Not g) = uniForm s b (f, g)
uniForm s b (And fs, And gs) = zipM fs gs >>= foldM (uniForm s) b
uniForm s b (Or fs,  Or gs)  = zipM fs gs >>= foldM (uniForm s) b
uniForm s b (Imp f0 f1, Imp g0 g1) = foldM (uniForm s) b [(f0, g0), (f1, g1)]
uniForm s b (Iff f0 f1, Iff g0 g1) = foldM (uniForm s) b [(f0, g0), (f1, g1)]
uniForm s b (Fa vs f, Fa ws g) = guard (vs == ws) >> uniForm s b (f, g)
uniForm s b (Ex vs f, Ex ws g) = guard (vs == ws) >> uniForm s b (f, g)
uniForm _ _ _ = End []

guardBT :: Text -> Bool -> BT ()
guardBT _ True = return ()
guardBT t False = End [t]

skipBT :: BT ()
skipBT = return ()

(<!>) :: BT a -> BT a -> BT a
(<!>) xs = xs !>= return

appFaRL :: InstMode -> PrvGoal -> Ctx -> BT (PrvGoal, Ctx)
appFaRL Same (f, g, k) c = do
  (xs, (g', k'), c') <- appFaR (g, k) c
  ((f', k''), c'') <- appFaL (Just xs) (f, k') c'
  return ((f', g', k''), c'')
appFaRL Perm (f, g, k) c = do
  (_, (g', k'), c') <- appFaR (g, k) c
  ((f', k''), c'') <- appFaL Nothing (f, k') c'
  return ((f', g', k''), c'')

appExLR :: InstMode -> PrvGoal -> Ctx -> BT (PrvGoal, Ctx)
appExLR Same (f, g, k) c = do
  (xs, (f', k'), c') <- appExL (f, k) c
  ((g', k''), c'') <- appExR (Just xs) (g, k') c'
  return ((f', g', k''), c'')
appExLR Perm (f, g, k) c = do
  (_, (f', k'), c') <- appExL (f, k) c
  ((g', k''), c'') <- appExR Nothing (g, k') c'
  return ((f', g', k''), c'')

origAnd :: [Form] -> [Goal] -> Ctx -> BT Ctx
origAnd [] [] c = return c
origAnd fs ((g, k) : gls) c =
  ( do ((g', k'), gls') <- pluck ((g, k) : gls)
       (f', fs') <- pluck fs
       c' <- appAx Exact (f', g', k') c
       return (fs', gls', c') ) !>=
    (\ (fs', gls', c') -> origAnd fs' gls' c') $
    do (f, fs') <- pluck fs
       c' <- orig (f, g, k) c
       origAnd fs' gls c'
origAnd _ _ _ = End []

origOr :: [Goal] -> [Form] -> Ctx -> BT Ctx
origOr [] [] c = return c
origOr ((f, k) : gls) gs c =
  ( do ((f', k'), gls') <- pluck ((f, k) : gls)
       (g', gs') <- pluck gs
       c' <- appAx Exact (f', g', k') c
       return (gls', gs', c') ) !>=
    (\ (gls', gs', c') -> origOr gls' gs' c') $
    do (g, gs') <- pluck gs
       c' <- orig (f, g, k) c
       origOr gls gs' c'
origOr _ _ _ = End ["orig-or-mismatch"]

pairSolveAnd :: [Form] -> [Goal]  -> Ctx -> BT Ctx
pairSolveAnd [] [] c = return c
pairSolveAnd fs ((g, k) : gls) c =
  ( do (f, fs') <- pluck fs
       c' <- appAx Exact (f, g, k) c
       return (fs', c') ) !>=
    uncurry (`pairSolveAnd` gls) $
    do (f, fs') <- pluck fs
       c' <- pairSolve (f, g, k) c
       pairSolveAnd fs' gls c'
pairSolveAnd _ _ _ = End ["pairsolve-and-mismatch"]

pairSolveOr :: [Goal] -> [Form] -> Ctx -> BT Ctx
pairSolveOr [] [] c = return c
pairSolveOr ((f, k) : gls) gs c =
  ( do (g, gs') <- pluck gs
       c' <- appAx Exact (f, g, k) c
       return (gs', c') ) !>=
    uncurry (pairSolveOr gls) $
    do (g, gs') <- pluck gs
       c' <- pairSolve (f, g, k) c
       pairSolveOr gls gs' c'
pairSolveOr _ _ _ = End ["pairsolve-or-mismatch"]

isOr :: Form -> Bool
isOr (Or _) = True
isOr _ = False

isAnd :: Form -> Bool
isAnd (And _) = True
isAnd _ = False

flat :: PrvGoal -> Ctx -> BT Ctx
flat (f, g, k) c =
  let pg = (f, g, k) in
  (appNots pg c <|> appFaRL Same pg c <|> appExLR Same pg c) !>=
    uncurry flat $
    (appImpLR pg c <|> appIffRL pg c) !>=
      (\ (pg0, pg1, c') -> flat pg0 c' >>= flat pg1) $
      if isOr f 
      then do (gs, k', c') <- appNestOrR (g, k) c
              (gls, c'') <- appNestOrL (f, k') c'
              pgs <- zipM gls gs <&> L.map (\ ((f_, k_), g_) -> (f_, g_, k_))
              foldM (flip flat) c'' pgs
      else 
        if isAnd f
        then do (fs, k', c') <- appNestAndL (f, k) c
                (gls, c'') <- appNestAndR (g, k') c'
                pgs <- zipM fs gls <&> L.map (\ (f_, (g_, k_)) -> (f_, g_, k_))
                foldM (flip flat) c'' pgs
        else matchAtom Exact pg c
          
orig :: PrvGoal -> Ctx -> BT Ctx
orig (f, g, k) c =
  let pg = (f, g, k) in
  (appNots pg c <|> appFaRL Perm pg c <|> appExLR Perm pg c) !>=
    uncurry orig $
    (appImpLR pg c <|> appIffRL pg c) !>=
      (\ (pg0, pg1, c') -> orig pg0 c' >>= orig pg1) $
      appOrR (g, k) c !>=
        (\ (gs, k', c') -> do
           (gls, c'') <- appOrL (f, k') c'
           origOr gls gs c'' ) $
        appAndL (f, k) c !>=
          (\ (fs, k', c') -> do
             (gls, c'') <- appAndR (g, k') c'
             origAnd fs gls c'' ) $
          (matchAtom Exact pg c <!> matchAtom Pars pg c)

skolemize :: [Form] -> PrvGoal -> Ctx -> BT Ctx
skolemize hs (f, g, k) c =
  let pg = (f, g, k) in
  appAx Exact pg c <!> (
    (appNotLR pg c <|> appFaRL Perm pg c <|> appExLR Perm pg c) !>=
      uncurry (skolemize hs) $
      (appImpLR pg c <|> appIffRL pg c) !>=
        (\ (pg0, pg1, c') -> skolemize hs pg0 c' >>= skolemize hs pg1) $
        appOrR (g, k) c !>=
          ( \ (gs, k', c') -> do
              (gls, c'') <- appOrL (f, k') c'
              glgs <- zipM gls gs
              pgs <- mapM (\ ((f_, k_), g_) -> return (f_, g_, k_)) glgs
              foldM (flip (skolemize hs)) c'' pgs ) $
          appAndL (f, k) c !>=
            ( \ (fs, k', c') -> do
                (gls, c'') <- appAndR (g, k') c'
                fgls <- zipM fs gls
                pgs <- mapM (\ (f_, (g_, k_)) -> return (f_, g_, k_)) fgls
                foldM (flip (skolemize hs)) c'' pgs ) $
            appAx Pars pg c <|>
            ( do (h, hs') <- pluck hs
                 ((h', k'), c0) <- tryAppFaL (h, k) c
                 ((f', m), (g', n), c1) <- appImpL (h', k') c0
                 c2 <- appAx ParFvs (f, f', m) c1
                 skolemize hs' (g', g, n) c2 )
  )

traceBT :: Text -> BT ()
traceBT t = trace (unpack t) skipBT

traceBTNL :: Text -> BT ()
traceBTNL t = trace (unpack t ++ "\n") skipBT

blank :: Ctx
blank = Ctx {fresh = 1, binding = HM.empty, proofs = HM.empty }

blank_ :: Ctx_
blank_ = Ctx_ {fresh_ = 0, binding_ = HM.empty}

isRdef :: Text -> Form -> Bool
isRdef r (Fa _ (Iff (Rel s _) _)) = r == s
isRdef r (Iff (Rel s _) _) = r == s
isRdef _ _ = False

checkElab :: Seq -> Form -> Elab -> IO ()
checkElab sq g (Plab p) = verify 0 sq (S.singleton g) p
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
  m2i (checkLrats (lratCtx 1 fs) lrs)

lratCtx :: Int -> [Form] -> HashMap Int Form
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

checkLrat :: HashMap Int Form -> Set Form -> [Int] -> Maybe ()
checkLrat _ _ [] = Nothing
checkLrat fs fxs [k] = do
  ls <- HM.lookup k fs >>= formToLits
  guard $ L.all (negated fxs) ls
checkLrat fs fxs (k : ks) = do
  ls <- HM.lookup k fs >>= formToLits
  fx <- breakSingleton $ L.filter (not . negated fxs) ls
  checkLrat fs (S.insert fx fxs) ks

checkLrats :: HashMap Int Form -> [Lrat] -> Maybe ()
checkLrats _ [] = trace "fail-0" Nothing
checkLrats fs [Add _ [] hs] = checkLrat fs S.empty hs
checkLrats _ [_] = trace "fail-1" Nothing
checkLrats fs (Add k ls hs : lrs) = do
  let ns = S.fromList $ L.map negLit ls
  checkLrat fs ns hs
  checkLrats (HM.insert k (Or ls) fs) lrs
checkLrats fs (Del _ ks : lrs) =
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

isSubstTerm :: [(Text, Term)] -> (Term, Term) -> IO ()
isSubstTerm vxs (Bv v, x) = guard $ L.elem (v, x) vxs
isSubstTerm vxs (x, y) = guard (x == y)

isSubstForm :: [(Text, Term)] -> (Form, Form) -> IO ()
isSubstForm vxs (Eq x0 x1, Eq y0 y1) = isSubstTerm vxs (x0, y0) >> isSubstTerm vxs (x1, y1)
isSubstForm vxs (Rel r xs, Rel s ys) = guard (r == s) >> zipM xs ys >>= mapM_ (isSubstTerm vxs)
isSubstForm vxs (Not f, Not g)   = isSubstForm vxs (f, g)
isSubstForm vxs (Or fs, Or gs) = zipM fs gs >>= mapM_ (isSubstForm vxs)
isSubstForm vxs (And fs, And gs) = zipM fs gs >>= mapM_ (isSubstForm vxs)
isSubstForm vxs (Imp f0 f1, Imp g0 g1) = isSubstForm vxs (f0, g0) >> isSubstForm vxs (f1, g1)
isSubstForm vxs (Iff f0 f1, Iff g0 g1) = isSubstForm vxs (f0, g0) >> isSubstForm vxs (f1, g1)
isSubstForm _ _ = MF.fail "is-subst-form"

isAOC :: [Term] -> Form -> IO ()
isAOC xs (Fa vs (Imp (Ex ws f) g)) = do
  guard $ L.all (isSkolemTerm vs) xs
  wxs <- zipM ws xs
  guard $ substForm wxs f == g
isAOC xs (Imp (Ex ws f) g) = do
  guard $ L.all isConstant xs
  wxs <- zipM ws xs
  guard $ substForm wxs f == g
isAOC _ _ = MF.fail "is-AOC"

elabIO :: Hyps -> AnForm -> IO Hyps
elabIO (nsq, sq) (Af n f a) = do
  print $ "Elaborating step = " <> n
  e <- elab nsq (Af n f a)
  checkElab sq f e
  return (HM.insert n f nsq, S.insert f sq)

lookupM :: (Hashable a, MonadFail m) => a -> HashMap a b -> m b
lookupM x h =
  case HM.lookup x h of
    Nothing -> MF.fail "lookup fail"
    Just y -> return y

appBndTerm :: Bnd -> Term -> Term
appBndTerm b (Fv k) =
  case HM.lookup k b of
    Nothing -> Fun "" []
    Just x -> appBndTerm b x
appBndTerm b (Fun f xs) = Fun f $ L.map (appBndTerm b) xs
appBndTerm _ x = x

appBndForm :: Bnd -> Form -> Form
appBndForm b (Eq x y) = Eq (appBndTerm b x) (appBndTerm b y)
appBndForm b (Rel r xs) = Rel r $ L.map (appBndTerm b) xs
appBndForm b (Not f) = Not $ appBndForm b f
appBndForm b (And fs) = And $ L.map (appBndForm b) fs
appBndForm b (Or fs) = Or $ L.map (appBndForm b) fs
appBndForm b (Imp f g) = Imp (appBndForm b f) (appBndForm b g)
appBndForm b (Iff f g) = Iff (appBndForm b f) (appBndForm b g)
appBndForm b (Fa vs f) = Fa vs $ appBndForm b f
appBndForm b (Ex vs f) = Ex vs $ appBndForm b f

collectGoal :: Prfs -> Goal -> Maybe (Form, Prf)
collectGoal ps (f, k) = do
  p <- collectPrf ps k
  return (f, p)

collectEqGoal :: Prfs -> EqGoal -> Maybe (Term, Term, Prf)
collectEqGoal ps (x, y, k) = do
  p <- collectPrf ps k
  return (x, y, p)

collectPrf_ :: Prfs -> Prf_ -> Maybe Prf
collectPrf_ ps (Ax_ f) = return $ Ax f
collectPrf_ ps (EqR_ x) = return $ EqR x
collectPrf_ ps (EqC_ eg0 eg1) = do
  tp0 <- collectEqGoal ps eg0
  tp1 <- collectEqGoal ps eg1
  return $ EqC tp0 tp1
collectPrf_ ps (EqSL_ x y k) = collectPrf ps k <&> EqSL x y
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
collectPrf ps k = lookupM k ps >>= collectPrf_ ps

appBndGoal :: Bnd -> (Form, Prf) -> (Form, Prf)
appBndGoal b (f, p) = (appBndForm b f, appBndPrf b p)

appBndEqGoal :: Bnd -> (Term, Term, Prf) -> (Term, Term, Prf)
appBndEqGoal b (x, y, p) = (appBndTerm b x, appBndTerm b y, appBndPrf b p)

appBndVarTerm :: Bnd -> (Text, Term) -> (Text, Term)
appBndVarTerm b (v, x) = (v, appBndTerm b x)

appBndPrf :: Bnd -> Prf -> Prf
appBndPrf b (Ax f) = Ax $ appBndForm b f
appBndPrf b (EqR x) = EqR $ appBndTerm b x
appBndPrf b (EqC eg0 eg1) = EqC (appBndEqGoal b eg0) (appBndEqGoal b eg1)
appBndPrf b (EqSL x y p) = EqSL (appBndTerm b x) (appBndTerm b y) (appBndPrf b p)
appBndPrf b (FunC f egs) = FunC f $ L.map (appBndEqGoal b) egs
appBndPrf b (RelC r egs) = RelC r $ L.map (appBndEqGoal b) egs
appBndPrf b (NotL f p) = NotL (appBndForm b f) (appBndPrf b p)
appBndPrf b (NotR f p) = NotR (appBndForm b f) (appBndPrf b p)
appBndPrf b (OrL gls) = OrL $ L.map (appBndGoal b) gls
appBndPrf b (OrR fs gs p) = OrR (L.map (appBndForm b) fs) (L.map (appBndForm b) gs) (appBndPrf b p)
appBndPrf b (AndL fs gs p) = AndL (L.map (appBndForm b) fs) (L.map (appBndForm b) gs) (appBndPrf b p)
appBndPrf b (AndR gls) = AndR $ L.map (appBndGoal b) gls
appBndPrf b (ImpL f g p q) = ImpL (appBndForm b f) (appBndForm b g) (appBndPrf b p) (appBndPrf b q)

appBndPrf b (IffR f g p q) = IffR (appBndForm b f) (appBndForm b g) (appBndPrf b p) (appBndPrf b q)
appBndPrf b (IffLO f g p) = IffLO (appBndForm b f) (appBndForm b g) (appBndPrf b p)
appBndPrf b (IffLR f g p) = IffLR (appBndForm b f) (appBndForm b g) (appBndPrf b p)

appBndPrf b (ImpRA f g p) = ImpRA (appBndForm b f) (appBndForm b g) (appBndPrf b p)
appBndPrf b (ImpRC f g p) = ImpRC (appBndForm b f) (appBndForm b g) (appBndPrf b p)
appBndPrf b (FaL vxs f p) = FaL (L.map (appBndVarTerm b) vxs) (appBndForm b f) (appBndPrf b p)
appBndPrf b (FaR vs k f p) = FaR vs k (appBndForm b f) (appBndPrf b p)
appBndPrf b (ExL vs k f p) = ExL vs k (appBndForm b f) (appBndPrf b p)
appBndPrf b (ExR vxs f p) = ExR (L.map (appBndVarTerm b) vxs) (appBndForm b f) (appBndPrf b p)
appBndPrf b (Cut f p q) = Cut (appBndForm b f) (appBndPrf b p) (appBndPrf b q)

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
shuffle x y = altBT (return (x, y)) (return (y, x))

factor :: Form -> Form -> BT Ctx
factor f g = do
   (gs, k, c) <- breakConc (g, 0) blank
   (gls, c) <- breakPrem (f, k) c
   litsMap gls gs c

gFunFunctor :: Gterm -> Maybe Text
gFunFunctor (Gfun t []) = return t
gFunFunctor _ = Nothing

infer :: Text -> [Form] -> Form -> BT Ctx
infer "equality_resolution" [f] g = factor f g
infer "subsumption_resolution" [f, g] h = resolve f g h
infer "superposition" [f, g] h = superposition f g h
infer "forward_demodulation" [f, g] h = superposition f g h
infer "backward_demodulation" [f, g] h = superposition f g h
infer "resolution" [f, g] h = resolve f g h
infer "rectify" [f] g = pairSolve (f, g, 0) blank
infer "negated_conjecture" [f] g = appAx Exact (f, g, 0) blank
infer "skolemisation" (f : fs) g = trace (unpack $ ppObj (f : fs) g) skolemize fs (f, g, 0) blank
infer "cnf_transformation" [f] g = cnfTrans f g
infer "factoring" [f] g = factor f g
infer "trivial_inequality_removal" [f] g = factor f g
infer "duplicate_literal_removal" [f] g = factor f g
infer "avatar_split_clause" (f : fs) g = avatarSplit fs f g
infer "flattening" [f] g = flat (f, g, 0) blank
infer r fs g = traceBT ("Unimplemented case : " <> r <> "\n" <> ppObj fs g) >> End []

rwro :: Form -> Form -> Int -> Ctx -> BT (Form, Int, Ctx)
rwro (Iff f g) f' k c = do
  ((_, m), (_, n), c') <- appIffLR (Iff f g, k) c >>= uncurry appImpL
  c'' <- appAx Lax (f, f', n) c'
  return (g, m, c'')
rwro _ _ _ _ = End []

mp :: Form -> Form -> Form -> Int -> Ctx -> BT Ctx
mp (Imp f g) f' g' k c = do
  ((_, m), (_, n), c') <- appImpL (Imp f g, k) c
  appAx Lax (f', f, m) c' >>= appAx Lax (g, g', n)
mp _ _ _ _ _ = End []

rwrr :: Form -> Form -> Int -> Ctx -> BT (Form, Int, Ctx)
rwrr (Iff f g) g' k c = do
  ((_, m), (_, n), c') <- appIffLO (Iff f g, k) c >>= uncurry appImpL
  c'' <- appAx Lax (g, g', n) c'
  return (f, m, c'')
rwrr _ _ _ _ = End []

rwlo :: Form -> Form -> Int -> Ctx -> BT (Form, Int, Ctx)
rwlo (Iff f g) f' k c = do
  ((_, m), (_, n), c') <- appIffLO (Iff f g, k) c >>= uncurry appImpL
  c'' <- appAx Lax (f', f, m) c'
  return (g, n, c'')
rwlo _ _ _ _ = End []

rwlr :: Form -> Form -> Int -> Ctx -> BT (Form, Int, Ctx)
rwlr (Iff f g) g' k c = do
  ((_, m), (_, n), c') <- appIffLR (Iff f g, k) c >>= uncurry appImpL
  c'' <- appAx Lax (g', g, m) c'
  return (f, n, c'')
rwlr _ _ _ _ = End []

expandResult :: Form -> Form -> Ctx -> BT (Form, Ctx)
expandResult (Iff f g) h c =
  ( do b <- uniForm Lax (binding c) (f, h)
       return (g, c {binding = b}) ) <|>
  ( do b <- uniForm Lax (binding c) (g, h)
       return (f, c {binding = b}) )
expandResult _ _ _ = End []

expandSplit :: [Form] -> ([Form], Int, Ctx) -> Form -> BT ([Form], Int, Ctx)
expandSplit ds (hs, k, c0) (Not g) = do
  d <- pick ds
  (f, c1) <- expandResult d g c0
  (m, n0, c2) <- appCut (Not f, k) c1
  ((_, _, n1), c3) <- appNotLR (Not f, Not g, n0) c2
  ((gf, n2), c4) <- appIffLO (d, n1) c3 <|> appIffLR (d, n1) c3
  c5 <- mp gf g f n2 c4
  return (Not f : hs, m, c5)
expandSplit ds (hs, k, c) g = do
  d <- pick ds
  (g', k', c') <- rwro d g k c <|> rwrr d g k c
  (hs', k'', c'') <- breakConc (g', k') c'
  return (hs' ++ hs, k'', c'')

formToLit :: Form -> Maybe Form
formToLit (Not (Not f)) = formToLit f
formToLit (Not (Rel r xs)) = return $ Not (Rel r xs)
formToLit (Rel r xs) = return $ Rel r xs
formToLit _ = Nothing

formToDisjs :: Form -> [Form]
formToDisjs (Or fs) = fs
formToDisjs f = [f]

formToLits :: Form -> Maybe [Form]
formToLits f = mapM formToLit $ formToDisjs f

litToAtom :: Form -> Maybe Form
-- litToAtom (Eq x y) = return $ Eq x y
-- litToAtom (Not (Eq x y)) = return $ Eq x y
litToAtom (Rel r xs) = return $ Rel r xs
litToAtom (Not (Rel r xs)) = return $ Rel r xs
litToAtom _ = Nothing

litToNum :: [Form] -> Form -> Maybe Int
litToNum as (Not a) = do
  k <- L.elemIndex a as
  return $ - (k + 1)
litToNum as a = do
  k <- L.elemIndex a as
  return $ k + 1

litsToNums :: [Form] -> [Form] -> Maybe [Int]
litsToNums as = mapM (litToNum as)

avatarSplit :: [Form] -> Form -> Form -> BT Ctx
avatarSplit ds f g = do
  (gs, k, c) <- breakConc (g, 0) blank
  (hss, k', c') <- foldM (expandSplit ds) ([], k, c) gs
  (gls, c'') <- breakPrem (f, k') c'
  litsMap gls (gs ++ hss) c''

breakFv :: Term -> BT Int
breakFv (Fv k) = return k
breakFv _ = End ["break-FV"]

instTerm :: Bnd -> (Term, Term) -> BT Bnd
instTerm b (Fv k, x) =
  case HM.lookup k b of
    Nothing -> trace (unpack $ "Mapping " <> ppInt k <> " to " <> ppTerm x <> "\n") return $ HM.insert k x b
    Just y -> guard (x == y) >> return b
instTerm b (Fun f xs, Fun g ys) =
  guard (f == g) >> zipM xs ys >>= foldM instTerm b
instTerm b (x, y) = guard (x == y) >> return b

instForm :: Bnd -> (Form , Form) -> BT Bnd
instForm b (Eq x0 x1, Eq y0 y1) = foldM instTerm b [(x0, y0), (x1, y1)]
instForm b (Rel r xs, Rel s ys) = guard (r == s) >> zipM xs ys >>= foldM instTerm b
instForm b (Not f, Not g) = instForm b (f, g)
instForm b (And fs, And gs) = zipM fs gs >>= foldM instForm b
instForm b (Or fs, Or gs)   = zipM fs gs >>= foldM instForm b
instForm b (Imp f0 f1, Imp g0 g1) = foldM instForm b [(f0, g0), (f1, g1)]
instForm b (Iff f0 f1, Imp g0 g1) = foldM instForm b [(f0, g0), (f1, g1)]
instForm b (Fa vs f, Fa ws g) = guard (vs == ws) >> instForm b (f, g)
instForm b (Ex vs f, Ex ws g) = guard (vs == ws) >> instForm b (f, g)
instForm _ _ = End ["inst-form"]

normalizeAOC :: Form -> BT ([Term], Form)
normalizeAOC (Fa vs (Imp (Ex ws f) g)) = do
  let (_, xs) = listFvs 0 ws 
  wxs <- zipM ws xs
  -- let xs = L.map snd wxs in
  let f' = substForm wxs f
  ks <- mapM breakFv xs
  b <- instForm HM.empty (f', g)
  xs <- mapM (`lookupM` b) ks
  return (xs, Fa vs (Imp (Ex ws f) (appBndForm b f')))
normalizeAOC (Imp (Ex ws f) g) = do
  let (_, xs) = listFvs 0 ws 
  wxs <- zipM ws xs
  let f' = substForm wxs f 
  ks <- mapM breakFv xs
  b <- instForm HM.empty (f', g)
  xs <- mapM (`lookupM` b) ks
  return (xs, Imp (Ex ws f) (appBndForm b f'))
normalizeAOC _ = End ["normalize-AOC"]

breakGfun :: Gterm -> BT Text
breakGfun (Gfun t []) = return t
breakGfun _ = End []

(<=>) :: Form -> Form -> Form
(<=>) = Iff

(==>) :: Form -> Form -> Form
(==>) = Imp

mkRdef :: Text -> Form -> Maybe Form
mkRdef r (Fa vs g) = do
  f <- mkRdef r g
  return (Fa vs f)
mkRdef r (Iff (Rel s xs) f) = guard (r == s) >> return (Iff (Rel s xs) f)
mkRdef r (Iff f (Rel s xs)) = guard (r == s) >> return (Iff (Rel s xs) f)
mkRdef _ _ = Nothing

proveRdef :: PrvGoal -> Ctx -> BT Ctx
proveRdef (f, g, k) c0 =
  let pg = (f, g, k) in
  appFaRL Same pg c0 !>=
    uncurry proveRdef $
    appAx Pars pg c0 <!>
    do ((go, ko), (gr, kr), c1) <- appIffR (g, k) c0
       ((fr, ko'), c2) <- appIffLR (f, ko) c1
       c3 <- appAx Pars (fr, go, ko') c2
       ((fo, kr'), c4) <- appIffLO (f, kr) c3
       appAx Pars (fo, gr, kr') c4

charToInt :: Char -> Maybe Int
charToInt '0' = Just 0
charToInt '1' = Just 1
charToInt '2' = Just 2
charToInt '3' = Just 3
charToInt '4' = Just 4
charToInt '5' = Just 5
charToInt '6' = Just 6
charToInt '7' = Just 7
charToInt '8' = Just 8
charToInt '9' = Just 9
charToInt _ = Nothing

textToInt :: Text -> Maybe Int
textToInt ('-' :> t) = textToNat t <&> negate
textToInt t = textToNat t

textToNat :: Text -> Maybe Int
textToNat (c :> t) = do
  k <- charToInt c
  textToNatCore k t
textToNat _ = Nothing

textToNatCore :: Int -> Text -> Maybe Int
textToNatCore k (c :> t) = do
  m <- charToInt c
  textToNatCore ((k * 10) + m) t
textToNatCore k _ = Just k

removeLastZero :: [Text] -> Maybe [Text]
removeLastZero [] = Nothing
removeLastZero ["0"] = Just []
removeLastZero (t : ts) = removeLastZero ts <&> (t :)

removeMidLastZero :: [Text] -> Maybe ([Text], [Text])
removeMidLastZero [] = Nothing
removeMidLastZero ("0" : ts) = do
  ts' <- removeLastZero ts
  return ([], ts')
removeMidLastZero (t : ts) = do
  (ts0, ts1) <- removeMidLastZero ts
  return (t : ts0, ts1)

textsToLrat :: [Form] -> [Text] -> Maybe Lrat
textsToLrat _ (t : "d" : ts) = do
  k <- textToInt t
  ks <- removeLastZero ts >>= mapM textToInt
  return $ Del k ks
textsToLrat as (t : ts) = do
  k <- textToInt t
  (ts0, ts1) <- removeMidLastZero ts
  fs <- mapM (textToInt >=> intToLit as) ts0
  ks <- mapM textToInt ts1
  return $ Add k fs ks
textsToLrat _ _ = Nothing

nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 (x : _) = Just x
nth k (_ : xs) = nth (k - 1) xs

intToLit :: [Form] -> Int -> Maybe Form
intToLit as k =
  if k < 0
  then nth (abs k) as <&> Not
  else nth k as

elab :: NSeq -> AnForm -> IO Elab
elab s (Af n g (Just (Gfun "file" [_, Gfun m []]))) = do
  f <- getHyp m s
  c <- b2i (orig (f, g, 0) blank)
  p <- m2i $ extractPrf c
  return $ Plab p
elab _ (Af n g (Just (Gfun "introduced" [Gfun "avatar_definition" [], Glist [Gfun "new_symbols" [Gfun "naming" [], Glist [Gfun r []]]]]))) = do
  f <- m2i $ mkRdef r g
  c <- b2i $ proveRdef (f, g, 0) blank
  p <- m2i $ extractPrf c
  return $ Rdef r f p
elab s (Af n g (Just (Gfun "introduced" [Gfun "choice_axiom" [], Glist []]))) = do
  (xs, f) <- b2i $ normalizeAOC g
  c <- b2i$ pairSolve (f, g, 0) blank
  p <- m2i $ extractPrf c
  return $ AOC xs f p
elab c (Af m g (Just (Gfun "inference" [Gfun "true_and_false_elimination" [], _, Glist [Gfun n []]]))) =
  do f <- getHyp n c
     guard (tfsimp f == g)
     return $ Tfe f
elab c (Af m g (Just (Gfun "inference" [Gfun "ennf_transformation" [], _, Glist [Gfun n []]]))) =
  do f <- getHyp n c
     return $ Nnf True f
elab c (Af m g (Just (Gfun "inference" [Gfun "nnf_transformation" [], _, Glist [Gfun n []]]))) =
  do f <- getHyp n c
     return $ Nnf False f
elab s (Af _ g (Just (Gfun "inference" [Gfun "avatar_sat_refutation" [], _, Glist l]))) = do
  fs <- m2i (mapM gFunFunctor l) >>= mapM (`lookupM` s)
  lss <- m2i $ mapM formToLits fs
  as <- m2i $ mapM litToAtom (L.concat lss) <&> LU.sortUniq
  nss <- m2i $ mapM (litsToNums as) lss
  let max = L.length as
  let head = "p cnf " <> ppInt max <> " " <> ppInt (L.length nss)
  let body = L.map (\ ns -> T.intercalate " " $ L.map ppInt $ ns ++ [0]) nss
  TIO.writeFile "temp.cnf" $ T.intercalate "\n" $ head : body
  print "Running cadical..."
  runCommand "cadical -q temp.cnf temp.drat" >>= waitForProcess
  print "Running drat-trim..."
  runCommand "drat-trim temp.cnf temp.drat -L temp.lrat" >>= waitForProcess
  t <- TIO.readFile "temp.lrat"
  let lns = L.map T.words $ T.lines t
  lrs <- m2i $ mapM (textsToLrat as) lns
  runCommand "rm temp.*" >>= waitForProcess 
  return $ Lrats fs lrs
elab s (Af _ g (Just (Gfun "inference" [Gfun r [], _, Glist l]))) = do
  fs <- m2i (mapM gFunFunctor l) >>= mapM (`lookupM` s)
  c <- b2i $ infer r fs g
  p <- m2i $ extractPrf c
  return $ Plab p
elab _ (Af _ _ a) = error $ "Unimplemented case : " ++ show a

cnfCore :: Goal -> ([Form], Ctx) -> BT ([Form], Ctx)
cnfCore (f, k) (gs, c) =
  let gl = (f, k) in
  appFaL Nothing gl c !>=
    (\ (gl', c') -> cnfCore gl' (gs, c')) $
    appAndL gl c !>=
      ( \ (fs, k', c') -> do
          (f, _) <- pluck fs
          cnfCore (f, k') (gs, c') ) $
      appOrL gl c !>=
        ( \ (fs, c') -> foldM (flip cnfCore) (gs, c') fs ) $
        ( do (g, gs') <- pluck gs
             c' <- appAx Pars (f, g, k) c
             return (gs', c') )

cnfTrans :: Form -> Form -> BT Ctx
cnfTrans f g = do
   (gs, k, c) <- breakConc (g, 0) blank
   cnfCore (f, k) (gs, c) <&> snd


mark :: Int -> BT ()
mark k = traceBT $ "Marking checkpoint " <> ppInt k <> "\n"

stayOrFlip :: Goal -> Ctx -> BT (Goal, Ctx)
stayOrFlip (Eq x y, k) c = return ((Eq x y, k), c) <|> appEqSL (Eq x y, k) c
stayOrFlip _ _ = End []

mapFilterBT :: (a -> BT b) -> [a] -> BT [b]
mapFilterBT f [] = return []
mapFilterBT f (x : xs) = f x !>= (\ y -> mapFilterBT f xs <&> (y :)) $ mapFilterBT f xs

rewrite :: Term -> Term -> (Term, Term) -> Bnd -> BT Bnd
rewrite x y (x', y') b = uniTerm Lax b (x, x') >>= flip (uniTerm Lax) (y, y')

failsBT :: BT a -> BT ()
failsBT (End _) = return ()
failsBT _ = End []

eq2eqs :: (Term, Term) -> BT [(Term, Term)]
eq2eqs (Fun f xs, Fun g ys) = guard (f == g) >> zipM xs ys
eq2eqs _ = End []

atoms2eqs :: Form -> Form -> BT [(Term, Term)]
atoms2eqs (Rel r xs) (Rel s ys) = guard (r == s) >> zipM xs ys
atoms2eqs (Eq x0 x1) (Eq y0 y1) = return [(x0, y0), (x1, y1)] <|> return [(x0, y1), (x1, y0)]
atoms2eqs _ _ = End []

lits2eqs :: Form -> Form -> BT [(Term, Term)]
lits2eqs (Not f) (Not g) = atoms2eqs f g
lits2eqs f g = atoms2eqs f g

insertAll :: (Ord a) => [a] -> [a] -> [a]
insertAll [] ys = ys
insertAll (x : xs) ys = L.insert x $ insertAll xs ys

breakList :: [a] -> BT (a, [a])
breakList [] = End []
breakList (x : xs) = return (x, xs)

uniLit :: UniMode -> Bnd -> Form -> Form -> BT Bnd
uniLit um b (Eq x0 x1) (Eq y0 y1) =
  uniForm um b (Eq x0 x1, Eq y0 y1) <|> uniForm um b (Eq x0 x1, Eq y1 y0)
uniLit um b (Not (Rel r xs)) (Not (Rel s ys)) = uniForm um b (Rel r xs, Rel s ys)
uniLit um b (Rel r xs) (Rel s ys) = uniForm um b (Rel r xs, Rel s ys)
uniLit _ _ _ _ = End []

super :: Term -> Term -> [Form] -> [Form] -> [Form] -> [(Term, Term)] -> Bnd -> BT Bnd
super x y fs gs hs eqs b =
  ( do (f, fs') <- pluck fs
       [_] <- mapFilterBT (uniLit Lax b f) hs
       return (f, fs') ) !>=
    (\ (f, fs') -> pick hs >>= uniLit Lax b f >>= super x y fs' gs hs eqs) $
    ( do (eq, eqs') <- pluck eqs
         failsBT $ eq2eqs eq
         failsBT $ uniNonFun Lax b eq
         return (eq, eqs') ) !>=
      (\ (eq, eqs') -> rewrite x y eq b >>= super x y fs gs hs eqs') $
      ( do (eq, eqs') <- pluck eqs
           failsBT $ rewrite x y eq b
           failsBT $ uniNonFun Lax b eq
           return (eq, eqs') ) !>=
        ( \ (eq, eqs') -> do
             eqs'' <- eq2eqs eq
             super x y fs gs hs (insertAll eqs'' eqs') b ) $
        ( do (eq, eqs') <- pluck eqs
             failsBT $ eq2eqs eq
             failsBT $ rewrite x y eq b
             return (eq, eqs') ) !>=
          (\ (eq, eqs') -> uniNonFun Lax b eq >>= super x y fs gs hs eqs') $
          ( do (g, gs') <- pluck gs
               [_] <- mapFilterBT (lits2eqs g) hs
               return (g, gs') ) !>=
            ( \ (g, gs') -> do
                eqs' <- pick hs >>= lits2eqs g
                super x y fs gs' hs (insertAll eqs' eqs) b ) $
            breakList fs !>=
              (\ (f, fs') -> pick hs >>= uniLit Lax b f >>= super x y fs' gs hs eqs) $
              breakList gs !>=
                (\ (g, gs') -> do
                   eqs' <- pick hs >>= lits2eqs g
                   super x y fs gs' hs (insertAll eqs' eqs) b ) $
                breakList eqs !>=
                  ( \ (eq, eqs') ->
                      ( do eqs'' <- eq2eqs eq
                           super x y fs gs hs (insertAll eqs'' eqs') b ) <|>
                      (rewrite x y eq b >>= super x y fs gs hs eqs') <|>
                      uniNonFun Lax b eq >>= super x y fs gs hs eqs' ) $
                  return b

uniNonFun :: UniMode -> Bnd -> (Term, Term) -> BT Bnd
uniNonFun um b (Fun _ _, _) = End []
uniNonFun um b (_, Fun _ _) = End []
uniNonFun um b xy = uniTerm um b xy

superSearch :: Form -> Form -> Form -> BT (Term, Term, [Goal], [Goal], [Form], Ctx)
superSearch pr0 pr1 h = do
  (hs, k0, c0) <- breakConc (h, 0) blank
  (f, g) <- shuffle pr0 pr1
  (fgls, c1) <- breakPrem (f, k0) c0
  (gl, fgls') <- pluck fgls
  ((Eq x y, k1), c2) <- stayOrFlip gl c1
  (ggls, c3) <- breakPrem (g, k1) c2
  let fs = L.map fst fgls'
  let gs = L.map fst ggls
  b <- commit $ super x y fs gs hs [] (binding c3)
  return (x, y, fgls', ggls, hs, c3 {binding = b})

superposition :: Form -> Form -> Form -> BT Ctx
superposition f g h = do
  (x, y, fgls, ggls, hs, c) <- commit $ superSearch f g h
  let fs = L.map fst fgls
  let gs = L.map fst ggls
  let bl = HM.toList $ binding c
  c' <- foldM (super0 hs) c fgls
  foldM (super1 x y hs) c' ggls

commit :: BT a -> BT a
commit (End _) = End []
commit (Pass ts x _) = Pass ts x $ End []

super0 :: [Form] -> Ctx -> Goal -> BT Ctx
super0 hs c (f, k) = commit (do {h <- pick hs; appAx Exact (f, h, k) c})

super1 :: Term -> Term -> [Form] -> Ctx -> Goal -> BT Ctx
super1 x y hs c (Not g, k) = do
  Not h <- pick hs
  ((_, _, k1), c1) <- appNotLR (Not g, Not h, k) c
  ((_, k2), c2) <- appEqSL (Eq x y, k1) c1
  super2 y x (h, g, k2) c2
super1 x y hs c (g, k) = do
  h <- pick hs
  super2 x y (g, h, k) c

super2 :: Term -> Term -> PrvGoal -> Ctx -> BT Ctx
super2 x y pg c =
  ( do (eg0, eg1, c') <- appEqC pg c
       foldM (super3 x y) c' [eg0, eg1] ) <|>
  ( do (egs, c') <- appRelC pg c
       foldM (super3 x y) c' egs )

super3 :: Term -> Term -> Ctx -> EqGoal -> BT Ctx
super3 x y c (x', y', k) =
  appEqRExact (Eq x' y', k) c <!>
  appAx Exact (Eq x y, Eq x' y', k) c <!>
  ( do (egs, c') <- appFunC (x', y', k) c
       foldM (super3 x y) c' egs )

resolve :: Form -> Form -> Form -> BT Ctx
resolve pr0 pr1 h = do
   (f, g) <- shuffle pr0 pr1
   (hs, k, c) <- breakConc (h, 0) blank
   (i_, j_, c0_) <- appCut (Rel "" [], k) c
   (gls0_, c1_) <- breakPrem (f, i_) c0_
   (gls1_, c2_) <- breakPrem (g, j_) c1_
   a <- pluck (L.map fst gls0_) >>= breakNot . fst
   (i, j, c0) <- appCut (Not a, k) c
   (gls0, c1) <- breakPrem (f, i) c0
   c2 <- litsMap gls0 (Not a : hs) c1
   ((_, j'), c3) <- appNotL (Not a, j) c2
   (gls1, c4) <- breakPrem (g, j') c3
   litsMap gls1 (a : hs) c4

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
  then ( do ((f', k'), c') <- appFaL Nothing (f, k) c 
            pairSolve (f', g, k') c' ) <|>
       ( do (_, (f', k'), c') <- appExL (f, k) c
            pairSolve (f', g, k') c' )
  else
    if pointLessQuant g
    then ( do (_, (g', k'), c') <- appFaR (g, k) c 
              pairSolve (f, g', k') c' ) <|>
         ( do ((g', k'), c') <- appExR Nothing (g, k) c
              pairSolve (f, g', k') c' ) 
    else
      (appNots pg c <|> appFaRL Same pg c <|> appExLR Same pg c) !>=
        uncurry pairSolve $
        (appImpLR pg c <|> appIffRL pg c) !>=
          (\ (pg0, pg1, c') -> pairSolve pg0 c' >>= pairSolve pg1) $
          appOrR (g, k) c !>=
            (\ (gs, k', c') -> do
               (gls, c'') <- appOrL (f, k') c'
               pairSolveOr gls gs c'' ) $
            appAndL (f, k) c !>=
              (\ (fs, k', c') -> do
                 (gls, c'') <- appAndR (g, k') c'
                 pairSolveAnd fs gls c'' ) $
              (matchAtom Exact pg c <!> matchAtom Pars pg c)

breakNot :: Form -> BT Form
breakNot (Not f) = return f
breakNot _ = End ["break-not"]

isNegRefl :: Form -> Bool
isNegRefl (Not (Eq x y)) = x == y
isNegRefl _ = False

pluckBy :: (a -> Bool) -> [a] -> BT [a]
pluckBy p [] = End ["pluck fail"]
pluckBy p (x : xs) =
  if p x
    then return xs
    else do xs' <- pluckBy p xs
            return (x : xs')

appEqRExact :: Goal -> Ctx -> BT Ctx
appEqRExact (Eq x y, p) c = do
  let b = binding c
  guard (appBndTerm b x == appBndTerm b y)
  ps <- bindPrf p (EqR_ x) (proofs c)
  return $ c {proofs = ps}
appEqRExact _ _ = End ["app-eq-R-X-other"]

appAx :: UniMode -> PrvGoal -> Ctx -> BT Ctx
appAx s (f, g, k) c = do
  b <- uniForm s (binding c) (f, g)
  ps <- bindPrf k (Ax_ f) (proofs c)
  return $ c {binding = b, proofs = ps}

matchLit :: UniMode -> PrvGoal -> Ctx -> BT Ctx
matchLit um pg c =
  appNotLR pg c !>=
    uncurry (matchAtom um) $
    matchAtom um pg c

matchAtom :: UniMode -> PrvGoal -> Ctx -> BT Ctx
matchAtom um (f, g, k) c =
  appAx um (f, g, k) c <|>
  ( do ((f', k'), c') <- appEqSL (f, k) c
       appAx um (f', g, k') c' )


appEqR :: UniMode -> Goal -> Ctx -> BT Ctx
appEqR s (Eq x y, k) c = do
  b <- uniTerm s (binding c) (x, y)
  ps <- bindPrf k (EqR_ x) (proofs c)
  return $ c {binding = b, proofs = ps}
appEqR _ _ _ = End ["Not an equation"]

appEqSL :: Goal -> Ctx -> BT (Goal, Ctx)
appEqSL (Eq x y, k) c =
  let m = fresh c in
  do ps <- bindPrf k (EqSL_ x y m) (proofs c)
     return ((Eq y x, m), c {fresh = m + 1, proofs = ps})
appEqSL _ _ = End ["eq-symm-L"]

tryAppFaL :: Goal -> Ctx -> BT (Goal, Ctx)
tryAppFaL gl c = appFaL Nothing gl c <!> return (gl, c)

appFaR :: Goal -> Ctx -> BT ([Term], Goal, Ctx)
appFaR (Fa vs f, k) c =
  let (m, xs) = listPars (fresh c) vs in
  do vxs <- zipM vs xs
     ps' <- bindPrf k (FaR_ vs (fresh c) f m) (proofs c)
     let c' = c {fresh = m + 1, proofs = ps'} in
       return (xs, (substForm vxs f, m), c')
appFaR (f, k) _ = End []

appFaL :: Maybe [Term] -> Goal -> Ctx -> BT (Goal, Ctx)
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
appFaL _ _ _ = End []

appCut :: Goal -> Ctx -> BT (Int, Int, Ctx)
appCut (f, k) c =
  let m = fresh c in
  do ps <- bindPrf k (Cut_ f m $ m + 1) (proofs c)
     return (m, m + 1, c {fresh = m + 2, proofs = ps})

appExL :: Goal -> Ctx -> BT ([Term], Goal, Ctx)
appExL (Ex vs f, k) c =
  let (m, xs) = listPars (fresh c) vs in
  do vxs <- zipM vs xs 
     ps' <- bindPrf k (ExL_ vs (fresh c) f m) (proofs c)
     let c' = c {fresh = m + 1, proofs = ps'} 
     return (xs, (substForm vxs f, m), c')
appExL (f, _) _ = End []

appExR :: Maybe [Term] -> Goal -> Ctx -> BT (Goal, Ctx)
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
appExR _ _ _ = End []

type Goals_ = ([Form], [Prf] -> Prf)

appAndR :: Goal -> Ctx -> BT ([Goal], Ctx)
appAndR (And fs, p) c =
  let (m, pfs) = getGoals (fresh c) fs in
  do ps <- bindPrf p (AndR_ pfs) (proofs c)
     let c' = c {fresh = m, proofs = ps} in
       return (pfs, c')
appAndR _ _ = End ["app-And-R"]

appBotL :: Goal -> Ctx -> BT Ctx
appBotL (Or [], k) c = do
  ps <- bindPrf k (OrL_ []) (proofs c)
  return $ c {proofs = ps}
appBotL _ _ = End []

appOrL :: Goal -> Ctx -> BT ([Goal], Ctx)
appOrL (Or fs, p) c =
  let (m, pfs) = getGoals (fresh c) fs in
  do ps <- bindPrf p (OrL_ pfs) (proofs c)
     let c' = c {fresh = m, proofs = ps} in
       return (pfs, c')
appOrL _ _ = End ["app-or-L"]

appAndL :: Goal -> Ctx -> BT ([Form], Int, Ctx)
appAndL (And fs, k) c =
  let m = fresh c in
  do ps <- bindPrf k (AndL_ fs fs m) (proofs c)
     return (fs, m, c {fresh = m + 1, proofs = ps})
appAndL (f, _) _ = End ["app-and-L : " <> ppForm f]

appOrR :: Goal -> Ctx -> BT ([Form], Int, Ctx)
appOrR (Or fs, k) c =
  let m = fresh c in
  do ps <- bindPrf k (OrR_ fs fs m) (proofs c)
     return (fs, m, c {fresh = m + 1, proofs = ps})
appOrR (f, _) _ = End ["app-or-R : " <> ppForm f]

appImpL :: Goal -> Ctx -> BT (Goal, Goal, Ctx)
appImpL (Imp f g, k) c =
  let m = fresh c in
  do ps <- bindPrf k (ImpL_ f g m $ m + 1) (proofs c)
     return ((f, m), (g, m + 1), c {fresh = m + 2, proofs = ps})
appImpL (f, _) _ = End ["app-imp-L : " <> ppForm f]

appIffR :: Goal -> Ctx -> BT (Goal, Goal, Ctx)
appIffR (Iff f g, k) c =
  let m = fresh c in
  do ps <- bindPrf k (IffR_ f g m $ m + 1) (proofs c)
     return ((Imp f g, m), (Imp g f, m + 1), c {fresh = m + 2, proofs = ps})
appIffR _ _ = End []

appIffLR :: Goal -> Ctx -> BT (Goal, Ctx)
appIffLR (Iff f g, k) c =
  let m = fresh c in
  do ps <- bindPrf k (IffLR_ f g m) (proofs c)
     return ((Imp g f, m), c {fresh = m + 1, proofs = ps})
appIffLR _ _ = End []

appIffLO :: Goal -> Ctx -> BT (Goal, Ctx)
appIffLO (Iff f g, k) c =
  let m = fresh c in
  do ps <- bindPrf k (IffLO_ f g m) (proofs c)
     return ((Imp f g, m), c {fresh = m + 1, proofs = ps})
appIffLO _ _ = End []

appImpRA :: Goal -> Ctx -> BT (Goal, Ctx)
appImpRA (Imp f g, k) c =
  let m = fresh c in
  do ps <- bindPrf k (ImpRA_ f g m) (proofs c)
     return ((f, m), c {fresh = m + 1, proofs = ps})
appImpRA (f, _) _ = End ["app-imp-R-ante : " <> ppForm f]

appImpRC :: Goal -> Ctx -> BT (Goal, Ctx)
appImpRC (Imp f g, k) c =
  let m = fresh c in
  do ps <- bindPrf k (ImpRC_ f g m) (proofs c)
     return ((g, m), c {fresh = m + 1, proofs = ps})
appImpRC (f, _) _ = End ["app-imp-R-cons : " <> ppForm f]

appFunC :: EqGoal -> Ctx -> BT ([EqGoal], Ctx)
appFunC (Fun f xs, Fun g ys, k) c =
  let m = fresh c in
  do guard (f == g)
     xys <- zipM xs ys
     let (n, egs) = listEqGoals m xys in
       do ps <- bindPrf k (FunC_ f egs) (proofs c)
          return (egs, c {fresh = n,  proofs = ps})
appFunC _ _ = End []

appEqC :: PrvGoal -> Ctx -> BT (EqGoal, EqGoal, Ctx)
appEqC (Eq x0 x1, Eq y0 y1, k) c =
  let m = fresh c in
  let eg0 = (x0, y0, m) in
  let eg1 = (x1, y1, m + 1) in
  do ps <- bindPrf k (EqC_ eg0 eg1) (proofs c)
     return (eg0, eg1, c {fresh = m + 2, proofs = ps})
appEqC _ _ = End []

appRelC :: PrvGoal -> Ctx -> BT ([EqGoal], Ctx)
appRelC (Rel r xs, Rel s ys, k) c =
  let m = fresh c in
  do guard (r == s)
     xys <- zipM xs ys
     let (n, egs) = listEqGoals m xys in
       do ps <- bindPrf k (RelC_ r egs) (proofs c)
          return (egs, c {fresh = n,  proofs = ps})
appRelC _ _ = End []

listEqGoals :: Int -> [(Term, Term)] -> (Int, [EqGoal])
listEqGoals k [] = (k, [])
listEqGoals k ((x, y) : xys) =
  let (m, egs) = listEqGoals (k + 1) xys in
  (m, (x, y, k) : egs)

appIffRL :: PrvGoal -> Ctx -> BT (PrvGoal, PrvGoal, Ctx)
appIffRL (f, g, k) c0 = do
  ((g0, m), (g1, n), c1) <- appIffR (g, k) c0
  ((f0, m'), c2) <- appIffLO (f, m) c1
  ((f1, n'), c3) <- appIffLR (f, n) c2
  return ((f0, g0, m'), (f1, g1, n'), c3)

appImpLR :: PrvGoal -> Ctx -> BT (PrvGoal, PrvGoal, Ctx)
appImpLR (f, g, k) c = do
  ((f0, m), (f1, n), c') <- appImpL (f, k) c
  ((g0, m'), c'') <- appImpRA (g, m) c'
  ((g1, n'), c''') <- appImpRC (g, n) c''
  return ((g0, f0, m'), (f1, g1, n'), c''')

pick :: [a] -> BT a
pick [] = End ["pick-empty"]
pick (x : xs) = return x <|> pick xs

pluck :: [a] -> BT (a, [a])
pluck [] = End ["pluck empty fail"]
pluck (x : xs) =
  altBT
    (return (x, xs))
    ( do (x', xs') <- pluck xs
         return (x', x : xs') )

data InstMode = Same | Perm
  deriving (Eq)

data UniMode = Lax | Pars | ParFvs | Exact
  deriving (Eq)

litsMap :: [Goal] -> [Form] -> Ctx -> BT Ctx
litsMap [] _ c = return c
litsMap ((f, k) : gls) gs c =
  let gl = (f, k) in
  ( appBotL gl c <|>
    (appNotL gl c >>= uncurry appEqRExact) <|>
    ( do g <- pick gs
         matchLit Exact (f, g, k) c ) ) !>=
    litsMap gls gs $
    (appNotL gl c >>= uncurry (appEqR Lax) >>= litsMap gls gs) <|>
    ( do g <- pick gs
         matchLit Lax (f, g, k) c >>= litsMap gls gs )

nl :: IO ()
nl = Prelude.putStr "\n"

main :: IO ()
main = do
  (tptp : tstp : _) <- getArgs
  tptp_afs <- parseName tptp
  tstp_afs <- sortAfs <$> parseName tstp
  let hs = L.foldl addHyp (HM.empty, S.empty) tptp_afs
  Prelude.putStr $ tptp ++ "\n"
  mapM_ putAnForm tptp_afs
  nl
  Prelude.putStr $ tstp ++ "\n"
  mapM_ putAnForm tstp_afs
  nl
  foldM_ elabIO hs tstp_afs
  Prelude.putStr "Elab finished.\n\n"
