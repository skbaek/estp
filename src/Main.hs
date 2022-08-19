{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
-- {-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use second" #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use list comprehension" #-}

module Main where

import Types
import Basic
import PP
import App
import Parse
import Check
import BT
import Sat
import Lem
import Control.Monad as M (guard, MonadPlus, foldM, foldM_, (>=>), mzero)
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative ( Alternative((<|>)) )
import System.Environment
import Data.List as L
    (filter, null, find, map, length, foldl, elem, partition, all, any, concat, (\\),
    elemIndex, insert, sortBy, concatMap, unzip, nub, splitAt, delete, reverse )
import Data.Text as T
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList,
  union, unions, difference, disjoint, (\\), null, lookupMin, isSubsetOf )
import qualified Data.Text.Lazy as TL (toStrict, intercalate)
import Data.Text.IO as TIO
import Data.Text.Read as TR
import Data.Functor ((<&>))
import Data.Map as HM (
  Map, insert, lookup, empty, map, member, mapMaybe, mapKeys, toList, fromListWithKey,
  delete, notMember, findWithDefault, partitionWithKey, isSubmapOf, filterWithKey )
import Debug.Trace (trace)
import Data.Maybe as MB ( isNothing, fromMaybe, mapMaybe )
import qualified Data.Bifunctor as DBF
import qualified System.Posix.Internals as Prelude
import qualified Data.IntMap as IM

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

top :: Form
top = And []

bot :: Form
bot = Or []

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
  if top `elem` gs then top
    else
      case L.filter (/= bot) gs of
        [g] -> g
        hs -> Or hs
tfsimp (And fs) =
  let gs = L.map tfsimp fs in
  if bot `elem` gs then bot
    else
      case L.filter (/= top) gs of
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
    (Or [], _) -> top
    (_, And []) -> top
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

vmTerm :: VR -> Term -> Maybe Term
vmTerm vm (Fun f xs) = Fun f <$> mapM (vmTerm vm) xs
vmTerm vm (Bv v) = Bv <$> lookupVR vm v
vmTerm vm x = return x

delBij :: (Ord k, Ord v) => k -> Bij k v -> Maybe (Bij k v)
delBij k (bj, jb) = do
  v <- HM.lookup k bj
  let bj' = HM.delete k bj
  let jb' = HM.delete v jb
  return (bj', jb')

delsBij :: (Ord k, Ord v) => [k] -> Bij k v -> Maybe (Bij k v)
delsBij [] b = return b
delsBij (k : ks) b = delBij k b >>= delsBij ks

tryDelBij :: (Ord k, Ord v) => k -> Bij k v -> Bij k v
tryDelBij k m@(kv, vk) =
  case HM.lookup k kv of
    Nothing -> m
    Just v -> (HM.delete k kv, HM.delete v vk)

tryDelsBij :: (Ord k, Ord v) => [k] -> Bij k v -> Bij k v
tryDelsBij ks kv = L.foldl (flip tryDelBij) kv ks

prn :: Int -> (Form, Form) -> IO Prf
prn k (f, g) =
  if f == g
  then return $ Ax f
  else prnRec k (f, g)

prnRec :: Int -> (Form, Form) -> IO Prf
prnRec k (Not f, Not g) = do
  p <- prn k (g, f)
  return $ notLR f g p
prnRec k (Or fs, Or gs) = do
  fgs <- zipM fs gs
  ps <- mapM (prn k) fgs
  fps <- zipM fs ps
  return $ OrR gs gs $ OrL fps
prnRec k (And fs, And gs) = do
  fgs <- zipM fs gs
  ps <- mapM (prn k) fgs
  gps <- zipM gs ps
  return $ AndL fs fs $ AndR gps

prnRec k (Imp fl fr, Imp gl gr) = do
  pl <- prn k (gl, fl)
  pr <- prn k (fr, gr)
  return $ ImpL fl fr (ImpRA gl gr pl) (ImpRC gl gr pr)

prnRec k (Iff fl fr, Iff gl gr) = do
  pol <- prn k (gl, fl) -- pol : gl |- fl
  por <- prn k (fr, gr) -- por : fr |- gr
  prr <- prn k (gr, fr) -- prr : gr |- fr
  prl <- prn k (fl, gl) -- prl : fl |- gl
  let po = IffLO fl fr $ ImpL fl fr pol por -- po : gl |- gr
  let pr = IffLR fl fr $ ImpL fr fl prr prl -- pr : gr |- gl
  return $ IffR gl gr (impR gl gr po) (impR gr gl pr)

prnRec k (Fa vs f, Fa ws g) = do
  let (k', xs) = listPars k ws
  vxs <- zipM vs xs
  wxs <- zipM ws xs
  let f' = substForm vxs f
  let g' = substForm wxs g
  p <- prn k' (f', g')
  return $ FaR ws k g $ FaL vxs f p

prnRec k (Ex vs f, Ex ws g) = do
  let (k', xs) = listPars k vs
  vxs <- zipM vs xs
  wxs <- zipM ws xs
  let f' = substForm vxs f
  let g' = substForm wxs g
  p <- prn k' (f', g')
  return $ ExL vs k f $ ExR wxs g p

prnRec _ _ = mzero

vmForm :: VR -> Form -> Maybe Form
vmForm gm (Rel r xs) = Rel r <$> mapM (vmTerm gm) xs
vmForm gm (Eq x y) = do
  [x', y'] <- mapM (vmTerm gm) [x, y]
  return $ Eq x' y'
vmForm gm (Not f) = Not <$> vmForm gm f
vmForm gm (Or fs) = Or <$> mapM (vmForm gm) fs
vmForm gm (And fs) = And <$> mapM (vmForm gm) fs
vmForm gm (Imp f g) = do
  f' <- vmForm gm f
  g' <- vmForm gm g
  return $ Imp f' g'
vmForm gm (Iff f g) = do
  f' <- vmForm gm f
  g' <- vmForm gm g
  return $ Iff f' g'
vmForm vm (Fa vs f) = do
  vs' <- mapM (lookupVR vm) vs
  f' <- vmForm vm f
  return $ Fa vs' f'
vmForm vm (Ex vs f) = do
  vs' <- mapM (lookupVR vm) vs
  f' <- vmForm vm f
  return $ Ex vs' f'

mergeBij :: (Ord a, Ord b) => Bij a b -> Bij a b -> Maybe (Bij a b)
mergeBij m n = foldM (\ n_ (x_, y_) -> addBij x_ y_ n_) n (HM.toList $ fst m)

flatOr :: [Form] -> [Form]
flatOr [] = []
flatOr (Or [] : fs) = flatOr fs
flatOr (Or fs : gs) = flatOr fs ++ flatOr gs
flatOr (f : fs) = f : flatOr fs

flatAnd :: [Form] -> [Form]
flatAnd [] = []
flatAnd (And [] : fs) = flatAnd fs
flatAnd (And fs : gs) = flatAnd fs ++ flatAnd gs
flatAnd (f : fs) = f : flatAnd fs

fxdTerm :: VR -> Term -> Bool
fxdTerm vm x = isJust $ vmTerm vm x

tt :: Text -> Maybe ()
tt t = trace (unpack t) (return ())

appAny :: (a -> Maybe a) -> [a] -> Maybe [a]
appAny f [] = nt
appAny f (x : xs) = ((: xs) <$> f x) <|> ((x :) <$> appAny f xs)

addLPs :: Text -> Text -> [([Text], [Text])] -> Maybe [([Text], [Text])]
addLPs v w [] = return []
addLPs v w ((vs, ws) : l) =
  case (deleteOnce v vs, deleteOnce w ws) of
    (Just vs', Just ws') -> ((vs', ws') :) <$> addLPs v w l
    (Nothing, Nothing) -> ((vs, ws) :) <$> addLPs v w l
    (_, _) -> nt

addVR :: Text -> Text -> VR -> Maybe VR
addVR v w (lps, bij) = do
  lps' <- addLPs v w lps
  bij' <- addBij v w bij
  return (lps', bij')

ppPair :: (Form, Form) -> Text
ppPair (f, g) = ppForm f <> "\n<-|->\n" <> ppForm g

orig :: Form -> Form -> IO Prf
orig f g = do
  -- pt $ "Source =\n" <> ppForm f <> "\n"
  -- pt $ "Target =\n" <> ppForm g <> "\n"
  (vr, _) <- searchOrig (emptyVR, [(f, g)])
  -- pt $ "VR found = " <> ppVR vr <> "\n"
  uvm 0 (snd vr) f g

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

blank :: Ctx
blank = Ctx {fresh = 1, binding = HM.empty, proofs = HM.empty }

isRdef :: Text -> Form -> Bool
isRdef r (Fa _ (Iff (Rel s _) _)) = r == s
isRdef r (Iff (Rel s _) _) = r == s
isRdef _ _ = False

putText :: Text -> IO ()
putText tx = Prelude.putStr (unpack tx)

checkElab :: Seq -> Form -> Elab -> IO ()
checkElab sq g (Plab p) =
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

-- elabIO' :: HM.Map Text Form -> AnForm -> IO (HM.Map Text Form)
-- elabIO' nsq (Af n f a) = do
--   print $ "Elaborating step = " <> n
--   elab' nsq (Af n f a)
--   return $ HM.insert n f nsq

zt :: Term
zt = Fun "" []

appBndTerm :: BndMode -> Bnd -> Term -> Term
appBndTerm Mid b (Fv k) =
  case HM.lookup k b of
    Nothing -> Fv k
    Just x -> appBndTerm Mid b x
appBndTerm End b (Fv k) =
  case HM.lookup k b of
    Nothing -> zt
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
collectPrf_ ps Sorry_ = return Asm
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
appBndPrf b Asm = Asm
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
appBndPrf b (Mrk s p) = Mrk s (appBndPrf b p)

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

gFunFunctor :: Gterm -> Maybe Text
gFunFunctor (Gfun t []) = return t
gFunFunctor _ = Nothing

-- inferOut' :: Text -> [Form] -> Form -> IO ()
-- inferOut' "superposition" [f, g] h         = superpose f g h
-- inferOut' "forward_demodulation" [f, g] h  = superpose f g h
-- inferOut' "backward_demodulation" [f, g] h = superpose f g h
-- inferOut' _ _ _ = return ()

inferOut :: Text -> [Form] -> Form -> IO Prf
inferOut "superposition" [f, g] h         = superpose f g h
inferOut "forward_demodulation" [f, g] h  = superpose f g h
inferOut "backward_demodulation" [f, g] h = superpose f g h
inferOut "negated_conjecture" [f] g = guard (f == g) >> return (Ax f)
inferOut "factoring" [f] g = efactor (Just True) f g
inferOut "duplicate_literal_removal" [f] g = efactor (Just True) f g
inferOut "equality_resolution" [f] g = efactor (Just False) f g
inferOut "trivial_inequality_removal" [f] g = efactor nt f g
inferOut "subsumption_resolution" [f, g] h = resolve f g h
inferOut "resolution" [f, g] h = resolve f g h
inferOut "definition_folding" (f : fs) g = definFold fs f g
inferOut "definition_unfolding" (f : fs) g = dunfold fs f g
inferOut "pure_predicate_removal" [f] g = ppr f g
inferOut "flattening" [f] g = flat f g
inferOut "equality_factoring" [f] g = eqfactor f g
inferOut "rectify" [f] g            = pairSolve f g
inferOut "avatar_component_clause" [f] g = avatarComp f g
inferOut tx fs g = do
  c <- infer tx fs g
  cast $ extractPrf c

infer :: Text -> [Form] -> Form -> IO Ctx
infer "skolemisation" (f : fs) g = skolemize fs (f, g, 0) blank
infer "cnf_transformation" [f] g = cast $ cnfTrans f g
infer "unused_predicate_definition_removal" [f] g = cast $ updr (f, g, 0) blank
infer "avatar_split_clause" (f : fs) g   = avatarSplit fs f g
infer "avatar_contradiction_clause" [f] g = avatarContra f g
infer r fs g = et $ "No inference : " <> r

df1rw :: [Form] -> Form -> IO (Form, Form)
df1rw [] _ = mzero
df1rw (Fa vs (Iff l r) : es) g =
  if g == l
  then return (Fa vs (l <=> r), r)
  else df1rw es g
df1rw (Iff l r : es) g =
  if g == l
  then return (l <=> r, r)
  else df1rw es g
df1rw (e : es) f = et $ "non-definition : " <> ppForm e

dfj :: [Form] -> [Form] -> IO (Form, [Form])
dfj es (f : fs) = (DBF.second (: fs) <$> df1 es f) <|> (DBF.second (f :) <$> dfj es fs)
dfj _ [] = mzero

specs :: VM -> [(Term, Term)] -> Maybe VM
specs vm [] = return vm
specs vm ((x, y) : xys) = do
  vm' <- spec vm x y
  specs vm' xys

spec2p :: VM -> Term -> Term -> Maybe VM
spec2p vm (Bv v) y@(Par k) = do
  case HM.lookup v vm of
    Just x -> do
      guard $ x == y
      return vm
    _ -> return $ HM.insert v y vm
spec2p vm (Bv v) y@(Fun f []) = do
  case HM.lookup v vm of
    Just x -> do
      guard $ x == y
      return vm
    _ -> return $ HM.insert v y vm
spec2p vm (Fun f xs) (Fun g ys) = do
  guard $ f == g
  xys <- zipM xs ys
  foldM (\ vm_ (x_, y_) -> spec2p vm_ x_ y_) vm xys
spec2p vm x y = do
  guard (x == y)
  return vm

spec :: VM -> Term -> Term -> Maybe VM
spec vm (Bv v) y = do
  case HM.lookup v vm of
    Just x -> do
      guard $ x == y
      return vm
    _ -> return $ HM.insert v y vm
spec vm x@(Fun f xs) y@(Fun g ys) = do
  guard $ f == g
  xys <- zipM xs ys
  foldM (\ vm_ (x_, y_) -> spec vm_ x_ y_) vm xys
spec vm x y = do
  guard (x == y)
  return vm


df1 :: [Form] -> Form -> IO (Form, Form)
df1 es (Ex vs f) = DBF.second (Ex vs) <$> df1 es f
df1 es (Fa vs f) = DBF.second (Fa vs) <$> df1 es f
df1 es (And fs)  = DBF.second And <$> dfj es fs
df1 es (Or fs)   = DBF.second Or <$> dfj es fs
df1 es (Not f)   = DBF.second Not <$> df1 es f
df1 es (Imp f g) = (DBF.second (`Imp` g) <$> df1 es f) <|> (DBF.second (Imp f) <$> df1 es g)
df1 es (Iff f g) = (DBF.second (`Iff` g) <$> df1 es f) <|> (DBF.second (Iff f) <$> df1 es g)
df1 _ (Eq _ _) = mzero
df1 es g@(Rel _ _) = df1rw es g

dff :: [Form] -> Form -> Form -> IO [(Form, Form)]
dff es f g =
  if f == g
  then return []
  else do (e, g') <- df1 es g
          ((e, g') :) <$> dff es f g'

type USOL = [Term]

duts :: [(Dir, Form)] -> [Term] -> Maybe [Term]
duts es (x : xs) = ((: xs) <$> dut es x) <|> ((x :) <$> duts es xs)
duts es [] = mzero

dut :: [(Dir, Form)] -> Term -> Maybe Term
dut es x@(Fun f xs) = (Fun f <$> duts es xs) <|> first (dutt x) es
dut es x = first (dutt x) es

dutt :: Term -> (Dir, Form) -> Maybe Term
dutt x (Obv, Fa vs (Eq a b)) = do
  vm <- cast $ spec HM.empty a x
  return $ subt vm b
dutt x (Rev, Fa vs (Eq a b)) = do
  vm <- cast $ spec HM.empty b x
  return $ subt vm a
dutt x (Obv, Eq a b)
  | x == a = return b
  | otherwise = nt
dutt x (Rev, Eq a b)
  | x == b = return a
  | otherwise = nt
dutt _ (dr, f) = et $ "non-equation : " <> ppForm f <> "\n"

du :: [(Dir, Form)] -> Term -> Term -> IO USOL
du es x y = do
  if x == y
  then return []
  else do x' <- cast $ dut es x
          (x' :) <$> du es x' y

dufAux :: [(Dir, Form)] -> Form -> (Form, [Form]) -> IO ([Form], (Form, [USOL]))
dufAux es g (f, fs) = (fs,) <$> duf es f g

dufs :: [(Dir, Form)] -> [Form] -> [Form] -> IO ([Form], [(Form, [USOL])])
dufs es = mapAccumM (\ fs_ g_ -> first (dufAux es g_) (plucks fs_))

duf :: [(Dir, Form)] -> Form -> Form -> IO (Form, [USOL])
duf es (Not f)   (Not g)   = DBF.first Not <$> duf es f g
duf es (Ex vs f) (Ex ws g) = DBF.first (Ex vs) <$> duf es f g
duf es (Fa vs f) (Fa ws g) = DBF.first (Fa vs) <$> duf es f g
duf es (And fs)  (And gs)  = do
  (fs', xss) <- unzip . snd <$> dufs es fs gs
  return (And fs', L.concat xss)
duf es (Or fs)   (Or gs)   = do
  (fs', xss) <- unzip . snd <$> dufs es fs gs
  return (Or fs', L.concat xss)
duf es (Imp e f) (Imp g h) = do
  (e', xsl) <- duf es e g
  (f', xsr) <- duf es f h
  return (Imp e' f', xsl ++ xsr)
duf es (Iff e f) (Iff g h) = do
  (e', xsl) <- duf es e g
  (f', xsr) <- duf es f h
  return (Iff e' f', xsl ++ xsr)
duf es (Eq w x)  (Eq y z)  =
  ( do ws <- du es w y
       xs <- du es x z
       return (w === x, [ws, xs]) ) <|>
  ( do xs <- du es x y
       ws <- du es w z
       return (x === w, [xs, ws]) )
duf es (Rel r xs) (Rel s ys) = (Rel r xs,) <$> mapM2 (du es) xs ys
duf _ _ _ = mzero

tsfuns :: [Term] -> Set Text
tsfuns = S.unions . L.map tfuns

tfuns :: Term -> Set Text
tfuns (Fun f xs) = S.insert f $ S.unions $ L.map tfuns xs
tfuns _ = S.empty

fsfuns :: [Form] -> Set Text
fsfuns = S.unions . L.map ffuns

ffuns :: Form -> Set Text
ffuns (Rel _ xs) = S.unions $ L.map tfuns xs
ffuns (Or fs) = S.unions $ L.map ffuns fs
ffuns (And fs) = S.unions $ L.map ffuns fs
ffuns (Eq x y) = S.unions $ L.map tfuns [x, y]
ffuns (Imp f g) = S.unions $ L.map ffuns [f, g]
ffuns (Iff f g) = S.unions $ L.map ffuns [f, g]
ffuns (Not f) = ffuns f
ffuns (Fa _ f) = ffuns f
ffuns (Ex _ f) = ffuns f


overlaps :: Ord a => Set a -> Set a -> Bool
overlaps r s = not $ disjoint r s

headFix :: Set Text -> Term -> Bool
headFix dfs (Fun f _) = S.member f dfs
headFix dfs _ = False

obvEq :: Set Text -> Form -> (Dir, Form)
obvEq dfs f@(Fa vs (Eq a b))
  | headFix dfs a && disjoint dfs (tfuns b) = (Obv, f)
  | headFix dfs b && disjoint dfs (tfuns a) = (Rev, f)
  | otherwise = et "obv-eq : cannot fix direction"
obvEq dfs f@(Eq a b)
  | headFix dfs a && disjoint dfs (tfuns b) = (Obv, f)
  | headFix dfs b && disjoint dfs (tfuns a) = (Rev, f)
  | otherwise = et "obv-eq : cannot fix direction"
obvEq dfs _ = et "obv-eq : non-equation"

dunfold :: [Form] -> Form -> Form -> IO Prf
dunfold es f h = do
  let dfs = ffuns f S.\\ ffuns h
  let des = L.map (obvEq dfs) es
  (f', uss) <- duf des f h
  -- pt "Unfold solution found :\n"
  -- pt $ ppListNl ppUsol uss
  (h', gs) <- dips f' uss
  guardMsg (h == h') "Simultaneous substitution result does not match target"
  -- pt "All interpolants :\n"
  -- pt $ ppListNl ppForm gs
  -- pt $ "f : " <> ppForm f <> "\n"
  -- pt $ "f' : " <> ppForm f' <> "\n"
  pl <- ppuf 0 f f'
  pr <- uips (puf 0 es) f' gs h
  return $ Cut f' (Cut (f <=> f') pl (iffMP f f')) pr

ppufAux :: Int -> [Form] -> [Form] -> IO [(Form, Form, Prf)]
ppufAux _ [] _ = return []
ppufAux k (f : fs) ggs = do
  ((g, p), gs) <- pluckFirst (\ g_ -> (g_,) <$> ppuf k f g_) ggs
  fgps <- ppufAux k fs gs
  return ((f, g, p) : fgps)

ppuf :: Int -> Form -> Form -> IO Prf
ppuf k (Not f) (Not g) = do
  p <- ppuf k f g
  return $ Cut (f <=> g) p $ iffToNotIffNot f g
ppuf k (Or fs) (Or gs) = do
  fgps <- ppufAux k fs gs
  let fgs = L.map (\ (f_, g_, _) -> (f_, g_)) fgps
  let iffps = L.map (\ (f_, g_, p_) -> (f_ <=> g_, p_)) fgps
  cuts iffps <$> cast (iffsToOrIffOr' fgs fs gs)
ppuf k (And fs) (And gs) = do
  fgps <- ppufAux k fs gs
  let fgs = L.map (\ (f_, g_, _) -> (f_, g_)) fgps
  let iffps = L.map (\ (f_, g_, p_) -> (f_ <=> g_, p_)) fgps
  cuts iffps <$> cast (iffsToAndIffAnd' fgs fs gs)
-- puf k es (And fs) (And gs) = do
--   fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> puf k es f_ g_) fs gs
--   cuts fgps <$> cast (iffsToAndIffAnd fs gs)
ppuf k (Imp e f) (Imp g h) = do
  pl <- ppuf k e g -- pl : |- fl <=> gl 
  pr <- ppuf k f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ impCong e f g h
ppuf k (Iff e f) (Iff g h) = do
  pl <- ppuf k e g -- pl : |- fl <=> gl 
  pr <- ppuf k f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ iffCong e f g h
ppuf k (Fa vs f) (Fa ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- ppuf k' f' g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToFaIffFa vs k f g
ppuf k (Ex vs f) (Ex ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- ppuf k' f' g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToExIffEx vs k f g
ppuf _ (Rel r xs) (Rel s ys) = do
  guard $ r == s && xs == ys
  return $ iffRefl (Rel r xs)
ppuf _ (Eq x y) (Eq a b)
  | x == a && y == b = return $ iffRefl (x === y)
  | x == b && y == a = return $ iffRFull (x === y) (a === b) (EqS x y) (EqS a b)
  | otherwise = mzero
ppuf _ f g = mzero

sliceUsols :: [USOL] -> ([Maybe Term], [USOL])
sliceUsols [] = ([], [])
sliceUsols ([] : uss) =
  let (mxs, uss') = sliceUsols uss in
  (nt : mxs, [] : uss')
sliceUsols ((x : xs) : uss) =
  let (mxs, uss') = sliceUsols uss in
  (Just x : mxs, xs : uss')

dips :: Form -> [USOL] -> IO (Form, [Form])
dips f uss =
  if L.all L.null uss
  then return (f, [])
  else do let (mxs, uss') = sliceUsols uss
          ([], g) <- ssubf mxs f
          (h, gs) <- dips g uss'
          return (h, g : gs)

ppMaybe :: (a -> Text) -> Maybe a -> Text
ppMaybe f (Just x) = "Just " <> f x
ppMaybe _ _  = "Nothing"

-- Like StateT but with return tuple swapped
newtype StateM s m a = StateM { runStateM :: s -> m (s, a) }

instance Functor m => Functor (StateM s m) where
    fmap f (StateM x) = StateM $ \s -> fmap (\(s', a) -> (s', f a)) (x s)

instance Monad m => Applicative (StateM s m) where
    pure x = StateM $ \s -> return (s, x)
    StateM f <*> StateM x = StateM $ \s -> do (s', f') <- f s
                                              (s'', x') <- x s'
                                              return (s'', f' x')

-- | Monadic variant of 'mapAccumL'.
mapAccumM :: (Monad m, Traversable t)
          => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumM f s t = runStateM (traverse (\x -> StateM (`f` x)) t) s

ssubts :: [Maybe Term] -> [Term] -> IO ([Maybe Term], [Term])
ssubts mxs [] = return (mxs, [])
ssubts (Just x : mxs) (_ : xs) = DBF.second (x :) <$> ssubts mxs xs
ssubts (_ : mxs) (x : xs) = DBF.second (x :) <$> ssubts mxs xs
ssubts [] (_ : _) = mzero

ssubf :: [Maybe Term] -> Form -> IO ([Maybe Term], Form)
ssubf mxs (Not f) = DBF.second Not <$> ssubf mxs f
ssubf mxs (Imp f g) = do
  (mxs', f') <- ssubf mxs f
  (mxs'', g') <- ssubf mxs' g
  return (mxs'', f' ==> g')
ssubf mxs (Iff f g) = do
  (mxs', f') <- ssubf mxs f
  (mxs'', g') <- ssubf mxs' g
  return (mxs'', f' <=> g')
ssubf mxs (Or fs) = DBF.second Or <$> mapAccumM ssubf mxs fs
ssubf mxs (And fs) = DBF.second And <$> mapAccumM ssubf mxs fs
ssubf mxs (Fa vs f) = DBF.second (Fa vs) <$> ssubf mxs f
ssubf mxs (Ex vs f) = DBF.second (Ex vs) <$> ssubf mxs f
ssubf mxs (Eq x y) = do
  (mxs', [x', y']) <- ssubts mxs [x, y]
  return (mxs', Eq x' y')
ssubf mxs (Rel r xs) = DBF.second (Rel r) <$> ssubts mxs xs

definFold :: [Form] -> Form -> Form -> IO Prf
definFold es f g = do
  -- pt $ "DFF Defins =\n" <> ppListNl ppForm es <> "\n"
  -- pt $ "DFF Source = " <> ppForm f <> "\n"
  -- pt $ "DFF Target = " <> ppForm g <> "\n"
  egs <- dff es f g
  -- pt $ ppListNl (\ (e_, g_) -> ppForm e_ <> "\n================\n" <> ppForm g_ <> "\n\n") egs
  -- pt "Begin use-GM...\n"
  uegs f egs g
  -- pt "use-GM success!\n"

uufs :: Form -> [(Form, Form)] -> Form -> IO Prf
uufs f [] h = do
  guardMsg (f == h) "f and g not equal after rewriting"
  return $ Ax f
uufs f ((e, g) : egs) h = do
  -- pt $ "f to be rewritten : " <> ppForm f <> "\n"
  -- pt $ "equation e : " <> ppForm e <> "\n"
  -- pt $ "interpolant g : " <> ppForm g <> "\n"
  pl <- uuf 0 f e g -- pgh : |- h <=> g
  pr <- uufs g egs h
  return $ Cut g (Cut (f <=> g) pl (IffLO f g $ mp f g)) pr

uips :: (Form -> Form -> IO Prf) -> Form -> [Form] -> Form -> IO Prf
uips pf f [] h = do
  guardMsg (f == h) "f and g not equal after rewriting"
  return $ Ax f
uips pf f (g : gs) h = do
  pl <- pf f g
  pr <- uips pf g gs h  -- pgh : |- h <=> g
  return $ Cut g (Cut (f <=> g) pl (IffLO f g $ mp f g)) pr

uegs :: Form -> [(Form, Form)] -> Form -> IO Prf
uegs f [] h = do
  guardMsg (f == h) "f and g not equal after rewriting"
  return $ Ax f
uegs f ((e, g) : egs) h = do
  pl <- uegs f egs g
  phg <- ueg 0 HM.empty h e g -- pgh : |- h <=> g
  return $ Cut g pl $ Cut (h <=> g) phg (IffLR h g $ mp g h)


ueg :: Int -> VM -> Form -> Form -> Form -> IO Prf
ueg k gm f e g =
  if f == g
  then return $ iffRefl f
  else uegt gm f e g <|> uegr k gm f e g

mapM2 :: (Monad m, Alternative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 f xs ys = zipM xs ys >>= mapM (uncurry f)

foldM2 :: (Monad m, Alternative m) => (a -> b -> c -> m a) -> a -> [b] -> [c] -> m a
foldM2 f x ys zs = do 
  yzs <- zipM ys zs 
  foldM (\ x_ (y_, z_) -> f x_ y_ z_) x yzs

cuts :: [(Form, Prf)] -> Prf -> Prf
cuts [] = id
cuts ((f, p) : fps) = Cut f p . cuts fps

uut :: Term -> Form -> Term -> IO Prf
uut x e y =
  if x == y
  then return $ EqR x
  else uutt x e y <|> uutr x e y

uutr :: Term -> Form -> Term -> IO Prf
uutr (Fun f xs) e (Fun g ys) = do
  guard $ f == g
  xyps <- mapM2 (\ x_ y_ -> (x_, y_,) <$> uut x_ e y_) xs ys
  return $ FunC f xyps
uutr _ _ _ = et "uutr cannot recurse"

uutt :: Term -> Form -> Term -> IO Prf
uutt x (Eq a b) y =
  case (x == a, y == b, x == b, y == a) of
    (True, True, _, _) -> return $ Ax (x === y)
    (_, _, True, True) -> return $ EqS a b
    _ -> mzero
uutt x (Fa vs (Eq a b)) y =
  case (specs HM.empty [(a, x), (b, y)], specs HM.empty [(a, y), (b, x)] ) of
    (Just vm, _) -> do
      vxs <- mapM (\ v_ -> (v_ ,) <$> cast (HM.lookup v_ vm)) vs
      return $ FaL vxs (a === b) $ Ax (x === y)
    (_, Just vm) -> do
      vxs <- mapM (\ v_ -> (v_ ,) <$> cast (HM.lookup v_ vm)) vs
      return $ FaL vxs (a === b) $ EqS y x
    _ -> mzero
uutt _ f _ = et "not an equation"

uuf :: Int -> Form -> Form -> Form -> IO Prf
uuf k f@(Rel r xs) e g@(Rel s ys) = do
  guard $ r == s
  xyps <- mapM2 (\ x_ y_ -> (x_, y_,) <$> uut x_ e y_) xs ys
  -- yxps <- mapM2 (\ x_ y_ -> (x_, y_,) <$> uut x_ e y_) ys xs
  -- return $ IffR f g (impRAC f g $ RelC r xyps) (impRAC g f $ RelC r yxps)
  return $ relCong r xyps
uuf k (Fa vs f) e (Fa ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- uuf k' f' e g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToFaIffFa vs k f g
uuf k (Not f) e (Not g) = do
  p <- uuf k f e g
  return $ Cut (f <=> g) p $ iffToNotIffNot f g
uuf _ f _ g = et $ "uuf unimplmented case,\nf : " <> ppForm f <> "\ng : " <> ppForm g <> "\n"

puf :: Int -> [Form] -> Form -> Form -> IO Prf
puf k es (Not f) (Not g) = do
  p <- puf k es f g
  return $ Cut (f <=> g) p $ iffToNotIffNot f g
puf k es (Or fs) (Or gs) = do
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> puf k es f_ g_) fs gs
  cuts fgps <$> cast (iffsToOrIffOr fs gs)
puf k es (And fs) (And gs) = do
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> puf k es f_ g_) fs gs
  cuts fgps <$> cast (iffsToAndIffAnd fs gs)
puf k es (Imp e f) (Imp g h) = do
  pl <- puf k es e g -- pl : |- fl <=> gl 
  pr <- puf k es f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ impCong e f g h
puf k es (Iff e f) (Iff g h) = do
  pl <- puf k es e g -- pl : |- fl <=> gl 
  pr <- puf k es f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ iffCong e f g h
puf k es (Fa vs f) (Fa ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- puf k' es f' g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToFaIffFa vs k f g
puf k es (Ex vs f) (Ex ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- puf k' es f' g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToExIffEx vs k f g
puf _ es (Rel r xs) (Rel s ys) = do
  xyps <- mapM2 (\ x_ y_ -> (x_, y_,) <$> put es x_ y_) xs ys
  return $ relCong r xyps
puf _ es (Eq a b) (Eq x y) = do
  pax <- put es a x
  pby <- put es b y
  return $ eqCong (a, x, pax) (b, y, pby)
puf _ _ f g =
  et $ "PUF failed:\n f : " <> ppForm f <> "\ng : " <> ppForm g <> "\n"

vmToVxs :: VM -> [Text] -> Maybe [(Text, Term)]
vmToVxs vm = mapM (\ v_ -> (v_,) <$> HM.lookup v_ vm)

vxsToVm :: [(Text, Term)] -> VM
vxsToVm = L.foldl (\ vm_ (v_, x_) -> HM.insert v_ x_ vm_) HM.empty

putt :: Term -> Term -> Form -> IO Prf
putt x y f@(Fa vs (Eq a b)) =
  ( do vm <- cast $ specs HM.empty [(a, x), (b, y)]
       vxs <- cast $ vmToVxs vm vs
       return $ FaL vxs (a === b) $ Ax (x === y) ) <|>
  ( do vm <- cast $ specs HM.empty [(a, y), (b, x)]
       vxs <- cast $ vmToVxs vm vs
       return $ FaL vxs (a === b) $ EqS y x )
putt x y (Eq a b)
  | x == a && y == b = return $ Ax (x === y)
  | x == b && y == a = return $ EqS y x
  | otherwise = mzero
putt x y _ = et "putt : not an equation"

put :: [Form] -> Term -> Term -> IO Prf
put es x@(Fun f xs) y@(Fun g ys)
  | x == y = return $ EqR x
  | otherwise =
    first (putt x y) es <|> do
      guard $ f == g
      xyps <- mapM2 (\ x_ y_ -> (x_, y_,) <$> put es x_ y_) xs ys
      return $ FunC f xyps
put es x y
  | x == y = return $ EqR x
  | otherwise = first (putt x y) es

uegr :: Int -> VM -> Form -> Form -> Form -> IO Prf
uegr k gm (Not f) e (Not g) = do
  p <- ueg k gm f e g
  return $ Cut (f <=> g) p $ iffToNotIffNot f g
uegr k gm (Or fs) e (Or gs) = do
  ps <- mapM2 (\ f_ g_ -> ueg k gm f_ e g_) fs gs
  fgs <- mapM2 (\ f_ g_ -> return $ Iff f_ g_) fs gs
  fgps <- zipM fgs ps
  cuts fgps <$> cast (iffsToOrIffOr fs gs)
uegr k gm (And fs) e (And gs) = do
  ps <- mapM2 (\ f_ g_ -> ueg k gm f_ e g_) fs gs
  fgs <- mapM2 (\ f_ g_ -> return $ Iff f_ g_) fs gs
  fgps <- zipM fgs ps
  cuts fgps <$> cast (iffsToAndIffAnd fs gs)
uegr k gm (Imp fl fr) e (Imp gl gr) = do
  pl <- ueg k gm fl e gl -- pl : |- fl <=> gl 
  pr <- ueg k gm fr e gr -- pl : |- fr <=> gr
  return $ Cut (fl <=> gl) pl $ Cut (fr <=> gr) pr $ impCong fl fr gl gr
uegr k gm (Iff fl fr) e (Iff gl gr) = do
  pl <- ueg k gm fl e gl -- pl : |- fl <=> gl 
  pr <- ueg k gm fr e gr -- pl : |- fr <=> gr
  return $ Cut (fl <=> gl) pl $ Cut (fr <=> gr) pr $ iffCong fl fr gl gr
uegr k gm (Fa vs f) e (Fa ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  gm' <- foldM (\ gm_ (v_, x_) -> cast $ addVM v_ x_ gm_) gm vxs
  p <- ueg k' gm' f' e g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToFaIffFa vs k f g
uegr k gm (Ex vs f) e (Ex ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  gm' <- foldM (\ gm_ (v_, x_) -> cast $ addVM v_ x_ gm_) gm vxs
  p <- ueg k' gm' f' e g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToExIffEx vs k f g
uegr _ _ f e g =
  et $ "UEGR failed:\n f : " <> ppForm f <> "\ne : " <> ppForm e <> "\ng : " <> ppForm g <> "\n"

uegt :: VM -> Form -> Form -> Form -> IO Prf
uegt gm f (Iff l r) g = do
  guard $ l == f
  guard $ r == g
  return $ Ax (f <=> g)
uegt gm f (Fa vs (Iff l r)) g = do
  vxs <- cast $ mapM (\ v_ -> (v_ ,) <$> HM.lookup v_ gm) vs
  let l' = substForm vxs l
  let r' = substForm vxs r
  guard $ l' == f
  guard $ r' == g
  return $ FaL vxs (l <=> r) $ Ax (f <=> g)
uegt _ _ e _ = et $ "Not a definition : " <> ppForm e

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isInst :: Form -> Form -> Bool
isInst (Fa vs f) g = do
  let (_, xs) = listFvs 0 vs
  case zipM vs xs of
    (Just vxs) -> let f' = substForm vxs f in
                  isJust $ uniForm Lax HM.empty (f', g)
    _ -> False
isInst f g = False

notLR :: Form -> Form -> Prf -> Prf
notLR f g p = NotL f $ NotR g p

findFDRW :: Form -> Form -> Form -> IO FD
findFDRW f g h@(Fa _ _) =
  (guard (isInst h (f <=> g)) >> return (RWFD Obv h)) <|>
  (guard (isInst h (g <=> f)) >> return (RWFD Rev h))
findFDRW f g h@(Iff p q) =
  (guard (f == p && g == q) >> return (RWFD Obv h)) <|>
  (guard (g == p && f == q) >> return (RWFD Rev h))
findFDRW _ _ _ = mzero

mapIfMem :: VM -> Text -> Term
mapIfMem gm t =
  case HM.lookup t gm of
    Nothing -> Bv t
    Just x -> x

findPPRFD :: Set Text -> Form -> Form -> BT FD
findPPRFD rs f g = do
  (g', d) <- findPPRFDSel rs Obv f
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

findPPRFDSel :: Set Text -> Dir -> Form -> BT (Form, FD)
findPPRFDSel rs dr f = return (f, AxFD) <|> findPPRFDRec rs dr f

findPPRFDRec :: Set Text -> Dir -> Form -> BT (Form, FD)
findPPRFDRec rs dr (Not f) = do
  (f', d) <- findPPRFDSel rs (revDir dr) f
  return (Not f', NotFD d)
findPPRFDRec rs dr (Imp f g) = do
  (f', df) <- findPPRFDSel rs (revDir dr) f
  (g', dg) <- findPPRFDSel rs dr g
  return (f' ==> g', ImpFD df dg)
    <|> (guard (dr == Rev && removeTarget rs f') >> return (g', TransFD (ImpFD df dg) (f' ==> g') DropFD))
    <|> (guard (dr == Rev && removeTarget rs g') >> return (Not f', TransFD (ImpFD df dg) (f' ==> g') DropFD))
findPPRFDRec rs dr (Fa vs f) = do
  (f', d) <- findPPRFDSel rs dr f
  return (Fa vs f', FaFD vs d)
findPPRFDRec rs dr (Ex vs f) = do
  (f', d) <- findPPRFDSel rs dr f
  return (Ex vs f', ExFD vs d)
findPPRFDRec rs Obv (Or fs) = do
  (fs', ds) <- mapM (findPPRFDSel rs Obv) fs <&> L.unzip
  let (f', d) = cleanOr fs'
  return (f', TransFD (OrFD ds) (Or fs') d)
findPPRFDRec rs Rev (Or fs) = do
  (fs', ds) <- mapM (findPPRFDSel rs Rev) fs <&> L.unzip
  fs'' <- removeByPreds rs fs'
  let (f', d) = cleanOr fs''
  return (f', TransFD (OrFD ds) (Or fs') $ TransFD DropFD (Or fs'') d)
findPPRFDRec rs Rev (And fs) = do
  (fs', ds) <- mapM (findPPRFDSel rs Rev) fs <&> L.unzip
  let (f', d) = cleanAnd fs'
  return (f', TransFD (AndFD ds) (And fs') d)
findPPRFDRec rs Obv (And fs) = do
  (fs', ds) <- mapM (findPPRFDSel rs Obv) fs <&> L.unzip
  fs'' <- removeByPreds rs fs'
  let (f', d) = cleanAnd fs''
  return (f', TransFD (AndFD ds) (And fs') $ TransFD DropFD (And fs'') d)
findPPRFDRec _ _ _ = mzero
-- findPPRFDRec _ _ f = return (f, Id)

removeByPreds :: Set Text -> [Form] -> BT [Form]
removeByPreds rs [] = return []
removeByPreds rs (f : fs) = do
  fs' <- removeByPreds rs fs
  return (f : fs') <|> (guard (removeTarget rs f) >> return fs')

removeTarget :: Set Text -> Form -> Bool
removeTarget rs f = not $ S.disjoint rs $ formPreds f

cleanOr' :: [Form] -> [Form]
cleanOr' [] = []
cleanOr' (And [] : fs) = cleanOr' fs
cleanOr' (Or fs : gs) = fs ++ cleanOr' gs
cleanOr' (f : fs) = f : cleanOr' fs

cleanAnd :: [Form] -> (Form, FD)
cleanAnd fs =
  if bot `elem` fs
  then (bot, ConstFD)
  else case cleanAnd' fs of
         [f] -> (f, TransFD DropFD (And [f]) WrapFD)
         fs' -> (And fs', DropFD)

cleanOr :: [Form] -> (Form, FD)
cleanOr fs =
  if top `elem` fs
  then (top, ConstFD)
  else case cleanOr' fs of
         [f] -> (f, TransFD DropFD (Or [f]) WrapFD)
         fs' -> (Or fs', DropFD)

cleanAnd' :: [Form] -> [Form]
cleanAnd' [] = []
cleanAnd' (Or [] : fs) = cleanAnd' fs
cleanAnd' (And fs : gs) = fs ++ cleanAnd' gs
cleanAnd' (f : fs) = f : cleanAnd' fs

flat :: Form -> Form -> IO Prf
flat f g = do
  -- pt "Flat step.\n"
  -- pt $ "f : " <> ppForm f <> "\n"
  -- pt $ "g : " <> ppForm g <> "\n"
  let f' = flatten f
  if f' == g
  then return Asm
  else do (sg, sf) <- ndiff g f'
          pt $ "Flattened result does not match,\nsg = " <> ppForm sg <> "\nsf = " <> ppForm sf <> "\n"
          error "FLAT-FAIL"

ndiff :: Form -> Form -> IO (Form, Form)
ndiff (Fa vs f) (Fa ws g) =
  if vs == ws
  then ndiff f g
  else return (Fa vs f, Fa ws g)
ndiff (Ex vs f) (Ex ws g) =
  if vs == ws
  then ndiff f g
  else return (Fa vs f, Fa ws g)
ndiff f@(Or fs) g@(Or gs) =
  case zipM fs gs of
    Just fgs -> first (uncurry ndiff) fgs
    _ -> return (f, g)
ndiff f@(And fs) g@(And gs) =
  case zipM fs gs of
    Just fgs -> first (uncurry ndiff) fgs
    _ -> return (f, g)
ndiff f g = guard (f /= g) >> return (f, g)

flatten :: Form -> Form
flatten (Not f) =
  case flatten f of
    Not f' -> f'
    f' -> Not f'
flatten (And fs) = And $ flatAnd $ L.map flatten fs
flatten (Or fs)  = Or  $ flatOr  $ L.map flatten fs
flatten (Fa vs f)  =
  case flatten f of
    Fa ws f' -> Fa (vs ++ ws) f'
    f' -> Fa vs f'
flatten (Ex vs f)  =
  case flatten f of
    Ex ws f' -> Ex (vs ++ ws) f'
    f' -> Ex vs f'
flatten (Imp f g)  = Imp (flatten f) (flatten g)
flatten (Iff f g)  = Iff (flatten f) (flatten g)
flatten f = f

ppr :: Form -> Form -> IO Prf
ppr f g = do
  pt "PPR step.\n"
  let rs = diffPreds f g
  pt $ "Removed predicates : " <> ppList id (S.toList rs) <> "\n"
  pt $ "f : " <> ppForm f <> "\n"
  pt $ "g : " <> ppForm g <> "\n"
  let f' = pprf rs True f
  if f' == g
  then return Asm
  else do pt $ "PPR result does not match, f' = " <> ppForm f' <> "\n"
          error "PPR-FAIL"

pp :: Set Text -> Form -> Bool
pp rs (Rel r _) = S.member r rs
pp _ _ = False

pprf :: Set Text -> Bool -> Form -> Form
pprf rs b (Not f) =
  let f' = pprf rs (not b) f in
  case f' of
    And [] -> Or []
    Or [] -> And []
    _ -> Not f'
pprf rs b (Or fs) =
  let fs' = flatOr (L.map (pprf rs b) fs) in
  if And [] `elem` fs'
  then And []
  else case L.filter (Or [] /=) fs' of
         [f] -> f
         fs'' -> Or fs''
pprf rs b (And fs) =
  let fs' = flatAnd (L.map (pprf rs b) fs) in
  if Or [] `elem` fs'
  then Or []
  else case L.filter (And [] /=) fs' of
         [f] -> f
         fs'' -> And fs''
pprf rs b (Imp f g) =
  let f' = pprf rs (not b) f in
  let g' = pprf rs b g in
  case (f', g') of
    (And [], _) -> g'
    (Or [], _) -> And []
    (_, And []) -> And []
    (_, Or []) -> Not f'
    _ -> Imp f' g'
pprf rs b f@(Iff _ _) = f
pprf rs b (Fa vs f) =
  case pprf rs b f of
    And [] -> And []
    Or [] -> Or []
    Fa ws f' -> Fa (vs ++ ws) f'
    f' -> Fa vs f'
  -- let f' = pprf rs b f in
  -- if isLC f' then f' else Fa vs f'
pprf rs b (Ex vs f) =
  case pprf rs b f of
    And [] -> And []
    Or [] -> Or []
    Ex ws f' -> Ex (vs ++ ws) f'
    f' -> Ex vs f'
  -- let f' = pprf rs b f in
  -- if isLC f' then f' else Ex vs f'
pprf rs b f@(Rel r _) =
  if S.member r rs
  then if b then And [] else Or []
  else f
pprf rs _ f@(Eq _ _) = f

isLC :: Form -> Bool
isLC (And []) = True
isLC (Or []) = True
isLC _ = False
{-

substTD :: [(Text, Term)] -> TD -> TD
substTD vxs Refl = Refl
substTD vxs (RW dr f) = RW dr f
substTD vxs (FunTD tds) = FunTD $ L.map (substTD vxs) tds
substTD vxs (TransTD tdl x tdr) = TransTD (substTD vxs tdl) (substTerm vxs x) (substTD vxs tdr)

substFD :: [(Text, Term)] -> FD -> FD
substFD _ AxFD = AxFD
substFD vxs (RelFD tds) = RelFD $ L.map (substTD vxs) tds
substFD vxs (NotFD d) = NotFD $ substFD vxs d
substFD vxs (OrFD ds) = OrFD $ L.map (substFD vxs) ds
substFD vxs (AndFD ds) = AndFD $ L.map (substFD vxs) ds
substFD vxs (ImpFD d0 d1) = ImpFD (substFD vxs d0) (substFD vxs d1)
substFD vxs (IffFD d0 d1) = IffFD (substFD vxs d0) (substFD vxs d1)
substFD vxs (TransFD fdl f fdr) = TransFD (substFD vxs fdl) (substForm vxs f) (substFD vxs fdr)
substFD vxs (FaFD vs d) =
  let vxs' = L.filter (\ (v, _) -> v `notElem` vs) vxs in
  FaFD vs $ substFD vxs' d
substFD vxs (ExFD vs d) =
  let vxs' = L.filter (\ (v, _) -> v `notElem` vs) vxs in
  ExFD vs $ substFD vxs' d
substFD _ (RWFD dr cf) = RWFD dr cf
substFD _ PermFD = PermFD
substFD _ WrapFD = WrapFD
substFD _ DropFD = DropFD
substFD _ ConstFD = ConstFD
-- substFD _ SymFD = SymFD
substFD _ AlphaFD = AlphaFD
substFD vxs DNFD = DNFD
substFD vxs (EqFD fdl fdr) =  EqFD (substTD vxs fdl) (substTD vxs fdr)

foldWithFDobv :: Ctx -> (FD, PrvGoal) -> IO Ctx
foldWithFDobv c0 (fd, (f, g, k)) = do
  (m, n, c1) <- cast $ appCut nt (f <=> g, k) c0
  c2 <- foldWithFD' fd f g m c1
  ((fg, n'), c3) <- cast $ appIffLO (f <=> g, n) c2
  cast $ appMP fg (f, g, n') c3

foldWithFDrev :: Ctx -> (FD, PrvGoal) -> IO Ctx
foldWithFDrev c0 (fd, (g, f, k)) = do
  (m, n, c1) <- cast $ appCut nt (f <=> g, k) c0
  c2 <- foldWithFD' fd f g m c1
  ((gf, n'), c3) <- cast $ appIffLR (f <=> g, n) c2
  cast $ appMP gf (g, f, n') c3

foldWithFD' :: FD -> Form -> Form -> Int -> Ctx -> IO Ctx
foldWithFD' fd@(RWFD Obv eqn) f g k c =
  foldWithFD fd f g k c
foldWithFD' fd@(RWFD Rev eqn) f g k c =
  foldWithFD fd f g k c
foldWithFD' fd f g k c =
  foldWithFD fd f g k c

  -}

impR :: Form -> Form -> Prf -> Prf
impR f g p = ImpRA f g $ ImpRC f g p

guardMsg :: (Alternative m, Monad m) => Bool -> Text -> m ()
guardMsg True _ = return ()
guardMsg False s = error (unpack s)

revBij :: Bij a a -> Bij a a
revBij (vw, wv) = (wv, vw)

andRs :: [(Form, Prf)] -> Form -> IO Prf
andRs fps (And fs) = AndR <$> mapM (\ f_ -> (f_,) <$> andRs fps f_) fs
andRs fps f = cast $ snd <$> L.find ((f ==) . fst) fps

orLs :: [(Form, Prf)] -> Form -> IO Prf
orLs fps (Or fs) = OrL <$> mapM (\ f_ -> (f_,) <$> orLs fps f_) fs
orLs fps f = cast $ snd <$> L.find ((f ==) . fst) fps

orRs :: Form -> Prf -> IO Prf
orRs (Or fs) p = OrR fs fs <$> foldM (flip orRs) p fs
orRs _ p = return p

andLs :: Form -> Prf -> IO Prf
andLs (And fs) p = AndL fs fs <$> foldM (flip andLs) p fs
andLs _ p = return p

uvm :: Int -> Bij Text Text -> Form -> Form -> IO Prf
uvm k vm f g =
  if f == g
  then return $ Ax f
  else uvmr k vm f g

uvmr :: Int -> Bij Text Text -> Form -> Form -> IO Prf

uvmr k vm (Or fs) (Or gs) = do
  let fs' = flatOr fs
  let gs' = flatOr gs
  fps <- useOrVR k vm fs' gs'
  -- OrR gs gs . OrL <$> useOrVR k vm fs gs
  orLs fps (Or fs) >>= orRs (Or gs)

uvmr k vm (And fs) (And gs) = do -- AndL fs fs . AndR <$> useAndVR k vm fs gs
  let fs' = flatAnd fs
  let gs' = flatAnd gs
  gps <- useAndVR k vm fs' gs'
  andRs gps (And gs) >>= andLs (And fs)

uvmr _ _ f@(Rel _ _) g@(Rel _ _) = do
  guard (f == g)
  return $ Ax f

uvmr _ _ f@(Eq x y) (Eq z w)
  | x == z && y == w = return $ Ax f
  | x == w && y == z = return $ EqS x y
  | otherwise = mzero

uvmr k vm (Not f) (Not g) = do
  p <- uvm k (revBij vm) g f
  return $ notLR f g p
uvmr k vm (Not (Not f)) g = do
  p <- uvm k vm f g
  return $ NotL (Not f) $ NotR f p
uvmr k vm f (Not (Not g)) = do
  p <- uvm k vm f g
  return $ NotR (Not g) $ NotL g p

uvmr k vm (Imp fl fr) (Imp gl gr) = do
  pl <- uvm k (revBij vm) gl fl
  pr <- uvm k vm fr gr
  return $ ImpL fl fr (ImpRA gl gr pl) (ImpRC gl gr pr)

uvmr k vm f@(Iff fl fr) g@(Iff gl gr) = do
  pol <- uvm k (revBij vm) gl fl -- pol : gl |- fl
  por <- uvm k vm fr gr         -- por : fr |- gr
  prr <- uvm k (revBij vm) gr fr -- prr : gr |- fr
  prl <- uvm k vm fl gl         -- prl : fl |- gl
  let po = IffLO fl fr $ ImpL fl fr pol por -- po : gl |- gr
  let pr = IffLR fl fr $ ImpL fr fl prr prl -- pr : gr |- gl
  return $ IffR gl gr (impR gl gr po) (impR gr gl pr)

uvmr k vm (Fa vs f) (Fa ws g) = do
  let (k', xs) = listPars k ws
  wxs <- zipM ws xs
  let ys = L.map (pairWithVR vm wxs) vs
  vys <- zipM vs ys
  let f' = substForm vys f
  let g' = substForm wxs g
  p <- uvm k' vm f' g'
  return $ FaR ws k g $ FaL vys f p

uvmr k vm (Ex vs f) (Ex ws g) = do
  let (k', xs) = listPars k vs
  vxs <- zipM vs xs
  let ys = L.map (pairWithVR (revBij vm) vxs) ws
  wys <- zipM ws ys
  let f' = substForm vxs f
  let g' = substForm wys g
  p <- uvm k' vm f' g'
  return $ ExL vs k f $ ExR wys g p

uvmr _ _ f g = mzero -- error $ unpack $ "use-VR umimplemented :\n" <> "f = " <> ppForm f <> "\ng = " <> ppForm g <> "\n"


useOrVR :: Int -> Bij Text Text -> [Form] -> [Form] -> IO  [(Form, Prf)]
useOrVR _ _ [] [] = return []
useOrVR k vm (f : fs) ggs = do
  (p, gs) <- first (\ (g_, gs_) -> (, gs_) <$> uvm k vm f g_) $ plucks ggs
  fps <- useOrVR k vm fs gs
  return $ (f, p) : fps
useOrVR _ _ [] (_ : _) = mzero

useAndVR :: Int -> Bij Text Text -> [Form] -> [Form] -> IO  [(Form, Prf)]
useAndVR _ _ [] [] = return []
useAndVR k vm ffs (g : gs) = do
  (p, fs) <- first (\ (f_, fs_) -> (, fs_) <$> uvm k vm f_ g) $ plucks ffs
  gps <- useAndVR k vm fs gs
  return $ (g, p) : gps
useAndVR _ _ (_ : _) [] = mzero


pairWithVR :: Bij Text Text -> [(Text, Term)] -> Text -> Term
pairWithVR vm wxs v =
  fromMaybe zt ( do w <- getBij v vm
                    snd <$> L.find ((w ==) . fst) wxs )
{-

useFD :: Int -> FD -> Form -> Form -> IO Prf
useFD _ AxFD f g =
  ( do guard (f == g)
       return $ Ax f ) <|>
  errorFD "AxFD" f g

useFD k (NotFD df) (Not f) (Not g) =
  ( do p <- useFD k (revDiff df) g f
       return $ notLR f g p ) <|>
  errorFD "NotFD" f g

useFD k DNFD (Not (Not f)) g = do
  guard $ f == g
  return $ NotL (Not f) $ NotR f $ Ax f

useFD k DNFD f (Not (Not g)) = do
  guard $ f == g
  return $ NotR (Not g) $ NotL g $ Ax g

useFD k (OrFD dfs) (Or fs) (Or gs) = do
  dffgs <- zip3M dfs fs gs
  ps <- mapM (\ (df_, f_, g_) -> useFD k df_ f_ g_) dffgs -- (useFD k)
  fps <- zipM fs ps
  return $ OrR gs gs $ OrL fps

useFD k (AndFD dfs) (And fs) (And gs) = do
  dffgs <- zip3M dfs fs gs
  ps <- mapM (\ (df_, f_, g_) -> useFD k df_ f_ g_) dffgs -- (useFD k)
  gps <- zipM gs ps
  return $ AndL fs fs $ AndR gps

useFD k (IffFD dfl dfr) f@(Iff fl fr) g@(Iff gl gr) =
  ( do pol <- useFD k (revDiff dfl) gl fl -- pol : gl |- fl
       por <- useFD k dfr fr gr           -- por : fr |- gr
       prr <- useFD k (revDiff dfr) gr fr -- prr : gr |- fr
       prl <- useFD k dfl fl gl           -- prl : fl |- gl
       let po = IffLO fl fr $ ImpL fl fr pol por -- po : gl |- gr
       let pr = IffLR fl fr $ ImpL fr fl prr prl -- pr : gr |- gl
       return $ IffR gl gr (impR gl gr po) (impR gr gl pr) ) <|>
  errorFD "IffFD" f g

useFD k (RWFD Obv (Fa vs (Iff f' g'))) f g = do
  pt "use-fd-iff-0\n"
  gm <- foldM gndForm HM.empty [(f', f), (g', g)]
  pt "use-fd-iff-1\n"
  let vxs = L.map (\ v_ -> (v_, HM.findWithDefault zt v_ gm)) vs
  return $ FaL vxs (f' <=> g') $ IffLO f g $ mp f g
useFD k (RWFD Rev (Fa vs (Iff g' f'))) f g = do
  gm_ <- gndForm HM.empty (g', g)
  gm <- gndForm gm_ (f', f)
  let vxs = L.map (\ v_ -> (v_, HM.findWithDefault zt v_ gm)) vs
  return $ FaL vxs (g' <=> f') $ IffLR g f $ mp f g
useFD k (RWFD Obv (Iff f' g')) f g = do
  guardMsg (f == f' && g == g') $ "definition mismatch\n" <> "fg = " <> ppForm (Iff f' g') <> "\nf =" <> ppForm f <> "\ng =" <> ppForm g
  return $ IffLO f g $ mp f g
useFD k (RWFD Rev (Iff g' f')) f g = do
  guardMsg (f == f' && g == g') $ "definition mismatch\n" <> "fg = " <> ppForm (Iff f' g') <> "\nf =" <> ppForm f <> "\ng =" <> ppForm g
  return $ IffLR g f $ mp f g

useFD k (FaFD _ df) (Fa vs f) (Fa ws g) = do
  guard (vs == ws)
  let (k', xs) = listPars k vs
  vxs <- zipM vs xs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- useFD k' (substFD vxs df) f' g'
  return $ FaR vs k g $ FaL vxs f p

useFD k (ExFD _ df) (Ex vs f) (Ex ws g) =
  ( do guard (vs == ws)
       let (k', xs) = listPars k vs
       vxs <- zipM vs xs
       let f' = substForm vxs f
       let g' = substForm vxs g
       p <- useFD k' (substFD vxs df) f' g'
       return $ ExL vs k f $ ExR vxs g p ) <|>
  errorFD "ExFD"  (Ex vs f) (Ex ws g)

useFD k (TransFD fdl g fdr) f h =
  ( do pl <- useFD k fdl f g
       pr <- useFD k fdr g h
       return $ Cut g pl pr ) <|>
  errorFD "Trans-FD" f h

useFD k AlphaFD (Fa vs f) (Fa ws g) =
  ( do let (k', xs) = listPars k ws
       vxs <- zipM vs xs
       wxs <- zipM ws xs
       let f' = substForm vxs f
       let g' = substForm wxs g
       guardMsg (f' == g') $ "Alpha conversion result unequal:\n" <> ppForm (Fa vs f) <> "\n" <> ppForm (Fa ws g) <> "\n"
       return $ FaR ws k g $ FaL vxs f $ Ax f' ) <|>
  errorFD "AlphaFD-Fa" (Fa vs f) (Fa ws g)

useFD k AlphaFD (Ex vs f) (Ex ws g) =
  ( do let (k', xs) = listPars k ws
       vxs <- zipM vs xs
       wxs <- zipM ws xs
       let f' = substForm vxs f
       let g' = substForm wxs g
       guardMsg (f' == g') "Alpha conversion result unequal"
       return $ ExL vs k f $ ExR wxs g $ Ax f' ) <|>
  errorFD "AlphaFD-Ex" (Ex vs f) (Ex ws g)

useFD k PermFD (Fa vs f) (Fa ws g) =
  ( do let (k', xs) = listPars k ws
       wxs <- zipM ws xs
       vxs <- cast $ mapM (\ v_ -> L.find ((v_ ==) . fst) wxs) vs
       let f' = substForm vxs f
       let g' = substForm wxs g
       guardMsg (f' == g') "Q-permutation result unequal"
       return $ FaR ws k g $ FaL vxs f $ Ax f' ) <|>
  errorFD "PermFD-Fa" (Fa vs f) (Fa ws g)

useFD k PermFD (Ex vs f) (Ex ws g) =
  ( do let (k', xs) = listPars k vs
       vxs <- zipM vs xs
       wxs <- cast $ mapM (\ v_ -> L.find ((v_ ==) . fst) vxs) ws
       let f' = substForm vxs f
       let g' = substForm wxs g
       guardMsg (f' == g') "Q-permutation result unequal"
       return $ ExL vs k f $ ExR wxs g $ Ax f' ) <|>
  errorFD "PermFD-Ex" (Ex vs f) (Ex ws g)

useFD k PermFD (Or fs) (Or gs) = return $ OrR gs gs $ OrL $ L.map (\ f_ -> (f_, Ax f_)) fs
useFD k PermFD (And fs) (And gs) = return $ AndL fs fs $ AndR $ L.map (\ g_ -> (g_, Ax g_)) gs
useFD k PermFD (Eq xl xr) (Eq yl yr) = return $ EqS xl xr

useFD k (ImpFD fdl fdr) (Imp fl fr) (Imp gl gr) = do
  pl <- useFD k (revDiff fdl) gl fl
  pr <- useFD k fdr fr gr
  return $ ImpL fl fr (ImpRA gl gr pl) (ImpRC gl gr pr)

useFD k (RelFD tds) (Rel f xs) (Rel g ys) = do
  guard $ f == g
  tdxys <- zip3M tds xs ys
  ps <- mapM useTD tdxys
  xyps <- zip3M xs ys ps
  return $ RelC f xyps

useFD k (EqFD tdl tdr) (Eq xl xr) (Eq yl yr) = do
  pl <- useTD (tdl, xl, yl)
  pr <- useTD (tdr, xr, yr)
  let p0 = Cut (xl === yl) pl $ EqS xl yl -- p0 :: |- yl = xl
  let p1 = Cut (xr === yr) pr $ EqT xl xr yr -- p1 :: |- xl = yr
  return $ Cut (yl === xl) p0 $ Cut (xl === yr) p1 $ EqT yl xl yr

useFD k (RWFD _ _) f g = errorFD "rw-fd-unimplemented" f g
useFD k WrapFD f g = errorFD "wrap-fd-unimplemented" f g
useFD k DropFD f g = errorFD "drop-fd-unimplemented" f g
useFD k ConstFD f g = errorFD "const-fd-unimplemented" f g

useFD k (EqFD _ _) f g = errorFD "invalid-eq-fd" f g
useFD k (OrFD _) f g = errorFD "invalid-or-fd" f g
useFD k (AndFD _) f g = errorFD "invalid-or-fd" f g
useFD k (ImpFD _ _) f g = errorFD "invalid-imp-fd" f g
useFD k (IffFD _ _) f g = errorFD "invalid-iff-fd" f g
useFD k (NotFD _) f g = errorFD "invalid-not-fd" f g
useFD k (FaFD _ _) f g = errorFD "invalid-fa-fd" f g
useFD k (ExFD _ _) f g = errorFD "invalid-ex-fd" f g
useFD k (RelFD _) f g = errorFD "invalid-rel-fd" f g
useFD k AlphaFD f g = errorFD "invalid-alpha-fd" f g
useFD k PermFD f g = errorFD "invalid-perm-fd" f g
useFD k DNFD f g = errorFD "invalid-not-fd" f g

-- useFD k fd f g = errorFD ("use-fd-unimplemented" <> pack (show fd)) f g

errorFD :: Text -> Form -> Form -> IO a
errorFD t f g = error (unpack $ t <> ":\nf : " <> ppForm f <> "\ng : " <> ppForm g <> "\n")



foldWithFD :: FD -> Form -> Form -> Int -> Ctx -> IO Ctx
foldWithFD AxFD f g k c = do
  (gl0, gl1, c0) <- cast $ appIffR (f <=> g, k) c
  c1 <- cast $ appImpRAC gl0 c0 >>= uncurry (appAx Exact)
  cast $ appImpRAC gl1 c1 >>= uncurry (appAx Exact)
foldWithFD (NotFD d) (Not f) (Not g) k c = do
  (k', k'', c0) <- cast $ appCut Nothing (f <=> g, k) c
  ((nfng, m0), (ngnf, n0), c1) <-  cast $ appIffR (Not f <=> Not g, k'') c0
  ((fg, n1), c2) <- cast $ appIffLO (f <=> g, n0) c1
  c3 <- cast $ appContra (fg, ngnf, n1) c2
  ((gf, m1), c4) <- cast $ appIffLR (f <=> g, m0) c3
  c5 <- cast $ appContra (gf, nfng, m1) c4
  foldWithFD' d f g k' c5
foldWithFD (NotFD _) _ _ _ _ = mzero
foldWithFD (IffFD d0 d1) (Iff f0 f1) (Iff g0 g1) k0 z0 = do
  (m0, k1, z1) <- cast $ appCut Nothing (f0 <=> g0, k0) z0
  -- m0 : |- f0 <=> g0 
  -- k1 : f0 <=> g0 |-
  z2 <- foldWithFD' d0 f0 g0 m0 z1
  (m1, k2, z3) <- cast $ appCut Nothing (f1 <=> g1, k1) z2
  -- m1 : f0 <=> g0 |- f1 <=> g1 
  -- k2 : f0 <=> g0, f1 <=> g1 |- (f0 <=> f1) <=> (g0 <=> g1)
  z4 <- foldWithFD' d1 f1 g1 m1 z3
  (m2, k3, z5) <- cast $ appCut Nothing (g0 <=> f0, k2) z4
  -- m2 : f0 <=> g0, f1 <=> g1 |- g0 <=> f0
  z6 <- cast $ appIffSym Exact (f0 <=> g0) (g0 <=> f0) m2 z5
  (m3, k4, z7) <- cast $ appCut Nothing (g1 <=> f1, k3) z6
  z8 <- cast $ appIffSym Exact (f1 <=> g1) (g1 <=> f1) m3 z7
  ((_, m4), (_, k5), z9) <- cast $ appIffR ((f0 <=> f1) <=> (g0 <=> g1), k4) z8
  ((_, m5), z10) <- cast $ appImpRA ((f0 <=> f1) ==> (g0 <=> g1), m4) z9
  ((_, m6), z11) <- cast $ appImpRC ((f0 <=> f1) ==> (g0 <=> g1), m5) z10
  z12 <- cast $ appIffTrans2 (g0 <=> f0) (f0 <=> f1) (f1 <=> g1) (g0 <=> g1) m6 z11
  ((_, k6), z13) <- cast $ appImpRA ((g0 <=> g1) ==> (f0 <=> f1), k5) z12
  ((_, k7), z14) <- cast $ appImpRC ((g0 <=> g1) ==> (f0 <=> f1), k6) z13
  cast $ appIffTrans2 (f0 <=> g0) (g0 <=> g1) (g1 <=> f1) (f0 <=> f1) k7 z14
foldWithFD (AndFD ds) f@(And fs) g@(And gs) k0 c0 = do
  (o0, o1, c1) <- cast $ appIffR (f <=> g, k0) c0
  (os0, c2) <- cast $ appImpR o0 c1 >>= uncurry appAndLR
  dos0 <- zipM ds os0
  c3 <- foldM foldWithFDobv c2 dos0
  (os1, c4) <- cast $ appImpR o1 c3 >>= uncurry appAndLR
  dos1 <- zipM ds os1
  foldM foldWithFDrev c4 dos1
foldWithFD (AndFD _) _ _ _ _ = mzero
foldWithFD (OrFD ds) f@(Or fs) g@(Or gs) k0 c0 = do
  (o0, o1, c1) <- cast $ appIffR (f <=> g, k0) c0
  (os0, c2) <- cast $ appImpR o0 c1 >>= uncurry appOrRL
  dos0 <- zipM ds os0
  c3 <- foldM foldWithFDobv c2 dos0
  (os1, c4) <- cast $ appImpR o1 c3 >>= uncurry appOrRL
  dos1 <- zipM ds os1
  foldM foldWithFDrev c4 dos1

foldWithFD (FaFD vs df) f@(Fa fvs _) g@(Fa gvs _) k0 c0 = do
  guard (vs == fvs && vs == gvs)
  (o0, o1, c1) <- cast $ appIffR (f <=> g, k0) c0

  ((_, _, m0), c2) <- cast $ appImpR o0 c1
  (xs, (gxs, m1), c3) <- cast $ appFaR (g, m0) c2
  ((fxs, m2), c4) <- cast $ appFaL (Just xs) (f, m1) c3
  vxs <- zipM vs xs
  let dfxs = substFD vxs df
  c5 <- foldWithFD' dfxs fxs gxs m2 c4

  ((_, _, n0), c6) <- cast $ appImpR o1 c5
  (ys, (fys, n1), c7) <- cast $ appFaR (f, n0) c6
  ((gys, n2), c8) <- cast $ appFaL (Just ys) (g, n1) c7
  vys <- zipM vs ys
  let dfys = substFD vys df
  foldWithFD' (revDiff dfys) gys fys n2 c8

foldWithFD (FaFD vs df) _ _ _ _ = error "forkRect"

foldWithFD (OrFD _) _ _ _ _ = mzero
foldWithFD (IffFD _ _) _ _ _ _ = mzero
foldWithFD (RWFD Obv (Fa vs fg)) f g k c = do
  ((fg', k'), c') <- cast $ appFaL Nothing (Fa vs fg, k) c
  cast $ appAx Lax (fg', f <=> g, k') c'
foldWithFD (RWFD Rev (Fa vs gf)) f g k c = do
  ((gf', k'), c') <- cast $ appFaL Nothing (Fa vs gf, k) c
  cast $ appIffSym Lax gf' (f <=> g) k c
foldWithFD (RWFD Obv fg) f g k c = cast $ appAx Exact (f <=> g, f <=> g, k) c
foldWithFD (RWFD Rev gh) f g k c = cast $ appIffSym Exact (g <=> f) (f <=> g) k c
foldWithFD (ImpFD _ _) f g k c = error "fold-with-fd-imp"
foldWithFD (RelFD _) f g k c = error "fold-with-fd-rel"
foldWithFD (EqFD _ _) f g k c = error "fold-with-fd-eq"
foldWithFD PermFD f g k c = error "fold-with-fd-perm"
foldWithFD WrapFD f g k c = error "fold-with-fd-wrap"
foldWithFD DropFD f g k c = error "fold-with-fd-drop"
foldWithFD AlphaFD f g k c = error "fold-with-fd-ac"
-- foldWithFD SymFD f g k c = error "fold-with-fd-sym"
foldWithFD ConstFD f g k c = error "fold-with-fd-const"
foldWithFD (ExFD _ _) f g k c = error "fold-with-fd-ex"
foldWithFD DNFD f g k c = error "fold-with-fd-dn"
foldWithFD TransFD {} f g k c = error "fold-with-fd-trans"

revTD :: TD -> TD
revTD Refl = Refl
revTD (RW dr f) = RW (revDir dr) f
revTD (FunTD tds) = FunTD $ L.map revTD tds
revTD (TransTD tdl x tdr) = TransTD (revTD tdr) x (revTD tdl)

revDiff :: FD -> FD
revDiff (RelFD tds) = RelFD $ L.map revTD tds
revDiff (NotFD d) = NotFD $ revDiff d
revDiff (OrFD ds) = OrFD $ L.map revDiff ds
revDiff (AndFD ds) = AndFD $ L.map revDiff ds
revDiff (FaFD vs d) = FaFD vs $ revDiff d
revDiff (ExFD vs d) = ExFD vs $ revDiff d
revDiff (ImpFD d0 d1) = ImpFD (revDiff d0) (revDiff d1)
revDiff (IffFD d0 d1) = IffFD (revDiff d0) (revDiff d1)
revDiff (TransFD d0 f d1) = TransFD (revDiff d1) f (revDiff d0)
revDiff (EqFD tdl tdr) = EqFD (revTD tdl) (revTD tdr)
revDiff AxFD = AxFD
revDiff (RWFD dr cf) = RWFD (revDir dr) cf
-- revDiff SymFD = SymFD
revDiff PermFD = PermFD
revDiff WrapFD = WrapFD
revDiff DropFD = DropFD
revDiff AlphaFD = AlphaFD
revDiff ConstFD = ConstFD
revDiff DNFD = DNFD

unfoldTD' :: TD -> EqGoal -> Ctx -> IO Ctx
unfoldTD' td eg c = unfoldTD td eg c <|> error (unpack $ "unfold-td\nTD: " <> pack (show td) <> "\nEG: " <> ppEqGoal  eg)

unfoldTD :: TD -> EqGoal -> Ctx -> IO Ctx
unfoldTD Refl eg@(x, y, k) c = cast $ appEqR Exact (Eq x y, k) c
unfoldTD (FunTD tds) eg@(Fun _ _, Fun _ _, _) c = do
  (egs, c') <- cast $ appFunC eg c
  tdegs <- zipM tds egs
  foldM (\ c_ (td_, eg_) -> unfoldTD' td_ eg_ c_) c' tdegs
unfoldTD (TransTD tdl y tdr) eg c = do
  (egl, egr, c') <- cast $ appEqTR y eg c
  unfoldTD' tdl egl c' >>= unfoldTD' tdr egr
unfoldTD (RW Obv f) eg@(x, y, k) c = do
  let ((f', k'), c') = tryAppFaL (f, k) c
  cast $  appAx Lax (f', Eq x y, k') c'
unfoldTD (RW Rev f) eg@(x, y, k) c = do
  let ((f', k'), c') = tryAppFaL (f, k) c
  cast $ appEqS Lax (f', Eq x y, k') c'
unfoldTD td eg _ = error $ unpack $ "unfold-td\nTD: " <> pack (show td) <> "\nEG: " <> ppEqGoal  eg

unfoldWithFD :: FD -> PrvGoal -> Ctx -> IO Ctx
unfoldWithFD AxFD pg c = cast $ appAx Exact pg c
unfoldWithFD (FaFD us d) (Fa vs f, Fa ws g, k) c = do
  (xs, (g', k'), c') <- cast $ appFaR (Fa ws g, k) c
  ((f', k''), c'') <- cast $ appFaL (Just xs) (Fa vs f, k') c'
  uxs <- zipM us xs
  let d' = substFD uxs d
  unfoldWithFD d' (f', g', k'') c''
unfoldWithFD (NotFD fd) pg@(Not _, Not _, _) c = do
  (pg', c') <- cast $ appNotLR pg c
  unfoldWithFD (revDiff fd) pg' c'
unfoldWithFD (RelFD tds) pg@(Rel _ _, Rel _ _, _) c = do
  (egs, c') <- cast $ appRelC pg c
  tdegs <- zipM tds egs
  foldM (\ c_ (td_, eg_) -> unfoldTD' td_ eg_ c_) c' tdegs
unfoldWithFD (EqFD tdl tdr) pg@(Eq _ _, Eq _ _, _) c = do
  (eql, eqr, c') <- cast $ appEqC pg c
  unfoldTD' tdl eql c' >>= unfoldTD' tdr eqr
unfoldWithFD (TransFD tdl g tdr) pg@(f, h, k) c = do
  (m, n, c') <- cast $ appCut nt (g, k) c
  unfoldWithFD tdl (f, g, m) c' >>= unfoldWithFD tdr (g, h, n)
-- unfoldWithFD SymFD pg c = cast $ appEqS Exact pg c
unfoldWithFD (OrFD tds) pg@(Or _, Or _, k) c = do
  (pgs, c') <- cast $ appOrRL pg c
  tdpgs <- zipM tds pgs
  foldM (\ c_ (td_, pg_) -> unfoldWithFD td_ pg_ c_) c' tdpgs
unfoldWithFD PermFD (Or fs, Or gs, k) c = do
  (_, k', c') <- cast $ appOrR (Or gs, k) c
  (gls, c'') <- cast $ appOrL (Or fs, k') c'
  cast $ disjMap gls gs c''
unfoldWithFD df pg c = error (unpack $ "diff-unfold\ndiff: " <> pack (show df) <> "\ngoal: " <> ppPrvGoal pg)

pprWithFD :: FD -> PrvGoal -> Ctx -> IO Ctx
pprWithFD AxFD pg c = cast $ appAx Exact pg c
pprWithFD (NotFD d) pg@(Not _, Not _, _) c = cast (appNotLR pg c) >>= uncurry (pprWithFD (revDiff d))
pprWithFD (NotFD _) _ _ = error "use-diff-0"
pprWithFD (TransFD d0 g d1) (f, h, k) c0 = do
  (k0, k1, c1) <- cast $ appCut nt (g, k) c0
  c2 <- pprWithFD d0 (f, g, k0) c1
  pprWithFD d1 (g, h, k1) c2
pprWithFD DropFD (f, Imp g h, k) c =
  ( do ((g', k'), c') <- cast $ appNotL (f, k) c
       ((_, k''), c'') <- cast $ appImpRA (Imp g h, k') c'
       cast $ appAx Exact (g, g', k'') c'' ) <|>
  ( do ((f', k'), c') <- cast $ appImpRC (Imp g h, k) c
       cast $ appAx Exact (f, f', k') c' )
pprWithFD DropFD (Or fs, Or gs, k) c = do
  let (_, k', c') = appNestOrR (Or gs, k) c
  let (gls, c'') = appNestOrL (Or fs, k') c'
  -- cast $ foldM (pickConsAppAx gs) c'' gls
  cast $ disjMap gls gs c''
pprWithFD DropFD (And fs, And gs, k) c = do
  let (_, k', c') = appNestAndL (And fs, k) c
  let (gls, c'') = appNestAndR (And gs, k') c'
  -- cast $ foldM (pickPremAppAx fs) c'' gls
  cast $ conjMap fs gls c''
pprWithFD DropFD _ _ = mzero

pprWithFD (OrFD ds) pg c = do
  (pgs, c') <- cast $ appOrRL pg c
  dpgs <- zipM ds pgs
  foldM (\ c_ (d_, pg_) -> pprWithFD d_ pg_ c_ ) c' dpgs
pprWithFD (AndFD ds) pg c = do
  (pgs, c') <- cast $ appAndLR pg c
  dpgs <- zipM ds pgs
  foldM (\ c_ (d_, pg_) -> pprWithFD d_ pg_ c_ ) c' dpgs

pprWithFD WrapFD (And [f], f', k) c = do
  (_, k', c') <- cast $ appAndL (And [f], k) c
  cast $ appAx Exact (f, f', k') c'
pprWithFD WrapFD (Or [f], f', k) c = do
  ([(_, k')], c') <- cast $ appOrL (Or [f], k) c
  cast $ appAx Exact (f, f', k') c'
pprWithFD WrapFD (f, Or [g], k) c = do
  (_, k', c') <- cast $ appOrR (Or [g], k) c
  cast $ appAx Exact (f, g, k') c'
pprWithFD WrapFD (f, And [g], k) c = do
  ([(_, k')], c') <- cast $ appAndR (And [g], k) c
  cast $ appAx Exact (f, g, k') c'
pprWithFD WrapFD _ _ = error "use-diff-4"

pprWithFD ConstFD (_, And [], k) c =
  cast $ appTopR (top, k) c
pprWithFD ConstFD (Or [], _, k) c =
  cast $ appBotL (bot, k) c
pprWithFD ConstFD (And [], Or fs, k) c = do
  guard $ top `elem` fs
  (_, k', c') <- cast $ appOrR (Or fs, k) c
  cast $ appTopR (top, k') c'
pprWithFD ConstFD (And fs, Or [], k) c = do
  guard $ bot `elem` fs
  (_, k', c') <- cast $ appAndL (And fs, k) c
  cast $ appBotL (top, k') c'
pprWithFD ConstFD _ _ = error "use-diff-5"
pprWithFD (ImpFD d0 d1) pg c = do
  (pg0, pg1, c') <- cast $ appImpLR pg c
  pprWithFD (revDiff d0) pg0 c' >>= pprWithFD d1 pg1
pprWithFD (FaFD us d) (Fa vs f, Fa ws g, k) c = do
  (xs, (g', k'), c') <- cast $ appFaR (Fa ws g, k) c
  ((f', k''), c'') <- cast $ appFaL (Just xs) (Fa vs f, k') c'
  uxs <- zipM us xs
  let d' = substFD uxs d
  pprWithFD d' (f', g', k'') c''
pprWithFD (ExFD us d) (Ex vs f, Ex ws g, k) c = do
  (xs, (f', k'), c') <- cast $ appExL (Ex vs f, k) c
  ((g', k''), c'') <- cast $ appExR (Just xs) (Ex ws g, k') c'
  uxs <- zipM us xs
  let d' = substFD uxs d
  pprWithFD d' (f', g', k'') c''
pprWithFD (FaFD _ _) _ _ = error "use-diff-fa"
pprWithFD (ExFD _ _) _ _ = error "use-diff-ex"
pprWithFD (RelFD tds) pg c = do
  (egs, c') <- cast $ appRelC pg c
  tdegs <- zipM tds egs
  foldM (\ c_ (td_, eg_) -> useOldTD td_ eg_ c_) c' tdegs
pprWithFD (EqFD tdl tdr) pg c = do
  (egl, egr, c') <- cast $ appEqC pg c
  useOldTD tdl egl c' >>= useOldTD tdr egr
-- pprWithFD SymFD pg c = cast $ appEqS Exact pg c

pprWithFD (RWFD _ _) _ _ = error "ppr-with-fd-rw"
pprWithFD (IffFD _ _) _ _ = error "ppr-with-fd-iff"
pprWithFD PermFD _ _ = error "ppr-with-fd-perm"
pprWithFD AlphaFD _ _ = error "ppr-with-fd-AC"
pprWithFD DNFD _ _ = error "ppr-with-fd-DN"

useTD :: (TD, Term, Term) -> IO Prf
useTD (Refl, x, y) = return (EqR x)
useTD (FunTD tds, Fun f xs, Fun g ys) = do
  tdxys <- zip3M tds xs ys
  ps <- mapM useTD tdxys
  xyps <- zip3M xs ys ps
  return $ FunC f xyps
useTD (TransTD tdl y tdr, x, z) = do
  pxy <- useTD (tdl, x, y)
  pyz <- useTD (tdr, y, z)
  return $ Cut (x === y) pxy $ Cut (y === z) pyz $ EqT x y z

useTD (RW Obv (Fa vs (Eq x' y')), x, y) = do
  m <- gndTerm HM.empty (x', x)
  m' <- gndTerm m (y', y)
  let vxs = getInsts m' vs
  return $ FaL vxs (Eq x' y') $ Ax (x === y)

useTD (RW Obv (Eq _ _), x, y) = return $ Ax (x === y)

useTD (RW Rev (Fa vs (Eq x' y')), y, x) = do
  m <- gndTerm HM.empty (x', x)
  m' <- gndTerm m (y', y)
  let vxs = getInsts m' vs
  return $ FaL vxs (Eq x' y') $ EqS x y

useTD (RW Rev (Eq _ _), x, y) = return $ EqS y x

useTD (td, _, _) = error $ "TD-unimp" ++ show td

getInsts :: VM -> [Text] -> [(Text, Term)]
getInsts m [] = []
getInsts m (v : vs) =
  case HM.lookup v m of
    (Just x) -> (v, x) : getInsts m vs
    _ -> (v, zt) : getInsts m vs

useOldTD :: TD -> EqGoal -> Ctx -> IO Ctx
useOldTD Refl (x, y, k) c = cast $ appEqR Exact (Eq x y, k) c
useOldTD (FunTD tds) eg c = do
  (egs, c') <- cast $ appFunC eg c
  tdegs <- zipM tds egs
  foldM (\ c_ (td_, eg_) -> useOldTD td_ eg_ c_) c' tdegs
useOldTD (RW Obv f) (x, y, k) c = do
  let ((f', k'), c') = tryAppFaL (f, k) c
  cast $ appAx Lax (f', x === y, k') c'
useOldTD (RW Rev f) (x, y, k) c = do
  let ((f', k'), c') = tryAppFaL (f, k) c
  cast $ appEqS Lax (f', x === y, k') c'
useOldTD (TransTD tdl y tdr) eg c = do
  (egl, egr, c') <- cast $ appEqTR y eg c
  useOldTD tdl egl c' >>= useOldTD tdr egr

-}
appEqTR :: Term -> EqGoal -> Ctx -> Maybe (EqGoal, EqGoal, Ctx)
appEqTR y (x, z, k) c = do
  (m, k', c') <- appCut Nothing (x === y, k) c
  (n, k'', c'') <- appCut Nothing (y === z, k') c'
  c''' <- appEqT Exact (x === y) (y === z) (x === z) k'' c''
  return ((x, y, m), (y, z, n), c''')

markBT :: Int -> BT ()
markBT k = trace (unpack $ "Marking checkpoint " <> ppInt k <> "\n") (return ())

avatarContra :: Form -> Form -> IO Ctx
avatarContra f g = do
  (gs, k, c) <- cast $ appConc (g, 0) blank
  (_, gls, c') <- cast $ appPrem Nothing (f, k) c
  cast $ litsMap Exact gls gs c'

-- avatarComp :: Form -> Form -> IO Prf -- Ctx
-- avatarComp (Iff s f) g = do
  -- (gs, k0, c0) <- cast $ appOrR (g, 0) blank
  -- (Not g, [conc]) <- pluck gs
  -- ((_, k1), c1) <- cast $ appNotR (Not g, k0) c0
  -- (prem, k2, c2) <- cast (rwlo f g k1 c1) <|> cast (rwlr f g k1 c1)
  -- pairSolve (prem, conc, k2) c2

avatarComp :: Form -> Form -> IO Prf -- Ctx
avatarComp f@(Iff l r) g =
  avatarCompCore f g <|>
  ( do p <- avatarCompCore (Iff r l) g
       return $ Cut (r <=> l) (iffSym l r) p )
avatarComp _ _ = et "avatar-comp : not iff"

avatarCompCore :: Form -> Form -> IO Prf -- Ctx
avatarCompCore (Iff s f) g = do
  (_, ggs) <- cast $ concLits 0 g
  gs <- cast $ deleteOnce (Not s) ggs
  fs <- cast $ premLits f
  vm <- mapSearch HM.empty fs gs
  pf <- usePrem (mapPremLit gs) vm f
  (_, pp) <- useConc 0 g
  return $ pp $ NotR s $ Cut f (iffMP s f) pf
avatarCompCore _ _ = et "avatar-comp-core : not iff"

type BF = (Bnd, Int)

instFaL :: Int -> Form -> BT (Form, Int)
instFaL k (Fa vs f) = do
  let (k', xs) = listFvs k vs
  vxs <- zipM vs xs
  return (substForm vxs f, k')
instFaL k f = return (f, k)

notFun :: Term -> Bool
notFun (Fun _ _ ) = False
notFun _ = True

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

addVM :: Text -> Term -> VM -> Maybe VM
addVM v x gm =
  case HM.lookup v gm of
    Just y ->
      if x == y
      then return gm
      else nt
    _ -> return $ HM.insert v x gm

gndTerm :: Term -> Term
gndTerm (Bv _) = zt
gndTerm (Fun f xs) = Fun f $ L.map gndTerm xs
gndTerm x = x

-- gndTerm :: VM -> (Term , Term) -> IO VM
-- gndTerm gm (Bv t, x) =
--   if Bv t == x
--   then return gm
--   else case HM.lookup t gm of
--          Nothing -> return $ HM.insert t x gm
--          Just y -> do
--            guardMsg (x == y) "Should be equal"
--            return gm
-- gndTerm gm (Fun f xs, Fun g ys) = do
--   guardMsg (f == g) "Cannot ground function"
--   zipM xs ys >>= foldM gndTerm gm
-- gndTerm gm (x, y) = do
--   guardMsg (x == y) $ "Cannot ground pair\n" <> ppTerm x <> "\n" <> ppTerm y <> "\n"
--   return gm

-- Fails at overwrite attempt with new value
addBij :: (Ord k, Ord v) => k -> v -> Bij k v -> Maybe (Bij k v)
addBij k v m@(kv, vk) =
  case (HM.lookup k kv, HM.lookup v vk) of
    (Nothing, Nothing) -> return (HM.insert k v kv, HM.insert v k vk)
    (Just v', Just k') -> guard (v == v' && k == k') >> return m
    _ -> mzero

getBij :: Ord k => k -> Bij k v -> Maybe v
getBij k (kv, _) = HM.lookup k kv

lookupVR :: VR -> Text -> Maybe Text
lookupVR vm v = getBij v (snd vm)

emptyVR :: VR
emptyVR = ([], (HM.empty, HM.empty))

vmts :: VR -> [Term] -> [Term] -> Maybe VR
vmts vm [] [] = return vm
vmts vm (x : xs) (y : ys) = do
  vm' <- vmt vm x y
  vmts vm' xs ys
vmts _ _ _ = nt

vmt :: VR -> Term -> Term -> Maybe VR
vmt vr (Bv v) (Bv w) = 
  case addVR v w vr of 
    Just vr' -> Just vr' 
    _ -> mzero

vmt vm (Fun f xs) (Fun g ys) = do
  guard (f == g) -- "Cannot ground function"
  vmts vm xs ys
vmt vm x y = do
  guard (x == y) -- "Cannot ground pair\n" <> ppTerm x <> "\n" <> ppTerm y <> "\n"
  return vm

fttt :: VR -> (Term , Term) -> Maybe VR
fttt m (Bv v, Bv w) = addVR v w m
fttt gm (Fun f xs, Fun g ys) = do
  guard (f == g) -- "Cannot ground function"
  zipM xs ys >>= foldM fttt gm
fttt gm (x, y) = do
  guard (x == y) -- "Cannot ground pair\n" <> ppTerm x <> "\n" <> ppTerm y <> "\n"
  return gm

agvmt :: VM -> Term -> Term
agvmt gm (Bv v) =
  case HM.lookup v gm of
    Just x -> gndTerm x
    _ -> zt
agvmt gm (Fun f xs) = Fun f $ L.map (agvmt gm) xs
agvmt _ x = x

avmt :: VM -> Term -> Term
avmt gm (Bv v) =
  case HM.lookup v gm of
    Just x -> x
    _ -> Bv v
avmt gm (Fun f xs) = Fun f $ L.map (avmt gm) xs
avmt _ x = x

tavmt :: VM -> Term -> Maybe Term
tavmt gm (Bv v) =
  case HM.lookup v gm of
    Just x -> return x
    _ -> mzero -- return $ Bv v
tavmt gm (Fun f xs) = Fun f <$> mapM (tavmt gm) xs
tavmt _ x = return x

tavmf :: VM -> Form -> Maybe Form
tavmf gm (Eq x y) = do
  x' <- tavmt gm x
  y' <- tavmt gm y
  return (Eq x' y')
tavmf gm (Rel r xs) = Rel r <$> mapM (tavmt gm) xs
tavmf gm (Or fs) = Or <$> mapM (tavmf gm) fs
tavmf gm (And fs) = And <$> mapM (tavmf gm) fs
tavmf gm (Not f) = do
  f' <- tavmf gm f
  return (Not f')
tavmf gm (Imp f g) = do
  f' <- tavmf gm f
  g' <- tavmf gm g
  return (Imp f' g')
tavmf gm (Iff f g) = do
  f' <- tavmf gm f
  g' <- tavmf gm g
  return (Iff f' g')
tavmf gm (Fa vs f) = Fa vs <$> tavmf gm f
tavmf gm (Ex vs f) = Ex vs <$> tavmf gm f

-- gndForm :: VM -> (Form , Form) -> IO VM
-- gndForm b (Eq x0 x1, Eq y0 y1) = foldM gndTerm b [(x0, y0), (x1, y1)] <|> foldM gndTerm b [(x0, y1), (x1, y0)]
-- gndForm b (Rel r xs, Rel s ys) = do
--   pt $ "Gnd attempt :\nf : " <> ppForm (Rel r xs) <> "\ng : " <> ppForm (Rel s ys) <> "\nvm :\n" <> pack (show $ HM.toList b) <> "\n"
--   guard (r == s)
--   xys <- zipM xs ys
--   b' <- foldM gndTerm b xys
--   pt "Gnd-rel success!\n"
--   return b'
-- gndForm b (Not f, Not g) = gndForm b (f, g)
-- gndForm b (And fs, And gs) = zipM fs gs >>= foldM gndForm b
-- gndForm b (Or fs, Or gs)   = zipM fs gs >>= foldM gndForm b
-- gndForm b (Imp f0 f1, Imp g0 g1) = foldM gndForm b [(f0, g0), (f1, g1)]
-- gndForm b (Iff f0 f1, Iff g0 g1) = foldM gndForm b [(f0, g0), (f1, g1)]
-- gndForm gm (Fa vs f, Fa ws g) = do
--   guard (vs == ws)
--   gm' <- gndForm gm (f, g)
--   guard $ L.all (`HM.notMember` gm') vs
--   return gm'
-- gndForm gm (Ex vs f, Ex ws g) = do
--   guard (vs == ws)
--   gm' <- gndForm gm (f, g)
--   guard $ L.all (`HM.notMember` gm') vs
--   return gm'
-- gndForm gm (f, g) = mzero

aoct :: [Term] -> [Text] -> VM -> Term -> Term -> IO VM
aoct zs ws vm (Bv v) y@(Fun f xs) = 
  case HM.lookup v vm of  
    Just x -> guard (x == y) >> return vm
    _ -> guard (isPerm zs xs) >> return (HM.insert v y vm)
aoct zs ws vm x@(Fun f xs) y@(Fun g ys) 
  | x == y = return vm 
  | f == g = foldM2 (aoct zs ws) vm xs ys
  | otherwise = mzero
aoct zs ws vm x y 
  | x == y = return vm 
  | otherwise = mzero

-- isPerm :: [a] -> [a] -> Bool
-- isPerm xs ys = S.fromList xs

-- aocf :: [Term] -> [Text] -> VM -> Form -> Form -> IO VM
-- aocf zs us vm f g  = do 
--   -- pt "============================================================\n"
--   -- pt $ "zs = " <> ppList ppTerm zs <> "\n"
--   -- pt $ "us = " <> ppList id us <> "\n"
--   -- pt $ "f = " <> ppForm f <> "\n"
--   -- pt $ "g = " <> ppForm g <> "\n"
--   aocf zs us vm f g 

aocf :: [Term] -> [Text] -> VM -> Form -> Form -> IO VM
aocf zs ws vm (Eq x y) (Eq z w) = 
  foldM2 (aoct zs ws) vm [x, y] [z, w] <|> foldM2 (aoct zs ws) vm [x, y] [w, z]
aocf zs ws vm (Rel r xs) (Rel s ys) = do 
  guard $ r == s
  foldM2 (aoct zs ws) vm xs ys
aocf zs us vm (Not f) (Not g) = aocf zs us vm f g
aocf zs us vm (Or fs) (Or gs) = foldM2 (aocf zs us) vm fs gs
aocf zs us vm (And fs) (And gs) = foldM2 (aocf zs us) vm fs gs
aocf zs us vm (Fa vs f) (Fa ws g) = do
  guard $ isPerm vs ws 
  aocf zs (ws L.\\ vs) vm f g
aocf zs us vm (Ex vs f) (Ex ws g) = do
  guard $ isPerm vs ws 
  aocf zs (ws L.\\ vs) vm f g
aocf zs ws vm f g = et "aocf : todo"

normalizeAOC :: Form -> IO ([Term], Form)
normalizeAOC (Fa vs (Imp (Ex ws f) g)) = do
  -- let (_, xs) = listFvs 0 ws
  -- wxs <- zipM ws xs
  -- let f' = substForm wxs f
  -- ks <- cast $ mapM breakFv xs
  -- b <- cast $ instForm HM.empty (f', g)
  -- xs' <- mapM (`lookupM` b) ks
  -- return (xs', Fa vs (Imp (Ex ws f) (appBndForm End b f')))
  vm <- aocf (L.map Bv vs) ws HM.empty  f g
  pt $ "VM =\n" <> ppVM vm 
  xs <- cast $ mapM (`HM.lookup` vm) ws
  mark 1
  return (xs, Fa vs (Imp (Ex ws f) (subf vm f)))
normalizeAOC (Imp (Ex ws f) g) = do
  vm <- aocf [] ws HM.empty f g
  xs <- cast $ mapM (`HM.lookup` vm) ws
  return (xs, Imp (Ex ws f) (subf vm f))
  -- let (_, xs) = listFvs 0 ws
  -- wxs <- zipM ws xs
  -- let f' = substForm wxs f
  -- ks <- cast $ mapM breakFv xs
  -- b <- cast $ instForm HM.empty (f', g)
  -- xs' <- mapM (`lookupM` b) ks
  -- return (xs', Imp (Ex ws f) (appBndForm End b f'))
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
mkRdef r (Or [f, Not (Rel s xs)]) = guard (r == s) >> return (Iff (Rel s xs) f)
mkRdef r (Or fs) = do
  (fs', Not (Rel s xs)) <- desnoc fs
  guard (r == s)
  return (Iff (Rel s xs) (Or fs'))
mkRdef _ f = error $ unpack $ "Cannot make rdef :\n" <> ppFormNl f

desnoc :: [a] -> Maybe ([a], a)
desnoc [] = nt
desnoc [x] = return ([], x)
desnoc (x : xs) = DBF.first (x :) <$> desnoc xs

proveRdef :: Form -> Form -> IO Prf
proveRdef (Fa vs f) (Fa ws g) = do
  guard (vs == ws)
  let (_, xs) = listPars 0 vs
  vxs <- zipM vs xs
  p <- proveRdef' (substForm vxs f) (substForm vxs g)
  return $ FaR ws 0 g $ FaL vxs f p
proveRdef f g = proveRdef' f g

rDefLemma0 :: Form -> Form -> Prf
rDefLemma0 f g =
  let p = IffLO f g (mp f g) in -- f, f <=> g |- g
  OrR [g, Not f] [g, Not f] $ NotR f p -- f <=> g |- g \/ ~f

rDefLemma1 :: Form -> [Form] -> [Form] -> Prf
rDefLemma1 r fs fsnr =
  let pl = rDefLemma0 r (Or fs) in -- (r <=> \/ fs) |- (\/ fs) \/ ~r
  let ps = L.map (\ f_ -> (f_, Ax f_)) fs in
  let pfsnr = OrL [(Or fs, OrL ps), (Not r, Ax (Not r))] in -- pfsnr : (\/ fs) \/ ~r |- fs, ~r
  let pr = OrR fsnr fsnr pfsnr in                           -- ps    : (\/ fs) \/ ~r |- \/ fsnr
  Cut (Or [Or fs, Not r]) pl pr -- (r <=> \/ fs) |- \/ fsnr

proveRdef' :: Form -> Form -> IO Prf
proveRdef' f@(Iff fl fr) g@(Iff gl gr) =
  (guard (f == g) >> return (Ax f)) <|>
  (guard (fl == gr && fr == gl) >> return (iffSym fl fr))
proveRdef' (Iff r b) (Or [b', Not r'])  = do
  guard (r == r' && b == b')
  return $ rDefLemma0 r b
proveRdef' (Iff r (Or fs)) (Or fsnr)  = do
  (fs', Not r') <- cast $ desnoc fsnr
  guard (r == r' && fs == fs')
  return $ rDefLemma1 r fs fsnr
proveRdef' f g = et $ "Anomaly! : " <> ppForm f <> " |- " <> ppForm g <> "\n"

relDef :: Text -> Form -> IO Elab
relDef r g = do
  f <- cast $ mkRdef r g
  -- c <- cast $ proveRdef (f, g, 0) blank
  -- p <- cast $ extractPrf c
  p <- proveRdef f g
  return $ Rdef r f p

addTicks :: Int -> Text -> Text
addTicks 0 t = t
addTicks k t = addTicks (k - 1) t <> "'"

type MTT = HM.Map Text Text
type MTI = HM.Map Text Int

rnt :: MTT -> Term -> Maybe Term
rnt m (Bv v) = do
  w <- HM.lookup v m
  return $ Bv w
rnt m (Fun f xs) = Fun f <$> mapM (rnt m) xs
rnt m x = return x

rnfs :: MTT -> MTI -> [Form] -> Maybe ([Form], MTI)
rnfs tt ti [] = return ([], ti)
rnfs tt ti (f : fs) = do
  (f', ti') <- rnf tt ti f
  DBF.first (f' :) <$> rnfs tt ti' fs

rnf :: MTT -> MTI -> Form -> Maybe (Form, MTI)
rnf tt ti (Rel r xs) = do
  xs' <- mapM (rnt tt) xs
  return (Rel r xs', ti)
rnf tt ti (Eq x y) = do
  [x', y'] <- mapM (rnt tt) [x, y]
  return (Eq x' y', ti)
rnf tt ti (Not f) = DBF.first Not <$> rnf tt ti f
rnf tt ti (Or fs) = DBF.first Or <$> rnfs tt ti fs
rnf tt ti (And fs) = DBF.first And <$> rnfs tt ti fs
rnf tt ti (Imp f g) = do
  ([f', g'], ti') <- rnfs tt ti [f, g]
  return (Imp f' g', ti')
rnf tt ti (Iff f g) = do
  ([f', g'], ti') <- rnfs tt ti [f, g]
  return (Iff f' g', ti')
rnf tt ti (Fa vs f) = do
  let (vs', tt', ti') = rnvs tt ti vs
  (f', ti'') <- rnf tt' ti' f
  return (Fa vs' f', ti'')
rnf tt ti (Ex vs f) = do
  let (vs', tt', ti') = rnvs tt ti vs
  (f', ti'') <- rnf tt' ti' f
  return (Ex vs' f', ti'')

rnvs :: MTT -> MTI -> [Text] -> ([Text], MTT, MTI)
rnvs tt ti [] = ([], tt, ti)
rnvs tt ti (v : vs) =
  let k = 1 + HM.findWithDefault (-1) v ti in
  let v' = addTicks k v in
  let tt' = HM.insert v v' tt in
  let ti' = HM.insert v k ti in
  let (vs', tt'', ti'') = rnvs tt' ti' vs in
  (v' : vs', tt'', ti'')

-- elab' :: NSeq -> AnForm -> IO ()
-- elab' s (Af _ g (Just (Gfun "inference" [Gfun r [], _, Glist l]))) = do
--   fs <- cast (mapM gFunFunctor l) >>= mapM (`lookupM` s)
--   inferOut' r fs g
-- elab' _ _ = return ()

elab :: NSeq -> AnForm -> IO Elab
elab s (Af n h (Just (Gfun "file" [_, Gfun m []]))) = do
  f <- getHyp m s
  Plab <$> orig f h
elab _ (Af n g (Just (Gfun "introduced" [Gfun "predicate_definition_introduction" [],Glist [Gfun "new_symbols" [Gfun "naming" [],Glist [Gfun r []]]]]))) = relDef r g
elab _ (Af n g (Just (Gfun "introduced" [Gfun "avatar_definition" [], Glist [Gfun "new_symbols" [Gfun "naming" [], Glist [Gfun r []]]]]))) = relDef r g
elab s (Af n g (Just (Gfun "introduced" [Gfun "choice_axiom" [], Glist []]))) = do
  pt $ "AOC : " <> ppForm g <> "\n"
  (xs, f) <- normalizeAOC g
  -- -- c <- cast$ pairSolve (f, g, 0) blank
  -- -- p <- cast $ extractPrf c
  mark 2
  pt $ "f = " <> ppForm f <> "\n"
  pt $ "g = " <> ppForm g <> "\n"
  p <- orig f g
  mark 3
  return $ AOC xs f p
elab c (Af m g (Just (Gfun "inference" [Gfun "true_and_false_elimination" [], _, Glist [Gfun n []]]))) =
  do f <- getHyp n c
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
  p <- inferOut r fs g
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
            c' <- cast (appAx Pars (f, g, k) c <|> appEqS Pars (f, g, k) c)
            return (gs', c')

cnfTrans :: Form -> Form -> BT Ctx
cnfTrans f g = do
  (gs, k, c) <- cast $ appConc (g, 0) blank
  cnfCore (f, k) (gs, c) <&> snd

-- tryFlipEqL :: Goal -> Ctx -> BT (Goal, Ctx)
-- tryFlipEqL (Eq x y, k) c = return ((Eq x y, k), c) <|> cast (flipEqL (Eq x y, k) c)
-- tryFlipEqL _ _ = []

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

uniEqnsMod :: Term -> Term -> [Eqn] -> Bnd -> BT Bnd
uniEqnsMod x y (eq : eqs) b =
  ( do eqs' <- cast $ eqnToEqns eq
       uniEqnsMod x y (insertAll eqs' eqs) b ) <|>
  ( do b' <- cast $ rewrite x y eq b
       uniEqnsMod x y eqs b' ) <|>
  ( do b' <- cast $ uniNonFun Lax b eq
       uniEqnsMod x y eqs b' )


uniEqnsMod x y [] b = return b

type Eqn = (Term, Term)

uniNonFun :: UniMode -> Bnd -> (Term, Term) -> Maybe Bnd
uniNonFun um b (Fun _ _, Fun _ _) = Nothing
uniNonFun um b xy = uniTerm um b xy

first :: (MonadFail m, Alternative m) => (a -> m b) -> [a] -> m b
first f = Prelude.foldr ((<|>) . f) (MF.fail "")

breakEq :: Form -> Maybe (Term, Term)
breakEq (Eq x y) = Just (x, y)
breakEq _ = Nothing

useConcs :: Int -> [Form] -> IO (Int, Prf -> Prf)
useConcs k [] = return (k, id)
useConcs k (f : fs) = do
  (k', pf) <- useConc k f
  (k'', pf') <- useConcs k' fs
  return (k'', pf . pf')

useConc :: Int -> Form -> IO (Int, Prf -> Prf)
useConc k (Fa vs f) = do
  let (k', vxs) = varPars k vs
  let vm = L.foldl (\ vm_ (v_, x_) -> HM.insert v_ x_ vm_) HM.empty vxs
  let f' = subf vm f
  (k'', pf) <- useConc k' f'
  return (k'', FaR vs k f . pf)
useConc k (Or fs) = do
  (k', pp) <- useConcs k fs
  return (k', OrR fs fs . pp)
useConc k g =
  if isLit g
  then return (k, id)
  else et $ "use conc : not lit : " <> ppForm g

varParsVM :: Int -> [Text] -> (Int, [(Text, Term)], VM)
varParsVM k vs =
  let (k', vxs) = varPars k vs in
  let vm = L.foldl (\ vm_ (v_, x_) -> HM.insert v_ x_ vm_) HM.empty vxs in
  (k', vxs, vm)

concsLits :: Int -> [Form] -> Maybe (Int, [Form])
concsLits k [] = return (k, [])
concsLits k (f : fs) = do
  (k', fs') <- concLits k f
  (k'', fs'') <- concsLits k' fs
  return (k'', fs' ++ fs'')

concLits :: Int -> Form -> Maybe (Int, [Form])
concLits k (Fa vs f) = do
  let (k', vxs) = varPars k vs
  let vm = L.foldl (\ vm_ (v_, x_) -> HM.insert v_ x_ vm_) HM.empty vxs
  let f' = subf vm f
  concLits k' f'
concLits k (Or fs) = concsLits k fs
concLits k f =
  if isLit f
  then return (k, [f])
  else error "conc lits : not lit"

premsLits :: [Form] -> Maybe [Form]
premsLits [] = return []
premsLits (f : fs) = do
  fs' <- premLits f
  fs'' <- premsLits fs
  return $ fs' ++ fs''

premLits :: Form -> Maybe [Form]
premLits (Fa vs f) = premLits f
premLits (Or fs) = premsLits fs
premLits f = guard (isLit f) >> return [f]

removeHead :: Text -> Text
removeHead t =
  case T.uncons t of
    Just (_, t') -> t'
    _ -> t

usVar :: Text -> Text
usVar v = "_" <> v

usTerm :: Term -> Term
usTerm (Bv v) = Bv $ usVar v
usTerm (Fun f xs) = Fun f $ L.map usTerm xs
usTerm x = x

usCla :: Form -> Maybe Form
usCla (Fa vs f) = Fa (L.map usVar vs) <$> usCla f
usCla (Or fs) = Or <$> mapM usCla fs
usCla (Not f) = Not <$> usAtom f
usCla f = usAtom f

usAtom :: Form -> Maybe Form
usAtom (Eq x y)   = return $ Eq (usTerm x) (usTerm y)
usAtom (Rel r xs) = return $ Rel r $ L.map usTerm xs
usAtom _ = nt

isUnderscored :: Text -> Bool
isUnderscored t =
  case T.uncons t of
    Just (c, _) -> c == '_'
    _ -> False

gndt :: Term -> Term
gndt (Bv _) = zt
gndt (Fun f xs) = Fun f $ L.map gndt xs
gndt x = x

claVars :: Form -> IO (Set Text)
claVars (Fa vs f) = S.union (S.fromList vs) <$> claVars f
claVars (Or fs) = S.unions <$> mapM claVars fs
claVars f = guard (isLit f) >> return S.empty

gndVar :: VM -> Text -> VM
gndVar vm v =
  case HM.lookup v vm of
    Just _ -> vm
    _ -> HM.insert v zt vm

-- factor :: Form -> Form -> IO Prf
-- factor f g = do
--   -- pt "Factor :\n"
--   -- pt $ "f : " <> ppForm f <> "\n"
--   -- pt $ "g : " <> ppForm g <> "\n"
--   fls <- cast $ premLits f
--   (_, gls) <- cast $ concLits 0 g
--   -- pt $ "f-lits : " <> ppList ppForm fls <> "\n"
--   -- pt $ "g-lits : " <> ppList ppForm gls <> "\n"
--   avs <- claVars f
--   _vm <- fctr gls (HM.empty, fls)
--   let vm = L.foldl gndVar (HM.map gndt _vm) avs
--   -- pt "Factor success, results :\n"
--   -- pt $ "VM =\n" <> ppVM vm <> "\n"
--   pf <- usePrem (mapPremLit gls) vm f
--   (_, pp) <- useConc 0 g
--   return $ pp pf


efactor :: Maybe Bool -> Form -> Form -> IO Prf
efactor mb f g = do
  -- pt "Eq-Factor :\n"
  -- pt $ "f : " <> ppForm f <> "\n"
  -- pt $ "g : " <> ppForm g <> "\n"
  fls <- cast $ premLits f
  (_, gls) <- cast $ concLits 0 g
  -- pt $ "f-lits : " <> ppList ppForm fls <> "\n"
  -- pt $ "g-lits : " <> ppList ppForm gls <> "\n"
  avs <- claVars f
  _vm <- efctr gls (HM.empty, mb, fls)
  let vm = L.foldl gndVar (HM.map gndt _vm) avs
  -- pt "Eq-Factor success, results :\n"
  -- pt $ "VM =\n" <> ppVM vm <> "\n"
  pf <- usePrem (mapPremLit gls) vm f
  (_, pp) <- useConc 0 g
  return $ pp pf

type EFS = (VM, Maybe Bool, [Form])
type FSTA = (VM, [Form])

isEqn :: Form -> Bool
isEqn (Eq _ _) = True
isEqn _ = False

isSing :: [a] -> Bool
isSing [_] = True
isSing _ = False

fContained :: [Form] -> (Term, Term, Dir, SST) -> Bool
fContained hs (_, y, _, _) =
  let hfs = fsfuns hs in
  let yfs = tfuns y in
  S.isSubsetOf yfs hfs

genSST :: Dir -> [Form] -> (Form, [Form]) -> [(Term, Term, Dir, SST)]
genSST dr gs (Eq x y, fs) = [(x, y, dr, (HM.empty, fs, gs, [])), (y, x, dr, (HM.empty, fs, gs, []))]
genSST dr gs _ = []

genZs :: Dir -> [Form] -> [Form] -> [Form] -> [(Term, Term, Dir, SST)]
genZs dr fs gs hs =
  case unspecs fs hs of
    (_ : _ : _) -> []
    [ffs] -> L.filter (fContained hs) $ genSST dr gs ffs
    [] -> L.concatMap (genSST dr gs) (plucks fs)

ppZ :: Term -> Term -> SST -> [Form] -> Text
ppZ x y (vm, fs, gs, xys) hs =
  "\n================================\n" <>
  "vm : \n" <> ppVM vm <> "\n" <>
  "x : " <> ppTerm x <> "\n" <>
  "y : " <> ppTerm y <> "\n" <>
  "fs :\n" <> ppListNl ppForm fs <>
  "gs :\n" <> ppListNl ppForm gs <>
  "eqs :\n" <> ppListNl ppEq xys <>
  "hs :\n" <> ppListNl ppForm hs

lspec :: Form -> Form -> Bool
lspec (Not f) (Not g) = aspec f g
lspec f g = aspec f g

aspec :: Form -> Form -> Bool
aspec (Rel r xs) (Rel s ys) = r == s && isJust (zipM xs ys >>= specs HM.empty)
aspec (Eq x y) (Eq a b) = isJust $ specs HM.empty [(x, a), (y, b)] <|> specs HM.empty [(x, b), (y, a)]
aspec _ _ = False

activeEqn :: Text -> Form -> Bool
activeEqn ff (Eq x y) =
  let xfs = tfuns x in
  let yfs = tfuns y in
  (S.member ff xfs && not (S.member ff yfs)) || (S.member ff yfs && not (S.member ff xfs))
activeEqn fs _ = False

passiveRel :: Set Text -> Form -> Maybe Text
passiveRel hfs (Eq _ _) = nt
passiveRel hfs f = S.lookupMin $ ffuns f S.\\ hfs

passiveSide :: [Form] -> [Form] -> Bool
passiveSide fls hls =
  let fls' = L.filter (\ f_ -> not (L.any (lspec f_) hls)) fls in
  case fls' of
    [] -> False
    [Eq _ _] -> False
    _ -> True

trivActiveSide :: [Form] -> [Form] -> Bool
trivActiveSide fls hls =
  let fls' = L.filter (\ f_ -> not (L.any (lspec f_) hls)) fls in
  case fls' of
    [] -> False
    [Eq _ _] -> True
    _ -> et "trivial active side : impossible case"

unspecs :: [Form] -> [Form] -> [(Form, [Form])]
unspecs fls hls = L.filter (\ (f_, _) -> not (L.any (lspec f_) hls)) (plucks fls)

resolve :: Form -> Form -> Form -> IO Prf
resolve f g h = do
  ([f', g'], _) <- cast $ rnfs HM.empty HM.empty [f, g]
  fls <- cast $ premLits f'
  gls <- cast $ premLits g'
  (_, hls) <- cast $ concLits 0 h
  (_vm, _p, dr) <- rslv (HM.empty, nt, fls, gls, hls)
  fvs <- claVars f'
  gvs <- claVars g'
  let avs = S.toList (S.union fvs gvs)
  let vm = L.foldl gndVar (HM.map gndt _vm) avs
  let pvt =  subf vm _p
  prff <- prn 0 (f, f')
  prfg <- prn 0 (g, g')
  case dr of
    Obv -> do
      pf <- usePrem (mapPremLit (pvt : hls)) vm f'
      pg <- usePrem (mapPremLit (Not pvt : hls)) vm g'
      (_, ph) <- useConc 0 h
      return $ Cut f' prff $ Cut g' prfg $ ph $ Cut pvt pf $ Cut (Not pvt) pg (NotL pvt $ Ax pvt)
    Rev -> do
      pf <- usePrem (mapPremLit (Not pvt : hls)) vm f'
      pg <- usePrem (mapPremLit (pvt : hls)) vm g'
      (_, ph) <- useConc 0 h
      return $ Cut f' prff $ Cut g' prfg $ ph $ Cut pvt pg $ Cut (Not pvt) pf (NotL pvt $ Ax pvt)

eqfInit :: (Form, [Form]) -> [(Term, Term, [Form])]
eqfInit (Not (Eq x y), fs) = [(x, y, fs), (y, x, fs)]
eqfInit _ = []

eqfactor :: Form -> Form -> IO Prf
eqfactor f g = do
  -- pt "Eq-Factor :\n"
  -- pt $ "f : " <> ppForm f <> "\n"
  -- pt $ "g : " <> ppForm g <> "\n"
  fs <- cast $ premLits f
  (_, ggs) <- cast $ concLits 0 g
  -- pt $ "f-lits : " <> ppList ppForm fs <> "\n"
  -- pt $ "g-lits : " <> ppList ppForm ggs <> "\n"
  let xygs = L.concatMap eqfInit $ plucks ggs
  (vm, x, y, gs) <- first (\ (x_, y_, gs_) -> eqf x_ y_ gs_ (HM.empty, fs, [])) xygs
  -- pt "Eq-Factor solution found.\n"
  -- pt "VM =\n"
  -- pt $ ppVM vm
  -- pt $ "x = " <> ppTerm x <> "\n"
  -- pt $ "y = " <> ppTerm y <> "\n"
  -- pt $ "gs = " <> ppList ppForm gs <> "\n"

  (_, pg) <- useConc 0 g
  pf <- usePrem (\f_ -> first (rwf x y f_) gs) vm f
  let eqpf
        | Not (x === y) `elem` ggs = NotR (x === y) pf
        | Not (y === x) `elem` ggs = NotR (y === x) $ Cut (x === y) (EqS y x) pf
        | otherwise = error "eqf : nonexistent eqn"
  return $ pg eqpf


superpose :: Form -> Form -> Form -> IO Prf
superpose f g h = do
  -- pt $ "f : " <> ppForm f <> "\n"
  -- pt $ "g : " <> ppForm g <> "\n"
  -- pt $ "h : " <> ppForm h <> "\n"
  ([f', g'], _) <- cast $ rnfs HM.empty HM.empty [f, g]
  -- pt $ "f' : " <> ppForm f' <> "\n"
  -- pt $ "g' : " <> ppForm g' <> "\n"
  fls <- cast $ premLits f'
  gls <- cast $ premLits g'
  (_, hls) <- cast $ concLits 0 h
  -- pt "Superposition :\n"
  -- pt $ "f-lits : " <> ppList ppForm fls <> "\n"
  -- pt $ "g-lits : " <> ppList ppForm gls <> "\n"
  -- pt $ "h-lits : " <> ppList ppForm hls <> "\n"
  let zos = genZs Obv fls gls hls
  let zrs = genZs Rev gls fls hls

  -- Prelude.putStr $ "Initial states count = " ++ show (L.length (zos ++ zrs)) ++ "\n"

  (x, y, dr, vm) <- first (\ (x_, y_, dr_, z_) -> super x_ y_ dr_ hls z_) (zos ++ zrs)
  -- pt "Supersolution found.\n"
  prff <- prn 0 (f, f')
  prfg <- prn 0 (g, g')
  (_, ph) <- useConc 0 h
  pfg <- case dr of
           Obv -> suprf vm x y f' g' hls
           Rev -> suprf vm x y g' f' hls
  return $ Cut f' prff $ Cut g' prfg $ ph pfg

rwt :: Term -> Term -> Term -> Term -> IO Prf
rwt x y a@(Fun f xs) b@(Fun g ys)
  | a == b = return $ EqR a
  | x == a && y == b = return $ Ax (x === y)
  | otherwise = do
    guard $ f == g
    xyps <- mapM2 (\ x_ y_ -> (x_, y_,) <$> rwt x y x_ y_) xs ys
    return $ FunC f xyps
rwt x y a b
  | a == b = return $ EqR a
  | x == a && y == b = return $ Ax (x === y)
  | otherwise = mzero

rwf :: Term -> Term -> Form -> Form -> IO Prf
rwf x y (Not f) (Not g) = do
   p <- rwf y x g f
   return $ Cut (y === x) (EqS x y) $ notLR f g p
rwf x y (Rel r xs) (Rel s ys) = do
  guard $ r == s
  xyps <- mapM2 (\ x_ y_ -> (x_, y_,) <$> rwt x y x_ y_) xs ys
  return $ RelC r xyps
rwf x y (Eq a b) (Eq c d) =
  ( do pac <- rwt x y a c
       pbd <- rwt x y b d
       return $ EqC (a, c, pac) (b, d, pbd) ) <|>
  ( do pad <- rwt x y a d
       pbc <- rwt x y b c
       return $ Cut (d === c) (EqC (a, d, pad) (b, c, pbc)) (EqS d c) )

rwf _ _ _ _ = mzero

useEqPremLit :: Term -> Term -> Prf -> Form -> [Form] -> IO Prf
useEqPremLit x y p f@(Eq a b) hs
  | (x == a) && (y == b) = return p
  | (x == b) && (y == a) = return $ Cut (x === y) (EqS a b) p
  | otherwise = mapPremLit hs f
useEqPremLit x y p f hs = mapPremLit hs f

suprf :: VM -> Term -> Term -> Form -> Form -> [Form] -> IO Prf
suprf vm x y f g hs = do
  -- pt "VM =\n"
  -- pt $ ppVM vm
  -- pt $ "x = " <> ppTerm x <> "\n"
  -- pt $ "y = " <> ppTerm y <> "\n"
  -- pt $ "g = " <> ppForm g <> "\n"
  -- pt $ "hs =\n" <> ppListNl ppForm hs <> "\n"
  pp <- usePrem (\g_ -> first (rwf x y g_) hs) vm g
  usePrem (flip (useEqPremLit x y pp) hs) vm f
usePrem :: (Form -> IO Prf) -> VM -> Form -> IO Prf
usePrem  pf vm (Fa vs f) = do
  let vxs = L.map (\ v_ -> (v_, agvmt vm (Bv v_))) vs
  let f' = substForm vxs f
  FaL vxs f <$> usePrem pf vm f'
usePrem pf vm (Or fs) = do
  fps <- mapM (\ f_ -> (f_ ,) <$> usePrem pf vm f_) fs
  return $ OrL fps
usePrem  pf _ f = pf f

mapPremLit :: [Form] -> Form -> IO Prf
mapPremLit hs f@(Not (Rel _ _)) = guard (f `elem` hs) >> return (Ax f)
mapPremLit hs f@(Rel _ _) = guard (f `elem` hs) >> return (Ax f)
mapPremLit hs (Eq x y)
  | Eq x y `elem` hs = return $ Ax (Eq x y)
  | Eq y x `elem` hs = return $ EqS x y
  | otherwise = error "use-prem : eq"
mapPremLit hs f@(Not (Eq x y))
  | f `elem` hs = return $ Ax f
  | x == y = return $ NotL (x === x) $ EqR x
  | Not (Eq y x) `elem` hs = return $ NotL (x === y) $ NotR (y === x) $ EqS y x
  | otherwise = et $ "use-prem-neq : " <> ppForm f
mapPremLit _ _ = error "map-prem-lit : other"

subt :: VM -> Term -> Term
subt vm (Bv v) =
  case HM.lookup v vm of
    Just x -> x
    _ -> Bv v
subt vm (Fun f xs) = Fun f $ L.map (subt vm) xs
subt vm x = x

subf ::  VM -> Form -> Form
subf vm (Eq x y) =
  let x' = subt vm x in
  let y' = subt vm y in
  Eq x' y'
subf vm (Rel r xs) = Rel r $ L.map (subt vm) xs
subf vm (Not f) = Not $ subf vm f
subf vm (Imp f g) =
  let f' = subf vm f in
  let g' = subf vm g in
  Imp f' g'
subf vm (Iff f g) =
  let f' = subf vm f in
  let g' = subf vm g in
  Iff f' g'
subf vm (Or fs)  = Or  $ L.map (subf vm) fs
subf vm (And fs) = And $ L.map (subf vm) fs
subf vm (Fa vs f) =
  let vm' = HM.filterWithKey (\ v_ _ -> v_ `notElem` vs) vm in
  Fa vs $ subf vm' f
subf vm (Ex vs f) =
  let vm' = HM.filterWithKey (\ v_ _ -> v_ `notElem` vs) vm in
  Ex vs $ subf vm' f

substVar :: Text -> Term -> Term -> Term
substVar v x (Bv w) = if v == w then x else Bv w
substVar v x (Par m) = Par m
substVar v x (Fv m) = Fv m
substVar v x (Fun f xs) = Fun f $ L.map (substVar v x) xs

ubv :: VM -> Text -> Term -> Maybe VM
-- scheme  : ubv vm v x
-- assumes : v is unbound in vm
-- assumes : if x is a variable, it is also unbound in vm
ubv vm v x =
  let x' = avmt vm x in
  let vm' = HM.map (substVar v x') vm in
  do guard $ not $ hasVar v x'
     return $ HM.insert v x' vm'

hasVar :: Text -> Term -> Bool
hasVar v (Bv w) = v == w
hasVar v (Fun _ xs) = L.any (hasVar v) xs
hasVar _ _ = False

lookUpTerm :: VM -> Term -> Maybe Term
lookUpTerm vm (Bv v) = HM.lookup v vm
lookUpTerm vm _ = nt


utrw :: VM -> Term -> Term -> Term -> Term -> Maybe VM
utrw vm x y a@(Fun f xs) b@(Fun g ys) =
  ut vm a b <|>
  ( do vm' <- ut vm x a
       ut vm' y b ) <|>
  ( do xys <- zipM xs ys
       foldM (\ vm_ (a_, b_) -> utrw vm_ x y a_ b_) vm xys )
utrw vm x y a b =
  ut vm a b <|>
  ( do vm' <- ut vm x a
       ut vm' y b )

ut :: VM -> Term -> Term -> Maybe VM
ut vm (Fv k) (Fv m) = guard (k == m) >> return vm
ut vm (Par k) (Par m) = guard (k == m) >> return vm
ut vm (Fun f xs) (Fun g ys) = do
  guard $ f == g
  xys <- zipM xs ys
  foldM (\ vm_ (x_, y_) -> ut vm_ x_ y_) vm xys
ut vm x y =
  if x == y
  then return vm
  else
    case (lookUpTerm vm x, lookUpTerm vm y, x, y) of
       (Just x', _, _, _) -> ut vm x' y
       (_, Just y', _, _) -> ut vm x y'
       (_, _, Bv v, _) -> ubv vm v y
       (_, _, _, Bv w) -> ubv vm w x
       (_, _, _, _) -> nt

utrws :: VM -> Term -> Term -> [(Term, Term)] -> Maybe VM
utrws vm x y [] = return vm
utrws vm x y ((a, b) : abs) = do
  vm' <- utrw vm x y a b
  utrws vm' x y abs

uts :: VM -> [(Term, Term)] -> Maybe VM
uts vm [] = return vm
uts vm ((x, y) : xys) = do
  vm' <- ut vm x y
  uts vm' xys

ul :: VM -> Form -> Form -> [VM]
ul vm (Not f) (Not g) = ua vm f g
ul vm f g = ua vm f g

ua :: VM -> Form -> Form -> [VM]
ua vm (Eq s t) (Eq u v) =
  case (uts vm [(s, u), (t, v)], uts vm [(s, v), (t, u)]) of
    (Just vm', Just vm'') ->
      case (HM.isSubmapOf vm' vm'', HM.isSubmapOf vm'' vm') of
        (True, _) -> [vm']
        (_, True) -> [vm'']
        _ -> [vm', vm'']
    (Just vm', _) -> [vm']
    (_, Just vm') -> [vm']
    _ -> []
ua vm (Rel r xs) (Rel s ys) =
  if r == s
  then cast $ zipM xs ys >>= uts vm
  else []
ua vm _ _ = []

type RSTA = (VM, Maybe (Form, Dir), [Form], [Form], [Form])

type SST = (VM, [Form], [Form], [(Term, Term)])

getShortList :: [[a]] -> Maybe [a]
getShortList [] = nt
getShortList [xs] = return xs
getShortList ([] : xss) = return []
getShortList ([x] : xss) = return [x]
getShortList (xs : xss) = do
  ys <- getShortList xss
  if L.length xs < L.length ys
  then return xs
  else return ys

genPvtAux :: VM -> Form -> Form -> [(VM, Form, Dir)]
genPvtAux vm (Not _) (Not _) = []
genPvtAux vm f (Not g) = L.map (, f, Obv) $ ua vm f g
genPvtAux vm (Not f) g = L.map (, f, Rev) $ ua vm f g
genPvtAux _ _ _ = []

genPvt :: VM -> (Form, [Form]) -> (Form, [Form]) -> [Form] -> [RSTA]
genPvt vm (f, fs) (g, gs) hs =
  L.map (\ (vm_, pvt_, dr_) -> (vm_, Just (pvt_, dr_), fs, gs, hs)) $ genPvtAux vm f g

genEF :: VM -> Maybe Bool -> Form -> [Form] -> [EFS]
genEF vm mb (Not (Eq x y)) fs =
  case ut vm x y of
    Just vm' -> [(vm', if isJust mb then Just True else nt, fs)]
    _ -> []
genEF _ _ _ _ = []

genFcs :: VM -> [Form] -> (Form, [Form]) -> [FSTA]
genFcs vm gs (f, fs) = L.map (, fs) (L.concatMap (ul vm f) gs)

genEFSs :: VM -> Maybe Bool -> [Form] -> (Form, [Form]) -> [EFS]
genEFSs vm (Just True) gs (f, fs) = L.map (, Just True, fs) (L.concatMap (ul vm f) gs)
genEFSs vm mb gs (f, fs) =
  genEF vm mb f fs ++ L.map (, mb, fs) (L.concatMap (ul vm f) gs)

genLftRss :: VM -> Maybe (Form, Dir) -> [Form] -> [Form] -> (Form, [Form]) -> [RSTA]
genLftRss vm Nothing gs hs (f, fs) =
  L.concatMap (\ (g_, gs_) -> genPvt vm (f, fs) (g_, gs_) hs) (plucks gs) ++ L.map (, nt, fs, gs, hs) (L.concatMap (ul vm f) hs)
genLftRss vm pd@(Just (pvt, Obv)) gs hs (f, fs) = L.map (, pd, fs, gs, hs) $ L.concatMap (ul vm f) (pvt : hs)
genLftRss vm pd@(Just (pvt, Rev)) gs hs (f, fs) = L.map (, pd, fs, gs, hs) $ L.concatMap (ul vm f) (Not pvt : hs)

genRgtRss :: VM -> Maybe (Form, Dir) -> [Form] -> [Form] -> (Form, [Form]) -> [RSTA]
genRgtRss vm Nothing fs hs (g, gs) =
  L.concatMap (\ (f_, fs_) -> genPvt vm (f_, fs_) (g, gs) hs) (plucks fs) ++ L.map (, nt, fs, gs, hs) (L.concatMap (ul vm g) hs)
genRgtRss vm pd@(Just (pvt, Obv)) fs hs (g, gs) = L.map (, pd, fs, gs, hs) $ L.concatMap (ul vm g) (Not pvt : hs)
genRgtRss vm pd@(Just (pvt, Rev)) fs hs (g, gs) = L.map (, pd, fs, gs, hs) $ L.concatMap (ul vm g) (pvt : hs)

superatv :: VM -> [Form] -> [Form] -> [(Term, Term)] -> (Form, [Form]) -> [SST]
superatv vm hs gs xys (f, fs) = L.map (, fs, gs, xys) $ L.concatMap (ul vm f) hs

superpsv :: VM -> Term -> Term -> [Form] -> [Form] -> [(Term, Term)] -> (Form, [Form]) -> [SST]
superpsv vm x y hs fs xys (g, gs) = L.map (\ xys_ ->(vm, fs, gs, xys_ ++ xys)) $ L.concatMap (genRWEqs g) hs

supereq :: VM -> Term -> Term -> [Form] -> [Form] -> [Form] -> ((Term, Term), [(Term, Term)]) -> [SST]
supereq vm x y hs fs gs ((Fun f xs, Fun g ys), xys) =
  let lft = case ut vm (Fun f xs) (Fun g ys) of
              Just vm' -> [(vm', fs, gs, xys)]
              _ -> [] in
  let mid = case uts vm [(x, Fun f xs), (y, Fun g ys)] of
              Just vm' -> [(vm', fs, gs, xys)]
              _ -> [] in
  let rgt = case (f == g, zipM xs ys) of
              (True, Just abs) -> [(vm, fs, gs, abs ++ xys)]
              _ -> [] in
  lft ++ mid ++ rgt
supereq vm x y hs fs gs ((a, b), xys) =
  let lft = case ut vm a b of
              Just vm' -> [(vm', fs, gs, xys)]
              _ -> [] in
  let rgt = case uts vm [(x, a), (y, b)] of
              Just vm' -> [(vm', fs, gs, xys)]
              _ -> [] in
  lft ++ rgt

genRWEqs :: Form -> Form -> [[(Term, Term)]]
genRWEqs (Not g) (Not h) = genRWEqs g h
genRWEqs (Rel r xs) (Rel s ys) =
  case (r == s, zipM xs ys) of
    (True, Just xys) -> [xys]
    _ -> []
genRWEqs (Eq x y) (Eq a b) = [[(x, a), (y, b)], [(x, b), (y, a)]]
genRWEqs _ _ = []

type EQFS = (VM, [Form], [(Term, Term)])
eqfbranch :: Term -> Term -> [Form] -> EQFS -> [[EQFS]]
eqfbranch x y gs (vm, fs, xys) =
  let fbs = L.map (eqfLit vm gs xys) (plucks fs) in
  let ebs = L.map (eqfEq vm x y fs) (plucks xys) in
  fbs ++ ebs

eqfLit :: VM -> [Form] -> [(Term, Term)] -> (Form, [Form]) -> [EQFS]
eqfLit vm gs xys (f, fs) = L.map (\ xys_ ->(vm, fs, xys_ ++ xys)) $ L.concatMap (genRWEqs f) gs

eqfEq :: VM -> Term -> Term -> [Form] -> ((Term, Term), [(Term, Term)]) -> [EQFS]
eqfEq vm x y fs ((Fun f xs, Fun g ys), xys) =
  let lft = case ut vm (Fun f xs) (Fun g ys) of
              Just vm' -> [(vm', fs, xys)]
              _ -> [] in
  let mid = case uts vm [(x, Fun f xs), (y, Fun g ys)] of
              Just vm' -> [(vm', fs, xys)]
              _ -> [] in
  let rgt = case (f == g, zipM xs ys) of
              (True, Just abs) -> [(vm, fs, abs ++ xys)]
              _ -> [] in
  lft ++ mid ++ rgt
eqfEq vm x y fs ((a, b), xys) =
  let lft = case ut vm a b of
              Just vm' -> [(vm', fs, xys)]
              _ -> [] in
  let rgt = case uts vm [(x, a), (y, b)] of
              Just vm' -> [(vm', fs, xys)]
              _ -> [] in
  lft ++ rgt

superbranch :: Term -> Term -> [Form] -> SST -> [[SST]]
superbranch x y hs (vm, fs, gs, xys) =
  let abs = L.map (superatv vm hs gs xys) (plucks fs) in
  let pbs = L.map (superpsv vm x y hs fs xys) (plucks gs) in
  let ebs = L.map (supereq vm x y hs fs gs) (plucks xys) in
  abs ++ pbs ++ ebs

genRss :: RSTA -> [[RSTA]]
genRss (vm, pd, fs, gs, hs) =
  let lft = L.map (genLftRss vm pd gs hs) (plucks fs) in
  let rgt = L.map (genRgtRss vm pd fs hs) (plucks gs) in
  lft ++ rgt

type Pivot = Maybe (Form, Dir)

ppDir :: Dir -> Text
ppDir Obv = "====>"
ppDir Rev = "<===="

ppPivot :: Pivot -> Text
ppPivot (Just (p, d)) = ppForm p <> " | " <> ppDir d
ppPivot _ = "unknown"

ppRsta :: RSTA -> Text
ppRsta (vm, pvt, fs, gs, hs) =
  "VM =\n" <> ppVM vm <>
  "\nPivot = " <> ppPivot pvt <>
  "\nFs = " <> ppForms fs <>
  "\nGs = " <> ppForms gs <>
  "\nHs = " <> ppForms hs

fctr :: [Form] -> FSTA -> IO VM
fctr _ (vm, []) = return vm
fctr gs (vm, fs) = do
  let fcss = L.map (genFcs vm gs) (plucks fs)
  guard $ [] `notElem` fcss
  fcs <- cast $ getShortList fcss
  first (fctr gs) fcs

efctr :: [Form] -> EFS -> IO VM
efctr _ (vm, _, []) = return vm
efctr gs (vm, mb, fs) = do
  let fcss = L.map (genEFSs vm mb gs) (plucks fs)
  guard $ [] `notElem` fcss
  fcs <- cast $ getShortList fcss
  first (efctr gs) fcs

mapSearchBranch :: VM -> [Form] -> [Form] -> [[(VM, [Form])]]
mapSearchBranch vm fs gs =
  L.map (\ (f_, fs_) -> L.concatMap (L.map (, fs_) . ul vm f_) gs) (plucks fs)

mapSearch :: VM -> [Form] -> [Form] -> IO VM
mapSearch vm [] _ = return vm
mapSearch vm fs gs = do
  let zss = mapSearchBranch vm fs gs
  guard $ [] `notElem` zss
  zs <- cast $ getShortList zss
  first (\ (vm_, fs_) -> mapSearch vm_ fs_ gs) zs

search :: (Eq a, Show a) => (a -> Bool) -> (a -> [[a]]) -> a -> IO a
search tm br x = 
  if tm x 
  then return x
  else do let zss = br x 
          guard $ [] `notElem` zss
          zs <- cast $ getShortList zss
          first (search tm br) zs

super :: Term -> Term -> Dir -> [Form] -> SST -> IO (Term, Term, Dir, VM)
super x y dr hs (vm , [], [], []) = do
  let x' = agvmt vm x
  let y' = agvmt vm y
  return (x', y', dr, vm)
super x y dr hs z = do
  -- pt $ ppZ x y z hs 
  let zss = superbranch x y hs z
  guard $ [] `notElem` zss
  zs <- cast $ getShortList zss
  first (super x y dr hs) zs

eqf :: Term -> Term -> [Form] -> (VM, [Form], [(Term, Term)]) -> IO (VM, Term, Term, [Form])
eqf x y gs (vm, [], []) = return (vm, x, y, gs)
eqf x y gs z = do
  let zss = eqfbranch x y gs z
  guard $ [] `notElem` zss
  zs <- cast $ getShortList zss
  first (eqf x y gs) zs

rslv :: RSTA -> IO (VM, Form, Dir)
rslv (vm, Just (pvt, dr), [], [], _) = return (vm, pvt, dr)
rslv r = do
  let rss = genRss r
  guard $ [] `notElem` rss
  rs <- cast $ getShortList rss
  first rslv rs

bvsOccurTerm :: [Text] -> Term -> Bool
bvsOccurTerm vs (Bv v) = v `elem` vs
bvsOccurTerm vs (Fun f xs) = L.any (bvsOccurTerm vs) xs
bvsOccurTerm vs _ = False

bvsOccurForm :: [Text] -> Form -> Bool
bvsOccurForm vs (Eq x y) = bvsOccurTerm vs x || bvsOccurTerm vs y
bvsOccurForm vs (Rel _ xs) = L.any (bvsOccurTerm vs) xs
bvsOccurForm vs (Not f) = bvsOccurForm vs f
bvsOccurForm vs (And fs) = L.any (bvsOccurForm vs) fs
bvsOccurForm vs (Or fs)  = L.any (bvsOccurForm vs) fs
bvsOccurForm vs (Imp f g) = bvsOccurForm vs f || bvsOccurForm vs g
bvsOccurForm vs (Iff f g) = bvsOccurForm vs f || bvsOccurForm vs g
bvsOccurForm vs (Fa ws f) = bvsOccurForm (vs L.\\ ws) f
bvsOccurForm vs (Ex ws f) = bvsOccurForm (vs L.\\ ws) f

bvint :: Text -> Term -> Bool
bvint v (Bv w) = v == w
bvint v (Fun f xs) = L.any (bvint v) xs
bvint v _ = False

bvinf :: Text -> Form -> Bool
bvinf v (Eq x y) = bvint v x || bvint v y
bvinf v (Rel _ xs) = L.any (bvint v) xs
bvinf v (Not f) = bvinf v f
bvinf v (And fs) = L.any (bvinf v) fs
bvinf v (Or fs)  = L.any (bvinf v) fs
bvinf v (Imp f g) = bvinf v f || bvinf v g
bvinf v (Iff f g) = bvinf v f || bvinf v g
bvinf v (Fa ws f) = v `notElem` ws && bvinf v f
bvinf v (Ex ws f) = v `notElem` ws && bvinf v f

pointLessQuant :: Form -> Bool
pointLessQuant (Fa vs f) = not $ bvsOccurForm vs f
pointLessQuant (Ex vs f) = not $ bvsOccurForm vs f
pointLessQuant _ = False

type PSS = (VR, [(Form, Form)])

forkRect :: VR -> ((Form, Form), [(Form, Form)]) -> [PSS]
forkRect vr ((Rel r xs, Rel s ys), fgs)
  | r == s =
    case vmts vr xs ys of
      Just vr' -> [(vr', fgs)]
      _ -> []
forkRect vr ((Eq x y, Eq a b), fgs) =
  let mb0 = vmts vr [x, y] [a, b] in
  let mb1 = vmts vr [x, y] [b, a] in
  L.map (, fgs) $ cast mb0 ++ cast mb1
forkRect vr ((Not f, Not g), fgs) = [(vr, (f, g) : fgs)]
forkRect (lps, bj) ((Fa vs f, Fa ws g), fgs) = [(((vs, ws) : lps, bj), (f, g) : fgs)]
forkRect (lps, bj) ((Ex vs f, Ex ws g), fgs) = [(((vs, ws) : lps, bj), (f, g) : fgs)]
forkRect vr ((Imp e f, Imp g h), fgs) = [(vr, (e, g) : (f, h) : fgs)]
forkRect vr ((Iff e f, Iff g h), fgs) = [(vr, (e, g) : (f, h) : fgs)]
forkRect vr ((Or [], Or []), fgs) = [(vr, fgs)]
forkRect vr ((And [], And []), fgs) = [(vr, fgs)]
forkRect vr ((Or (f : fs), Or ggs), fgs) = 
  L.map (\ (g_, gs_) -> (vr, (f, g_) : (Or fs, Or gs_) : fgs)) (plucks ggs)
forkRect vr ((And (f : fs), And ggs), fgs) = 
  L.map (\ (g_, gs_) -> (vr, (f, g_) : (And fs, And gs_) : fgs)) (plucks ggs)
forkRect vr ((_, _),  _) = mzero

forkOrig :: VR -> ((Form, Form), [(Form, Form)]) -> [PSS]
forkOrig vr ((Rel r xs, Rel s ys), fgs)
  | r == s =
    case vmts vr xs ys of
      Just vr' -> [(vr', fgs)]
      _ -> []
forkOrig vr ((Eq x y, Eq a b), fgs) =
  let mb0 = vmts vr [x, y] [a, b] in
  let mb1 = vmts vr [x, y] [b, a] in
  L.map (, fgs) $ cast mb0 ++ cast mb1
forkOrig vr ((Not f, Not g), fgs) = [(vr, (f, g) : fgs)]
forkOrig vr ((Not (Not f), g), fgs) = [(vr, (f, g) : fgs)]
forkOrig (lps, bj) ((Fa vs f, Fa ws g), fgs) = [(((vs, ws) : lps, bj), (f, g) : fgs)]
forkOrig (lps, bj) ((Ex vs f, Ex ws g), fgs) = [(((vs, ws) : lps, bj), (f, g) : fgs)]
forkOrig vr ((Imp e f, Imp g h), fgs) = [(vr, (e, g) : (f, h) : fgs)]
forkOrig vr ((Iff e f, Iff g h), fgs) = [(vr, (e, g) : (f, h) : fgs)]
forkOrig vr ((Or [], Or []), fgs) = [(vr, fgs)]
forkOrig vr ((And [], And []), fgs) = [(vr, fgs)]
forkOrig vr ((Or (f : fs), Or ggs), fgs) 
  | L.any isOr (f : fs) = [(vr, (Or (flatOr (f : fs)), Or ggs) : fgs)] 
  | L.any isOr ggs = [(vr, (Or (f : fs), Or (flatOr ggs)) : fgs)] 
  | otherwise = L.map (\ (g_, gs_) -> (vr, (f, g_) : (Or fs, Or gs_) : fgs)) (plucks ggs)
forkOrig vr ((And (f : fs), And ggs), fgs) 
  | L.any isAnd (f : fs) = [(vr, (And (flatAnd (f : fs)), And ggs) : fgs)] 
  | L.any isAnd ggs = [(vr, (And (f : fs), And (flatAnd ggs)) : fgs)] 
  | otherwise = L.map (\ (g_, gs_) -> (vr, (f, g_) : (And fs, And gs_) : fgs)) (plucks ggs)
forkOrig vr ((_, _),  _) = mzero
-- fod (vm, Not (Not f), g) =   [(vm, [(f, g)])]
-- fod (vm, Imp e f, Imp g h) = [(vm, [(e, g), (f, h)])]
-- fod (vm, Iff e f, Iff g h) = [(vm, [(e, g), (f, h)])]
-- fod ((lps, bj), Fa vs f, Fa ws g) =
--   if L.length vs == L.length ws
--   then [(((vs, ws) : lps, bj), [(f, g)])]
--   else []
-- fod ((lps, bj), Ex vs f, Ex ws g) =
--   if L.length vs == L.length ws
--   then [(((vs, ws) : lps, bj), [(f, g)])]
--   else []

searchRect :: (VR, [(Form, Form)]) -> IO (VR, [(Form, Form)])
searchRect = search (L.null . snd) (\ (vr_, fgs_) -> L.map (forkRect vr_) (plucks fgs_))

searchOrig :: (VR, [(Form, Form)]) -> IO (VR, [(Form, Form)])
searchOrig = search (L.null . snd) (\ (vr_, fgs_) -> L.map (forkOrig vr_) (plucks fgs_))

revart :: HM.Map Text Text -> Term -> Term
revart vw (Bv v) = 
  case HM.lookup v vw of 
    Just x -> Bv x
    _ -> et "revart : no mapping"
revart vw (Fun f xs) = Fun f $ L.map (revart vw) xs
revart vw x = x

revarf :: HM.Map Text Text -> Form -> Form
revarf vw (Not f) = Not $ revarf vw f
revarf vw (Fa vs f) = Fa (L.map (\ v_ -> HM.findWithDefault "_" v_ vw) vs) $ revarf vw f
revarf vw (Ex vs f) = Ex (L.map (\ v_ -> HM.findWithDefault "_" v_ vw) vs) $ revarf vw f
revarf vw (Imp f g) = Imp (revarf vw f) (revarf vw g)
revarf vw (Iff f g) = Iff (revarf vw f) (revarf vw g)
revarf vw (Or fs) = Or $ L.map (revarf vw) fs
revarf vw (And fs) = And $ L.map (revarf vw) fs
revarf vw (Rel r xs) = Rel r $ L.map (revart vw) xs
revarf vw (Eq x y) = Eq (revart vw x) (revart vw y)

norm :: Form -> Form
norm (Not f) = Not $ norm f
norm (Fa vs f) = 
  case L.filter (`bvinf` f) vs of 
    [] -> norm f 
    vs' -> Fa vs' $ norm f
norm (Ex vs f) = 
  case L.filter (`bvinf` f) vs of 
    [] -> norm f 
    vs' -> Ex vs' $ norm f
norm (Imp f g) = Imp (norm f) (norm g)
norm (Iff f g) = Iff (norm f) (norm g)
norm (Or fs) = Or $ L.map norm fs
norm (And fs) = And $ L.map norm fs
norm f@(Rel r xs) = f
norm f@(Eq x y) = f

pnm :: Int -> Form -> Form -> IO Prf
pnm _ _ _ = return Asm

pairSolve :: Form -> Form -> IO Prf
pairSolve f g = do
  pt $ "g  = " <> ppForm g <> "\n"
  let nf = norm f
  pt $ "nf  = " <> ppForm nf <> "\n"
  ([_, nf'], _) <- cast $ rnfs HM.empty HM.empty [g, nf]
  pt $ "nf'  = " <> ppForm nf' <> "\n"
  (vr, _) <- searchRect (emptyVR, [(nf', g)])
  pt $ "VR  =\n" <> ppVR vr <> "\n"
  let rf = revarf (fst $ snd vr) nf'
  pt $ "rf  = " <> ppForm rf <> "\n"
  p0 <- pnm 0 f nf
  p1 <- prn 0 (nf, rf)
  p2 <- psol 0 rf g
  return $ 
    Cut nf (Cut (f <=> nf) p0 $ iffMP f nf) $ 
      Cut rf (Cut (nf <=> rf) p1 $ iffMP nf rf) $ 
        Cut (rf <=> g) p2 (iffMP rf g)

  -- ([_, f'], _) <- cast $ rnfs HM.empty HM.empty [g, f]
  -- pt $ "f' = " <> ppForm f' <> "\n"
  -- let nf = norm f'
  -- pt $ "nf = " <> ppForm nf <> "\n"
  -- ((_, (vw, _)), _) <- searchRect (emptyVR, [(nf, g)])
  -- let rf = revarf vw f 
  -- pt $ "rf = " <> ppForm rf <> "\n"
  -- p0 <- prn 0 (f, f')
  -- p2 <- psol vw HM.empty HM.empty 0 nf g

vacFaIff :: Int -> [Text] -> Form -> Prf
vacFaIff k vs f = iffRFull (Fa vs f) f (FaL (L.map (, zt) vs) f $ Ax f) (FaR vs k f $ Ax f)

iffVacFa :: Int -> [Text] -> Form -> Prf
iffVacFa k vs f = iffRFull f (Fa vs f) (FaR vs k f $ Ax f) (FaL (L.map (, zt) vs) f $ Ax f)

iffVacEx :: Int -> [Text] -> Form -> Prf
iffVacEx k vs f = iffRFull f (Ex vs f) (ExR (L.map (, zt) vs) f $ Ax f) (ExL vs k f $ Ax f)

vacExIff :: Int -> [Text] -> Form -> Prf
vacExIff k vs f = iffRFull (Ex vs f) f (ExL vs k f $ Ax f) (ExR (L.map (, zt) vs) f $ Ax f)

alphaFa :: Int -> [Text] -> Form -> [Text] -> Form -> IO Prf
alphaFa k vs f ws g = do
  let (_, wxs) = varPars k ws
  vxs <- mapM2 (\ v_ (_, x_) -> return (v_, x_)) vs wxs
  let f' = substForm vxs f
  let g' = substForm wxs g
  guard $ f' == g'
  -- return $ FaR ws k g $ FaL vxs f $ Ax f'
  return $ iffRFull (Fa vs f) (Fa ws g)
    (FaR ws k g $ FaL vxs f $ Ax f')
    (FaR vs k f $ FaL wxs g $ Ax f')

alphaEx :: Int -> [Text] -> Form -> [Text] -> Form -> IO Prf
alphaEx k vs f ws g = do
  let (_, wxs) = varPars k ws
  vxs <- mapM2 (\ v_ (_, x_) -> return (v_, x_)) vs wxs
  let f' = substForm vxs f
  let g' = substForm wxs g
  guard $ f' == g'
  return $ iffRFull (Ex vs f) (Ex ws g)
    (ExL vs k f $ ExR wxs g $ Ax f')
    (ExL ws k g $ ExR vxs f $ Ax f')

psol :: Int -> Form -> Form -> IO Prf
psol _ (Eq x y) (Eq a b)
  | x == a && y == b = return $ iffRefl (x === y)
  | x == b && y == a = return $ iffRFull (x === y) (a === b) (EqS x y) (EqS a b)
psol k f@(Rel _ _) g@(Rel _ _) = guard (f == g) >> return (iffRefl f)
psol k (Not f) (Not g) = do
  p <- psol k f g 
  return $ Cut (f <=> g) p $ iffToNotIffNot f g
psol k (Fa vs f) (Fa ws g) = do
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- psol k' f' g'
  Cut (Fa vs (f <=> g)) (FaR vs k (f <=> g) p) <$> faIffToFaIffFa' k vs f ws g
psol k (Ex vs f) (Ex ws g) = do
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- psol k' f' g'
  Cut (Fa vs (f <=> g)) (FaR vs k (f <=> g) p) <$> faIffToExIffEx' k vs f ws g
psol k (And fs) (And gs) = do
  fgps <- psols k fs gs  --mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> psol k f_ g_) fs gs
  let fgs = L.map (\ (f_, g_, _) -> (f_, g_)) fgps
  let iffps = L.map (\ (f_, g_, p_) -> (f_ <=> g_, p_)) fgps
  cuts iffps <$> cast (iffsToAndIffAnd' fgs fs gs)
psol k (Or fs) (Or gs) = do
  -- fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> psol k f_ g_) fs gs
  fgps <- psols k fs gs
  let fgs = L.map (\ (f_, g_, _) -> (f_, g_)) fgps
  let iffps = L.map (\ (f_, g_, p_) -> (f_ <=> g_, p_)) fgps
  cuts iffps <$> cast (iffsToOrIffOr' fgs fs gs)
psol k (Imp e f) (Imp g h) = do
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> psol k f_ g_) [e, f] [g, h]
  return $ cuts fgps $ impCong e f g h
psol k (Iff e f) (Iff g h) = do
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> psol k f_ g_) [e, f] [g, h]
  return $ cuts fgps $ iffCong e f g h
psol _ f g = et $ "Todo :\n  f : " <> ppForm f <> "\n  g : " <> ppForm g <> "\n"

psols :: Int -> [Form] -> [Form] -> IO [(Form, Form, Prf)]
psols k [] [] = return []
psols k [] _ = mzero
psols k (f : fs) ggs = 
  first 
    ( \ (g_, gs_) -> do 
         p <- psol k f g_ 
         fgps <- psols k fs gs_
         return ((f, g_, p) : fgps) ) 
    (plucks ggs)



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

-- flipEqR :: Goal -> Ctx -> Maybe (Goal, Ctx)
-- flipEqR (Eq x y, k) c0 = do
--   (m, n, c1) <- appCut nt (Eq y x, k) c0
--   c2 <- appEqS Exact (Eq y x, Eq x y, n) c1
--   return ((y === x, m), c2)
-- flipEqR _ _ = Nothing
-- 
-- flipLitR :: Goal -> Ctx -> Maybe (Goal, Ctx)
-- flipLitR (Not (Eq x y), k) c0 = do
--   (m, n, c1) <- appCut nt (Not (y === x), k) c0
--   c2 <- appNotLR (Not (y === x), Not (x === y), n) c1 >>= uncurry (appEqS Exact)
--   return ((Not (y === x), m), c2)
-- flipLitR (l, k) c = flipEqR (l, k) c
-- 
-- flipLitsR :: [Form] -> Int -> Ctx -> ([Form], Int, Ctx)
-- flipLitsR [] k c = ([], k, c)
-- flipLitsR (l : ls) k c =
--   let (ls', k', c') = flipLitsR ls k c in
--   case flipLitR (l, k') c' of
--     Just ((l', k''), c'') -> (l : l' : ls', k'', c'')
--     _ -> (l : ls', k', c')

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
breakOr (Or ls) =
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

disjMap :: [Goal] -> [Form] -> Ctx -> Maybe Ctx
disjMap [] _ c = return c
disjMap ((f, k) : fks) gs c = do
  c' <- appBotL (f, k) c <|> first (\ g -> appAx Exact (f, g, k) c) gs
  disjMap fks gs c'

litsMap :: UniMode -> [Goal] -> [Form] -> Ctx -> BT Ctx
litsMap _ [] _ c = return c
litsMap um ((f, k) : gls) gs c =
  let gl = (f, k) in
  cast (appBotL gl c) <|>
    cast (appNotL gl c >>= uncurry (appEqR um)) <|>
    ( do g <- pick gs
         matchLit um (f, g, k) c ) >>=
    litsMap um gls gs

main :: IO ()
main = do
  (tptp : tstp : flags) <- getArgs
  tptp_afs <- parseName tptp
  tstp_afs <- sortAfs <$> parseName tstp
  let hs = L.foldl addHyp (HM.empty, S.empty) tptp_afs
  Prelude.putStr $ tptp ++ "\n"
  if "silent" `elem` flags
    then return ()
    else mapM_ putAnForm tptp_afs
  Prelude.putStr $ tstp ++ "\n"
  if "silent" `elem` flags
    then return ()
    else mapM_ putAnForm tstp_afs
  foldM_ elabIO hs tstp_afs
  Prelude.putStr "Elab complete.\n\n"

  -- Prelude.putStr $ tstp ++ "\n"
  -- tstp_afs <- sortAfs <$> parseName tstp
  -- foldM_ elabIO' HM.empty tstp_afs
  -- Prelude.putStr "Elab complete.\n\n"