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
-- import App
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

isOr :: Form -> Bool
isOr (Or _) = True
isOr _ = False

isAnd :: Form -> Bool
isAnd (And _) = True
isAnd _ = False

-- updr :: PrvGoal -> Ctx -> BT Ctx
-- updr pg@(f, g, k) c =
--   cast (appFaRL Same pg c) !>=
--     uncurry updr $
--     cast (appAx Lax pg c) <|>
--     ( do ((f', k'), c') <- cast $ appIffLO (f, k) c
--          updr (f', g, k') c' ) <|>
--     ( do ((f', k'), c') <- cast $ appIffLR (f, k) c
--          updr (f', g, k') c' )

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
  return $ IffR gl gr (impRAC gl gr po) (impRAC gr gl pr)

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

tt :: Text -> Maybe ()
tt t = trace (unpack t) (return ())

nt :: Maybe a
nt = Nothing 

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
  (vr, _) <- cast $ searchOrig (emptyVR, [(f, g)])
  -- pt $ "VR found = " <> ppVR vr <> "\n"
  uvm 0 (snd vr) f g

-- skolemize :: [Form] -> PrvGoal -> Ctx -> IO Ctx
-- skolemize hs pg c =
--   case ( appAx Pars pg c,
--          appNotLR pg c <|> appFaRL Perm pg c <|> appExLR Perm pg c,
--          appImpLR pg c <|> appIffRL pg c,
--          appOrRL pg c <|> appAndLR pg c ) of
--     (Just c', _, _, _) -> return c'
--     (_, Just (pg, c'), _, _) -> skolemize hs pg c'
--     (_, _, Just (pg0, pg1, c'), _) -> skolemize hs pg0 c' >>= skolemize hs pg1
--     (_, _, _, Just (pgs, c')) -> foldM (flip (skolemize hs)) c' pgs
--     _ -> first (skolemizeAux pg c) (pluck hs)
-- 
-- skolemizeAux :: PrvGoal -> Ctx -> (Form, [Form]) -> IO Ctx
-- skolemizeAux (f, g, k) c (h, hs) = do
--   let ((h', k'), c0) = tryAppFaL (h, k) c
--   ((f', m), (g', n), c1) <- cast $ appImpL (h', k') c0
--   c2 <- cast $ appAx ParFvs (f, f', m) c1
--   skolemize hs (g', g, n) c2

-- blank :: Ctx
-- blank = Ctx {fresh = 1, binding = HM.empty, proofs = HM.empty }

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

-- infer' :: Text -> [Form] -> Form -> IO ()
-- infer' "superposition" [f, g] h         = superpose f g h
-- infer' "forward_demodulation" [f, g] h  = superpose f g h
-- infer' "backward_demodulation" [f, g] h = superpose f g h
-- infer' _ _ _ = return ()

infer :: Text -> [Form] -> Form -> IO Prf
infer "superposition" [f, g] h         = superpose f g h
infer "forward_demodulation" [f, g] h  = superpose f g h
infer "backward_demodulation" [f, g] h = superpose f g h
infer "negated_conjecture" [f] g = guard (f == g) >> return (Ax f)
infer "factoring" [f] g = efactor (Just True) f g
infer "duplicate_literal_removal" [f] g = efactor (Just True) f g
infer "equality_resolution" [f] g = efactor (Just False) f g
infer "trivial_inequality_removal" [f] g = efactor nt f g
infer "subsumption_resolution" [f, g] h = resolve f g h
infer "resolution" [f, g] h = resolve f g h
infer "definition_folding" (f : fs) g = definFold fs f g
infer "definition_unfolding" (f : fs) g = dunfold fs f g
infer "pure_predicate_removal" [f] g = ppr f g
infer "flattening" [f] g = flat f g
infer "equality_factoring" [f] g = eqfactor f g
infer "rectify" [f] g            = pairSolve f g
infer "avatar_component_clause" [f] g = avatarComp f g

infer "cnf_transformation" [f] g = cnfTrans f g
infer "avatar_split_clause" (f : fs) g    = avatarSplit fs f g

infer "unused_predicate_definition_removal" [f] g = return Asm -- et "todo : updr" -- cast $ updr (f, g, 0) blank
infer "skolemisation" (f : fs) g = return Asm -- et "todo : skolemisation" -- skolemize fs (f, g, 0) blank
infer "avatar_contradiction_clause" [f] g = return Asm -- et "todo : avatar contra" -- avatarContra f g
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

mapIfMem :: VM -> Text -> Term
mapIfMem gm t =
  case HM.lookup t gm of
    Nothing -> Bv t
    Just x -> x

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

removeByPreds :: Set Text -> [Form] -> BT [Form]
removeByPreds rs [] = return []
removeByPreds rs (f : fs) = do
  fs' <- removeByPreds rs fs
  return (f : fs') <|> (guard (removeTarget rs f) >> return fs')

removeTarget :: Set Text -> Form -> Bool
removeTarget rs f = not $ S.disjoint rs $ formPreds f

flat :: Form -> Form -> IO Prf
flat f g = do
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
  -- pt "PPR step.\n"
  let rs = diffPreds f g
  -- pt $ "Removed predicates : " <> ppList id (S.toList rs) <> "\n"
  -- pt $ "f : " <> ppForm f <> "\n"
  -- pt $ "g : " <> ppForm g <> "\n"
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
pprf rs b (Ex vs f) =
  case pprf rs b f of
    And [] -> And []
    Or [] -> Or []
    Ex ws f' -> Ex (vs ++ ws) f'
    f' -> Ex vs f'
pprf rs b f@(Rel r _) =
  if S.member r rs
  then if b then And [] else Or []
  else f
pprf rs _ f@(Eq _ _) = f

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
  return $ IffR gl gr (impRAC gl gr po) (impRAC gr gl pr)

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

-- avatarContra :: Form -> Form -> IO Ctx
-- avatarContra f g = do
--   (gs, k, c) <- cast $ appConc (g, 0) blank
--   (_, gls, c') <- cast $ appPrem Nothing (f, k) c
--   cast $ litsMap Exact gls gs c'

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
  (_, ggs, pp) <- useConc 0 g
  -- (_, ggs) <- cast $ concLits 0 g
  gs <- cast $ deleteOnce (Not s) ggs
  fs <- cast $ premLits f
  vm <- mapSearch HM.empty fs gs
  pf <- usePrem (mapPremLit gs) vm f
  return $ pp $ NotR s $ Cut f (iffMP s f) pf
avatarCompCore _ _ = et "avatar-comp-core : not iff"

expandLit :: Int -> Form -> Form -> IO (Int, [Form], Prf -> Prf)
expandLit k (Not g) (Iff g' h) 
  | g == g' = return (k, [Not h], \ p_ -> Cut (Not h) p_ (notLR h g $ iffMP g h))
expandLit k g (Iff g' h) 
  | g == g' = do 
    (k', hs, ph) <- useConc k h
    return (k', hs, \ p_ -> Cut h (ph p_) (iffMPR g h) )
expandLit _ _ _ = mzero

expandLits :: [Form] -> Int -> [Form] -> IO (Int, [Form], Prf -> Prf)
expandLits ds k [] = return (k, [], id)
expandLits ds k (g : gs) = do
  (k', hs, pg) <- first (expandLit k g) ds
  (k'', hs', pgs) <- expandLits ds k' gs 
  return (k'', hs ++ hs', pg . pgs)
  
--   d <- pick ds
--   (f, c1) <- cast $ expandResult d g c0
--   (m, n0, c2) <- cast $ appCut nt (Not f, k) c1
--   ((_, _, n1), c3) <- cast $ appNotLR (Not f, Not g, n0) c2
--   ((gf, n2), c4) <- cast (appIffLO (d, n1) c3) <|> cast (appIffLR (d, n1) c3)
--   c5 <- cast $ appMP gf (g, f, n2) c4
--   return (Not f : hs, m, c5)
-- expandSplit ds (hs, k, c) g = do
--   d <- pick ds
--   (g', k', c') <- cast $ rwro d g k c <|> rwrr d g k c
--   (hs', k'', c'') <- cast $ appConc (g', k') c'
--   return (hs' ++ hs, k'', c'')

avatarSplit :: [Form] -> Form -> Form -> IO Prf
avatarSplit ds f g = do
  (k, gs, pg) <- useConc 0 g
  (_, hs, ph) <- expandLits ds k gs
  vm <- cast $ searchCnf (hs ++ gs) (HM.empty, [f])
  pf <- proveCnfTrans vm f (hs ++ gs)
  return $ pg $ ph pf

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

isGndTerm :: Term -> Bool
isGndTerm (Bv _) = False
isGndTerm (Fun f xs) = L.all isGndTerm xs
isGndTerm x = True

isGndAtom :: Form -> Bool
isGndAtom (Eq x y) = isGndTerm x && isGndTerm y
isGndAtom (Rel _ xs) = L.all isGndTerm xs
isGndAtom _ = False

isGndLit :: Form -> Bool
isGndLit (Not f) = isGndAtom f
isGndLit f = isGndAtom f

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
  vm <- aocf (L.map Bv vs) ws HM.empty  f g
  -- pt $ "VM =\n" <> ppVM vm 
  xs <- cast $ mapM (`HM.lookup` vm) ws
  return (xs, Fa vs (Imp (Ex ws f) (subf vm f)))
normalizeAOC (Imp (Ex ws f) g) = do
  vm <- aocf [] ws HM.empty f g
  xs <- cast $ mapM (`HM.lookup` vm) ws
  return (xs, Imp (Ex ws f) (subf vm f))
normalizeAOC _ = mzero


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
--   infer' r fs g
-- elab' _ _ = return ()

elab :: NSeq -> AnForm -> IO Elab
elab s (Af n h (Just (Gfun "file" [_, Gfun m []]))) = do
  f <- getHyp m s
  Plab <$> orig f h
elab _ (Af n g (Just (Gfun "introduced" [Gfun "predicate_definition_introduction" [],Glist [Gfun "new_symbols" [Gfun "naming" [],Glist [Gfun r []]]]]))) = relDef r g
elab _ (Af n g (Just (Gfun "introduced" [Gfun "avatar_definition" [], Glist [Gfun "new_symbols" [Gfun "naming" [], Glist [Gfun r []]]]]))) = relDef r g
elab s (Af n g (Just (Gfun "introduced" [Gfun "choice_axiom" [], Glist []]))) = do
  -- pt $ "AOC : " <> ppForm g <> "\n"
  (xs, f) <- normalizeAOC g
  -- pt $ "f = " <> ppForm f <> "\n"
  -- pt $ "g = " <> ppForm g <> "\n"
  p <- orig f g
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
  p <- infer r fs g
  return $ Plab p

elab _ (Af _ _ a) = error $ "Unimplemented case : " ++ show a

cnfTrans :: Form -> Form -> IO Prf
cnfTrans f g = do
  -- pt "CNF-Trans :\n"
  -- pt $ "f : " <> ppForm f <> "\n"
  -- pt $ "g : " <> ppForm g <> "\n"
  (_, gs, pp) <- useConc 0 g
  -- pt $ "gs : " <> ppList ppForm gs <> "\n"
  vm <- cast $ searchCnf gs (HM.empty, [f])
  -- pt $ "vm :\n" <> ppVM vm <> "\n"
  pf <- proveCnfTrans vm f gs
  return $ pp pf

useConcs :: Int -> [Form] -> IO (Int, [Form], Prf -> Prf)
useConcs k [] = return (k, [], id)
useConcs k (f : fs) = do
  (k', fs', pf) <- useConc k f
  (k'',fs'', pf') <- useConcs k' fs
  return (k'', fs' ++ fs'', pf . pf')

useConc :: Int -> Form -> IO (Int, [Form], Prf -> Prf)
useConc k (Fa vs f) = do
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  (k'', fs, pf) <- useConc k' f'
  return (k'', fs, FaR vs k f . pf)
useConc k (Or fs) = do
  (k', fs', pp) <- useConcs k fs
  return (k', fs', OrR fs fs . pp)
useConc k g =
  if isLit g
  then return (k, [g], id)
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

efactor :: Maybe Bool -> Form -> Form -> IO Prf
efactor mb f g = do
  -- pt "Eq-Factor :\n"
  -- pt $ "f : " <> ppForm f <> "\n"
  -- pt $ "g : " <> ppForm g <> "\n"
  fls <- cast $ premLits f
  (_, gls, pp) <- useConc 0 g
  -- (_, gls) <- cast $ concLits 0 g
  -- pt $ "f-lits : " <> ppList ppForm fls <> "\n"
  -- pt $ "g-lits : " <> ppList ppForm gls <> "\n"
  avs <- claVars f
  _vm <- efctr gls (HM.empty, mb, fls)
  let vm = L.foldl gndVar (HM.map gndt _vm) avs
  -- pt "Eq-Factor success, results :\n"
  -- pt $ "VM =\n" <> ppVM vm <> "\n"
  pf <- usePrem (mapPremLit gls) vm f
  return $ pp pf

type EFS = (VM, Maybe Bool, [Form])
type FSTA = (VM, [Form])

fContained :: [Form] -> (Term, Term, Dir, SST) -> Bool
fContained hs (_, y, _, _) =
  let hfs = fsfuns hs in
  let yfs = tfuns y in
  S.isSubsetOf yfs hfs

genSST :: Dir -> [Form] -> [Form] -> (Form, [Form]) -> [(Term, Term, Dir, SST)]
genSST dr hs gs (Eq x y, fs) = 
  let fs' = L.filter (\ l_ -> not (litOccursIn l_ hs)) fs in 
  let gs' = L.filter (\ l_ -> not (litOccursIn l_ hs)) gs in 
  [(x, y, dr, (HM.empty, fs', gs', [])), (y, x, dr, (HM.empty, fs', gs', []))]
genSST dr hs gs _ = []

genZs :: Dir -> [Form] -> [Form] -> [Form] -> [(Term, Term, Dir, SST)]
genZs dr fs gs hs =
  case unspecs fs hs of
    (_ : _ : _) -> []
    [ffs] -> L.filter (fContained hs) $ genSST dr hs gs ffs
    [] -> L.concatMap (genSST dr hs gs) (plucks fs)

lspec :: Form -> Form -> Bool
lspec (Not f) (Not g) = aspec f g
lspec f g = aspec f g

aspec :: Form -> Form -> Bool
aspec (Rel r xs) (Rel s ys) = r == s && isJust (zipM xs ys >>= specs HM.empty)
aspec (Eq x y) (Eq a b) = isJust $ specs HM.empty [(x, a), (y, b)] <|> specs HM.empty [(x, b), (y, a)]
aspec _ _ = False

unspecs :: [Form] -> [Form] -> [(Form, [Form])]
unspecs fls hls = L.filter (\ (f_, _) -> not (L.any (lspec f_) hls)) (plucks fls)

resolve :: Form -> Form -> Form -> IO Prf
resolve f g h = do
  ([f', g'], _) <- cast $ rnfs HM.empty HM.empty [f, g]
  fls <- cast $ premLits f'
  gls <- cast $ premLits g'
  -- (_, hls) <- cast $ concLits 0 h
  (_, hls, ph) <- useConc 0 h
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
      return $ Cut f' prff $ Cut g' prfg $ ph $ Cut pvt pf $ Cut (Not pvt) pg (NotL pvt $ Ax pvt)
    Rev -> do
      pf <- usePrem (mapPremLit (Not pvt : hls)) vm f'
      pg <- usePrem (mapPremLit (pvt : hls)) vm g'
      return $ Cut f' prff $ Cut g' prfg $ ph $ Cut pvt pg $ Cut (Not pvt) pf (NotL pvt $ Ax pvt)

eqfInit :: (Form, [Form]) -> [(Term, Term, [Form])]
eqfInit (Not (Eq x y), fs) = [(x, y, fs), (y, x, fs)]
eqfInit _ = []

eqfactor :: Form -> Form -> IO Prf
eqfactor f g = do
  fs <- cast $ premLits f
  -- (_, ggs) <- cast $ concLits 0 g
  (_, ggs, pg) <- useConc 0 g
  let xygs = L.concatMap eqfInit $ plucks ggs
  (vm, x, y, gs) <- first (\ (x_, y_, gs_) -> eqf x_ y_ gs_ (HM.empty, fs, [])) xygs

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
  (_, hls, ph) <- useConc 0 h
  -- pt $ "f-lits :\n" <> ppListNl ppForm fls <> "\n"
  -- pt $ "g-lits :\n" <> ppListNl ppForm gls <> "\n"
  -- pt $ "h-lits :\n" <> ppListNl ppForm hls <> "\n"
  let zos = genZs Obv fls gls hls
  let zrs = genZs Rev gls fls hls
  let zs = zos ++ zrs 
  -- pt "Initial Zs :\n"
  -- pt $ ppListNl ppZ zs 
  (x, y, dr, vm) <- first (\ (x_, y_, dr_, z_) -> super x_ y_ dr_ hls z_) zs
  -- pt "VM found:\n"
  -- pt $ ppVM vm
  prff <- prn 0 (f, f')
  prfg <- prn 0 (g, g')
  pfg <- case dr of
           Obv -> suprf vm x y f' g' hls
           Rev -> suprf vm x y g' f' hls
  return $ Cut f' prff $ Cut g' prfg $ ph pfg

ppZ :: (Term, Term, Dir, SST) -> Text
ppZ (x, y, _, (vm, fs, gs, xys)) = 
  "--------------------------------------------------\n" <>
  "vm = \n" <> ppVM vm <>
  "x = " <> ppTerm x <> "\n" <>
  "y = " <> ppTerm y <> "\n" <>
  "fs = " <> ppListNl ppForm fs <> "\n" <>
  "gs = " <> ppListNl ppForm gs <>
  "eqs = " <> ppListNl ppEq xys

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

proveCnfTrans :: VM -> Form -> [Form] -> IO Prf
proveCnfTrans vm (Fa vs f) gs = do
  let vxs = L.map (\ v_ -> (v_, agvmt vm (Bv v_))) vs
  let f' = substForm vxs f
  FaL vxs f <$> proveCnfTrans vm f' gs
proveCnfTrans vm (And fs) gs = 
  first (\ f_ -> AndL fs [f_] <$> proveCnfTrans vm f_ gs) fs
proveCnfTrans vm (Or fs) gs = do
  fps <- mapM (\ f_ -> (f_,) <$> proveCnfTrans vm f_ gs) fs 
  return $ OrL fps
proveCnfTrans _ (Not (Eq x y)) gs 
  | Not (Eq y x) `elem` gs = return $ notLR (x === y) (y === x) $ EqS y x 
proveCnfTrans _ (Eq x y) gs 
  | Eq y x `elem` gs = return $ EqS x y 
proveCnfTrans _ f gs 
  | f `elem` gs = return $ Ax f
  | otherwise = mzero 

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
mapPremLit _ f = et $ "map-prem-lit-exception : " <> ppForm f <> "\n"

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

litOccursIn :: Form -> [Form] -> Bool
litOccursIn (Not (Eq x y)) hs = Not (Eq x y) `elem` hs || Not (Eq y x) `elem` hs
litOccursIn (Eq x y) hs = Eq x y `elem` hs || Eq y x `elem` hs
litOccursIn f@(Not (Rel _ _)) hs = f `elem` hs
litOccursIn f@(Rel _ _) hs = f `elem` hs
litOccursIn _ _ = False

-- atomOccursIn :: Form -> [Form] -> Bool
-- atomOccursIn (Eq x y) hs = (x === y) `elem` hs || (y === x) `elem` hs
-- atomOccursIn f@(Rel _ _) hs = f `elem` hs
-- atomOccursIn _ _ = False

tryGndLit :: VM -> Form -> Maybe Form
tryGndLit vm f = do
  f' <- tavmf vm f
  guard $ isGndLit f' 
  return f'

superatv :: VM -> [Form] -> [Form] -> [(Term, Term)] -> (Form, [Form]) -> [SST]
superatv vm hs gs xys (f, fs) = 
  case tryGndLit vm f of 
    Just f' -> 
      if litOccursIn f' hs 
      then trace (unpack $ "trivlit found : " <> ppForm f') [(vm, fs, gs, xys)]
      else trace (unpack $ "trivlit not found : " <> ppForm f') []
    _ -> L.map (, fs, gs, xys) $ L.concatMap (ul vm f) hs

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

trivNeqLit :: [Form] -> Form -> Bool
trivNeqLit hs (Not (Eq x y)) = Not (Eq x y) `elem` hs || Not (Eq y x) `elem` hs 
trivNeqLit hs f@(Rel _ _) = f `elem` hs 
trivNeqLit hs f@(Not (Rel _ _)) = f `elem` hs 
trivNeqLit _ _ = False


-- forkSuper :: [Form] -> SST -> [[SST]]
-- forkSuper hs (Uninit fs gs) = 
--   case (matchRelWLOG fs hs, matchRelWLOG gs hs) of 
--     (Just fs', _) -> [[Uninit fs' gs]]
--     (_, Just gs') -> [[Uninit fs gs']]
--     _ -> _

-- forkSuper hs (vm, fs, gs, Just (dr, x, y, xys)) = _

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

search :: (Eq a, Show a) => (a -> Bool) -> (a -> [[a]]) -> a -> Maybe a
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

fixedTerm :: HM.Map Text Text -> Term -> Bool
fixedTerm vw (Bv v) = HM.member v vw
fixedTerm vw (Fun _ xs) = L.all (fixedTerm vw) xs
fixedTerm _ _ = True

fixedForm :: HM.Map Text Text -> Form -> Bool
fixedForm vw (Eq x y) = fixedTerm vw x && fixedTerm vw y
fixedForm vw (Rel _ xs) = L.all (fixedTerm vw) xs
fixedForm vw (Not f) = fixedForm vw f
fixedForm vw (Imp f g) = fixedForm vw f && fixedForm vw g
fixedForm vw (Iff f g) = fixedForm vw f && fixedForm vw g
fixedForm vw (Or fs) =  L.all (fixedForm vw) fs
fixedForm vw (And fs) =  L.all (fixedForm vw) fs
fixedForm vw (Fa vs f) =  L.all (`HM.member` vw) vs && fixedForm vw f
fixedForm vw (Ex vs f) =  L.all (`HM.member` vw) vs && fixedForm vw f

forkOrig :: VR -> ((Form, Form), [(Form, Form)]) -> [PSS]
forkOrig vr ((Rel r xs, Rel s ys), fgs)
  | r == s =
    case vmts vr xs ys of
      Just vr' -> [(vr', fgs)]
      _ -> []
forkOrig vr ((Eq x y, Eq a b), fgs) 
  | fixedTerm (fst $ snd vr) x || fixedTerm (fst $ snd vr) y = 
    let mb0 = vmts vr [x, y] [a, b] in
    let mb1 = vmts vr [x, y] [b, a] in
    case (mb0, mb1) of 
      (Just vr', _) -> [(vr', fgs)]
      (_, Just vr') -> [(vr', fgs)]
      _ -> []
  | otherwise =
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
  | otherwise = 
    case pluckFind (fixedForm $ fst (snd vr)) (f : fs) of 
      Just (f', fs') -> 
        case pluckFirst (\ g_ -> searchOrig (vr, [(f', g_)])) ggs of 
          Just ((vr', []), gs') -> [(vr', (Or fs', Or gs') : fgs)]
          _ -> []
      _ -> L.map (\ (g_, gs_) -> (vr, (f, g_) : (Or fs, Or gs_) : fgs)) (plucks ggs)
forkOrig vr ((And (f : fs), And ggs), fgs) 
  | L.any isAnd (f : fs) = [(vr, (And (flatAnd (f : fs)), And ggs) : fgs)] 
  | L.any isAnd ggs = [(vr, (And (f : fs), And (flatAnd ggs)) : fgs)] 
  | otherwise = 
    case pluckFind (fixedForm $ fst (snd vr)) (f : fs) of 
      Just (f', fs') -> 
        case pluckFirst (\ g_ -> searchOrig (vr, [(f', g_)])) ggs of 
          Just ((vr', []), gs') -> [(vr', (And fs', And gs') : fgs)]
          _ -> []
      _ -> L.map (\ (g_, gs_) -> (vr, (f, g_) : (And fs, And gs_) : fgs)) (plucks ggs)
forkOrig vr ((_, _),  _) = mzero

searchRect :: (VR, [(Form, Form)]) -> Maybe (VR, [(Form, Form)])
searchRect = search (L.null . snd) (\ (vr_, fgs_) -> L.map (forkRect vr_) (plucks fgs_))

searchOrig :: (VR, [(Form, Form)]) -> Maybe (VR, [(Form, Form)])
searchOrig = search (L.null . snd) (\ (vr_, fgs_) -> L.map (forkOrig vr_) (plucks fgs_))

searchCnf :: [Form] -> (VM, [Form]) -> Maybe VM
searchCnf gls z = fst <$> search (L.null . snd) (\ (vm_, fs_) -> L.map (forkCnf vm_ gls) (plucks fs_)) z

-- type SST = (VM, Dir, Term, Term, [Form] [Form], [(Term, Term)])

-- terminalSuper :: SST -> Bool
-- terminalSuper (_, _, _, _, [] [] []) = True
-- terminalSuper _ = False

-- searchSuper :: [Form] -> SST -> IO SST
-- searchSuper hs = 
--   search 
--     terminalSuper
--     -- (\ (vr_, eqn_, fs_, gs_) -> _)
--     (forkSuper hs)

forkCnf :: VM -> [Form] -> (Form, [Form]) -> [(VM, [Form])]
forkCnf vm gls (Or fs, hs) = [(vm, fs ++ hs)]
forkCnf vm gls (Fa _ f, hs) = [(vm, f : hs)]
forkCnf vm gls (And fs, hs) = L.map (\ f_ -> (vm, f_ : hs)) fs
forkCnf vm gs (f, hs) = L.map (, hs) $ L.concatMap (ul vm f) gs

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
  -- pt $ "g  = " <> ppForm g <> "\n"
  let nf = norm f
  -- pt $ "nf  = " <> ppForm nf <> "\n"
  ([_, nf'], _) <- cast $ rnfs HM.empty HM.empty [g, nf]
  -- pt $ "nf'  = " <> ppForm nf' <> "\n"
  (vr, _) <- cast $ searchRect (emptyVR, [(nf', g)])
  -- pt $ "VR  =\n" <> ppVR vr <> "\n"
  let rf = revarf (fst $ snd vr) nf'
  -- pt $ "rf  = " <> ppForm rf <> "\n"
  p0 <- pnm 0 f nf
  p1 <- prn 0 (nf, rf)
  p2 <- psol 0 rf g
  return $ 
    Cut nf (Cut (f <=> nf) p0 $ iffMP f nf) $ 
      Cut rf (Cut (nf <=> rf) p1 $ iffMP nf rf) $ 
        Cut (rf <=> g) p2 (iffMP rf g)

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

isAtom :: Form -> Bool
isAtom (Rel _ _) = True
isAtom (Eq _ _) = True
isAtom _ = False

isLit :: Form -> Bool
isLit (Not f) = isAtom f
isLit f = isAtom f

isCla :: Form -> Bool
isCla (Or fs) = L.all isLit fs
isCla _ = False

tryLast :: [a] -> Maybe a
tryLast [] = nt
tryLast [x] = Just x
tryLast (_ : xs) = tryLast xs


snipDotP :: String -> Maybe String
snipDotP [] = nt
snipDotP [_] = nt
snipDotP ".p" = return ""
snipDotP [_, _] = nt
snipDotP (c : cs) = (c :) <$> snipDotP cs

snipName :: String -> Maybe String
snipName [] = nt
snipName ('/' : cs) = 
  case snipName cs of 
    Just s -> Just s 
    _ -> snipDotP cs 
snipName (_ : cs) = 
  case snipName cs of 
    Just s -> Just s 
    _ -> nt

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
  name <- cast $ snipName tptp
  if tooHard name 
  then return ()
  else foldM_ elabIO hs tstp_afs
  Prelude.putStr "Elab complete.\n\n"

tooHard :: String -> Bool  
tooHard n = False -- n `elem` ["ALG038+1", "ALG016+1", "HAL004+1", "ALG114+1", "ALG111+1", "ALG121+1"]

  -- Prelude.putStr $ tstp ++ "\n"
  -- tstp_afs <- sortAfs <$> parseName tstp
  -- foldM_ elabIO' HM.empty tstp_afs
  -- Prelude.putStr "Elab complete.\n\n"