{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Basic where

import Types
import PP

import Data.Maybe (fromMaybe)
import Data.Text as T (Text, uncons, unsnoc, unpack, null)
import Data.List as L
import Data.Map as HM ( Map, insert, lookup, empty, map, member, mapMaybe, toList, fromListWithKey, delete, findWithDefault )
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList, union, unions )
import Control.Monad as M (MonadPlus, mzero, foldM, guard)
import Control.Monad.Fail as MF (MonadFail, fail)
-- import Control.Monad.Plus as MP 
import Control.Applicative as A
import Data.Functor ((<&>))
import qualified Data.Bifunctor as DBF
-- import Data.Hashable (Hashable)
import Data.Text.Read as TR ( decimal )
import Debug.Trace (trace)

pattern (:>) :: Char -> Text -> Text
pattern x :> xs <- (T.uncons -> Just (x, xs))

substBv :: [(Text, Term)] -> Text -> Term
substBv [] s = Var s
substBv ((t, x) : txs) s = if t == s then x else substBv txs s

substTerm :: [(Text, Term)] -> Term -> Term
-- substTerm txs (Par k) = Par k
substTerm txs (Var t) = substBv txs t
substTerm txs (Fun f xs) = Fun f $ L.map (substTerm txs) xs

substForm :: [(Text, Term)] -> Form -> Form
substForm vxs (Eq x y) = Eq (substTerm vxs x) (substTerm vxs y)
substForm vxs (Rel r xs) = Rel r $ L.map (substTerm vxs) xs
substForm vxs (Not f) = Not $ substForm vxs f
substForm vxs (And fs) = And $ L.map (substForm vxs) fs
substForm vxs (Or fs)  = Or  $ L.map (substForm vxs) fs
substForm vxs (Imp f g) = Imp (substForm vxs f) (substForm vxs g)
substForm vxs (Iff f g) = Iff (substForm vxs f) (substForm vxs g)
substForm vxs (Fa vs f) =
  let vxs' = L.filter (\ (v, _) -> v `notElem` vs) vxs in
  Fa vs $ substForm vxs' f
substForm vxs (Ex vs f) =
  let vxs' = L.filter (\ (v, _) -> v `notElem` vs) vxs in
  Ex vs $ substForm vxs' f

par :: Int -> Term
par k = Fun (ppSQ $ "#" <> ppInt k) []

varPars :: Int -> [Text] -> (Int, [(Text, Term)])
varPars k [] = (k, [])
varPars k (v : vs) =
  let (m, vxs) = varPars (k + 1) vs in
  (m, (v, par k) : vxs)

listPars :: Int -> [Text] -> (Int, [Term])
listPars k [] = (k, [])
listPars k (_ : vs) =
  let (m, xs) = listPars (k + 1) vs in
  (m, par k : xs)

zipM :: (Monad m, Alternative m) => [a] -> [b] -> m [(a, b)]
zipM [] [] = return []
zipM (x : xs) (y : ys) = zipM xs ys <&> ((x, y) :)
zipM xs ys = A.empty

zip3M :: (Monad m, Alternative m) => [a] -> [b] -> [c] -> m [(a, b, c)]
zip3M [] [] [] = return []
zip3M (x : xs) (y : ys) (z : zs) = zip3M xs ys zs <&> ((x, y, z) :)
zip3M _ _ _ = A.empty

lookupM :: (Eq a, Ord a, MonadFail m) => a -> Map a b -> m b
lookupM x h =
  case HM.lookup x h of
    Nothing -> MF.fail "lookup fail"
    Just y -> return y

nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 (x : _) = Just x
nth k (_ : xs) = nth (k - 1) xs

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

insertAll :: (Ord a) => [a] -> [a] -> [a]
insertAll [] ys = ys
insertAll (x : xs) ys = L.insert x $ insertAll xs ys

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f [] = []
mapFilter f (x : xs) =
  case f x of
    Nothing -> mapFilter f xs
    Just y -> y : mapFilter f xs

class MonadCast m where
  cast :: (MonadFail n) => m a -> n a

instance MonadCast Maybe where
  cast = \case
           Nothing -> MF.fail ""
           (Just x) -> return x

-- (?>) :: Maybe a -> (a -> b) -> b -> b
-- (?>) (Just x) f _ = f x
-- (?>) Nothing _ y = y

isPerm :: (Eq a) => [a] -> [a] -> Bool
isPerm xs ys = L.null (xs \\ ys) && L.null (ys \\ xs)

mark :: Int -> IO ()
mark k = print $ "Marking checkpoint " <> show k

pt :: Text -> IO ()
pt t = Prelude.putStr $ unpack t

ptnl :: Text -> IO ()
ptnl t = Prelude.putStr $ unpack $ t <> "\n"

et :: Text -> a
et t = error (unpack t)

deleteOnce :: (Eq a) => a -> [a] -> Maybe [a]
deleteOnce _ [] = Nothing
deleteOnce x (y : ys) =
  if x == y
  then return ys
  else (y :) <$> deleteOnce x ys

plucks :: [a] -> [(a, [a])]
plucks [] = []
plucks (x : xs) =
  let xxs = plucks xs in
  (x, xs) : L.map (DBF.second (x :)) xxs

pluckFind :: (a -> Bool) -> [a] -> Maybe (a, [a])
pluckFind f xs = L.find (f . fst) (plucks xs)

pluckFirst :: (MonadPlus m) => (a -> m b) -> [a] -> m (b, [a])
pluckFirst _ [] = mzero
pluckFirst f (x : xs) = (f x >>= \ y_ -> return (y_, xs)) <|> DBF.second (x :) <$> pluckFirst f xs

first :: (MonadFail m, Alternative m) => (a -> m b) -> [a] -> m b
first f = Prelude.foldr ((<|>) . f) (MF.fail "")

breakSingleton :: [a] -> Maybe a
breakSingleton [x] = Just x
breakSingleton _ = Nothing

zt :: Term
zt = Fun "c" []

isNeg :: Form -> Bool
isNeg (Not _) = True
isNeg _ = False

isPos :: Form -> Bool
isPos = not . isNeg

isGndTerm :: Term -> Bool
isGndTerm (Var _) = False
isGndTerm (Fun f xs) = L.all isGndTerm xs
-- isGndTerm x = True

isGndAtom :: Form -> Bool
isGndAtom (Eq x y) = isGndTerm x && isGndTerm y
isGndAtom (Rel _ xs) = L.all isGndTerm xs
isGndAtom _ = False

isGndLit :: Form -> Bool
isGndLit (Not f) = isGndAtom f
isGndLit f = isGndAtom f

mapM2 :: (Monad m, Alternative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 f xs ys = zipM xs ys >>= mapM (uncurry f)

foldM2 :: (Monad m, Alternative m) => (a -> b -> c -> m a) -> a -> [b] -> [c] -> m a
foldM2 f x ys zs = do
  yzs <- zipM ys zs
  foldM (\ x_ (y_, z_) -> f x_ y_ z_) x yzs

top :: Form
top = And []

bot :: Form
bot = Or []

isTop :: Form -> Bool
isTop = (top ==)

isBot :: Form -> Bool
isBot = (bot ==)

varInt :: Text -> Term -> Bool
varInt v (Var w) = v == w
varInt v (Fun f xs) = L.any (varInt v) xs
-- varInt v _ = False

varInf :: Text -> Form -> Bool
varInf v (Eq x y) = varInt v x || varInt v y
varInf v (Rel _ xs) = L.any (varInt v) xs
varInf v (Not f) = varInf v f
varInf v (And fs) = L.any (varInf v) fs
varInf v (Or fs)  = L.any (varInf v) fs
varInf v (Imp f g) = varInf v f || varInf v g
varInf v (Iff f g) = varInf v f || varInf v g
varInf v (Fa ws f) = v `notElem` ws && varInf v f
varInf v (Ex ws f) = v `notElem` ws && varInf v f

-- Like StateT but with return tuple swapped
newtype StateM s m a = StateM { runStateM :: s -> m (s, a) }

instance Functor m => Functor (StateM s m) where
    fmap f (StateM x) = StateM $ \s -> fmap (DBF.second f) (x s)

instance Monad m => Applicative (StateM s m) where
    pure x = StateM $ \s -> return (s, x)
    StateM f <*> StateM x = StateM $ \s -> do (s', f') <- f s
                                              (s'', x') <- x s'
                                              return (s'', f' x')

-- | Monadic variant of 'mapAccumL'.
mapAccumM :: (Monad m, Traversable t) => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumM f s t = runStateM (traverse (\x -> StateM (`f` x)) t) s


isOr :: Form -> Bool
isOr (Or _) = True
isOr _ = False

isAnd :: Form -> Bool
isAnd (And _) = True
isAnd _ = False

nt :: Maybe a
nt = Nothing

breakVar :: Term -> Maybe Text
breakVar (Var v) = Just v
breakVar _ = Nothing

isConstant :: Term -> Bool
isConstant (Fun _ []) = True
isConstant _ = False

breakFa :: Form -> Maybe ([Text], Form)
breakFa (Fa vs f) = return (vs, f)
breakFa _ = mzero

breakEx :: Form -> Maybe ([Text], Form)
breakEx (Ex vs f) = return (vs, f)
breakEx _ = mzero

isAtom :: Form -> Bool
isAtom (Rel _ _) = True
isAtom (Eq _ _) = True
isAtom _ = False

isLit :: Form -> Bool
isLit (Not f) = isAtom f
isLit f = isAtom f

ru :: (Monad m) => m ()
ru = return ()

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

cuts :: [(Form, Prf)] -> Prf -> Prf
cuts [] = id
cuts ((f, p) : fps) = Cut f p . cuts fps

guardMsg :: (Alternative m, Monad m) => Text -> Bool -> m ()
guardMsg _ True  = return ()
guardMsg s False = error (unpack s)

claVars :: Form -> IO (Set Text)
claVars (Fa vs f) = S.union (S.fromList vs) <$> claVars f
claVars (Or fs) = S.unions <$> mapM claVars fs
claVars f = guard (isLit f) >> return S.empty

formVars :: Form -> Set Text
formVars (Fa vs f) = S.union (S.fromList vs) $ formVars f
formVars (Ex vs f) = S.union (S.fromList vs) $ formVars f
formVars (Imp f g) = S.union (formVars f) (formVars g)
formVars (Iff f g) = S.union (formVars f) (formVars g)
formVars (Not f) = formVars f
formVars (Or fs) = S.unions $ L.map formVars fs
formVars (And fs) = S.unions $ L.map formVars fs
formVars _ = S.empty

varsInt :: [Text] -> Term -> Bool
varsInt vs (Var v) = v `elem` vs
varsInt vs (Fun f xs) = L.any (varsInt vs) xs
-- varsInt vs _ = False

varsInf :: [Text] -> Form -> Bool
varsInf vs (Eq x y) = varsInt vs x || varsInt vs y
varsInf vs (Rel _ xs) = L.any (varsInt vs) xs
varsInf vs (Not f) = varsInf vs f
varsInf vs (And fs) = L.any (varsInf vs) fs
varsInf vs (Or fs)  = L.any (varsInf vs) fs
varsInf vs (Imp f g) = varsInf vs f || varsInf vs g
varsInf vs (Iff f g) = varsInf vs f || varsInf vs g
varsInf vs (Fa ws f) = varsInf (vs L.\\ ws) f
varsInf vs (Ex ws f) = varsInf (vs L.\\ ws) f

-- isJust :: Maybe a -> Bool
-- isJust (Just _) = True
-- isJust _ = False

gndTerm :: Term -> Term
gndTerm (Var _) = zt
gndTerm (Fun f xs) = Fun f $ L.map gndTerm xs
-- gndTerm x = x

agvmt :: VM -> Term -> Term
agvmt gm (Var v) =
  case HM.lookup v gm of
    Just x -> gndTerm x
    _ -> zt
agvmt gm (Fun f xs) = Fun f $ L.map (agvmt gm) xs
-- agvmt _ x = x

avmt :: VM -> Term -> Term
avmt gm (Var v) =
  case HM.lookup v gm of
    Just x -> x
    _ -> Var v
avmt gm (Fun f xs) = Fun f $ L.map (avmt gm) xs

tavmt :: VM -> Term -> Maybe Term
tavmt gm (Var v) =
  case HM.lookup v gm of
    Just x -> return x
    _ -> mzero -- return $ Var v
tavmt gm (Fun f xs) = Fun f <$> mapM (tavmt gm) xs

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

getShortList :: [[a]] -> Maybe [a]
getShortList [] = nt
getShortList [xs] = return xs
getShortList ([] : xss) = return []
getShortList ([x] : xss) = return [x]
getShortList (xs : xss) = do
  ys <- getShortList xss
  if shorter xs ys
  then return xs
  else return ys

shorter :: [a] -> [b] -> Bool
shorter _ [] = False
shorter [] _ = True
shorter (_ : xs) (_ : ys) = shorter xs ys

substVar :: Text -> Term -> Term -> Term
substVar v x (Var w) = if v == w then x else Var w
-- substVar v x (Par m) = Par m
substVar v x (Fun f xs) = Fun f $ L.map (substVar v x) xs

hasVar :: Text -> Term -> Bool
hasVar v (Var w) = v == w
hasVar v (Fun _ xs) = L.any (hasVar v) xs

appVrTerm :: VR -> Term -> Term
appVrTerm vr (Var v) =
  case HM.lookup v (fst vr) of
    Just x -> Var x
    _ -> et "appVrTerm : no mapping"
appVrTerm vw (Fun f xs) = Fun f $ L.map (appVrTerm vw) xs

appVrForm :: VR -> Form -> Form
appVrForm vr (Not f) = Not $ appVrForm vr f
appVrForm vr (Fa vs f) = Fa (L.map (\ v_ -> HM.findWithDefault "_" v_ (fst vr)) vs) $ appVrForm vr f
appVrForm vr (Ex vs f) = Ex (L.map (\ v_ -> HM.findWithDefault "_" v_ (fst vr)) vs) $ appVrForm vr f
appVrForm vr (Imp f g) = Imp (appVrForm vr f) (appVrForm vr g)
appVrForm vr (Iff f g) = Iff (appVrForm vr f) (appVrForm vr g)
appVrForm vr (Or fs) = Or $ L.map (appVrForm vr) fs
appVrForm vr (And fs) = And $ L.map (appVrForm vr) fs
appVrForm vr (Rel r xs) = Rel r $ L.map (appVrTerm vr) xs
appVrForm vr (Eq x y) = Eq (appVrTerm vr x) (appVrTerm vr y)

litOccursIn :: Form -> [Form] -> Bool
litOccursIn (Not (Eq x y)) hs = Not (Eq x y) `elem` hs || Not (Eq y x) `elem` hs
litOccursIn (Eq x y) hs = Eq x y `elem` hs || Eq y x `elem` hs
litOccursIn f@(Not (Rel _ _)) hs = f `elem` hs
litOccursIn f@(Rel _ _) hs = f `elem` hs
litOccursIn _ _ = False

sublist :: Eq a => [a] -> [a] -> Bool
sublist [] _ = True
sublist (x : xs) ys = elem x ys && sublist xs ys

desnoc :: [a] -> Maybe ([a], a)
desnoc [] = nt
desnoc [x] = return ([], x)
desnoc (x : xs) = DBF.first (x :) <$> desnoc xs

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

readInt :: Text -> Maybe Int
readInt t = 
  case TR.decimal t of 
    Left _ -> nt -- et $ "cannot read int : " <> t
    Right (k, t') -> do 
      guard $ T.null t' 
      return k

unquote :: Text -> Maybe Text
unquote t = do 
  ('\'', t') <- T.uncons t
  (t'', '\'') <- T.unsnoc t'
  return t''

pairWithVR' :: VR -> Text -> IO (Text, Term)
pairWithVR' (vw, _) v = 
  case HM.lookup v vw of 
    Just w -> return (v, Var w)
    _ -> mzero

pairWithVR :: VR -> [(Text, Term)] -> Text -> Term
pairWithVR (vw, _) wxs v =
  fromMaybe zt ( do w <- HM.lookup v vw
                    snd <$> L.find ((w ==) . fst) wxs )

formSJ :: Form -> Bool
formSJ (Or [_])  = True
formSJ (And [_]) = True
formSJ (Not f) = formSJ f
formSJ (Imp f g) = formSJ f || formSJ g
formSJ (Iff f g) = formSJ f || formSJ g
formSJ (Or fs) = L.any formSJ fs
formSJ (And fs) = L.any formSJ fs
formSJ (Fa _ f) = formSJ f
formSJ (Ex _ f) = formSJ f
formSJ _ = False

elabSingleJunct :: EF -> Bool
elabSingleJunct (f, _, _, _, _, _) = formSJ f

{- write -}
