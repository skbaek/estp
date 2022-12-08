{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Basic where

import Types

import Data.Word (Word8)
import Data.Char.Decode (decodeByte, CodePage437 (CodePage437), encodeWithDefault)
import qualified Data.ByteString as BS 
import Data.ByteString.Builder (Builder, toLazyByteString) 
import Data.List as L
import Data.Map as HM 
  ( Map, insert, lookup, empty, map, member, mapMaybe, toList, 
    fromListWithKey, delete, findWithDefault, singleton )
import Data.Set as S 
  ( empty, insert, member, singleton, toList, Set, fromList, union, unions, size )
import Control.Monad as M (MonadPlus, mzero, foldM, guard)
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative as A
import Data.Functor ((<&>))
import qualified Data.Bifunctor as DBF
import Debug.Trace (trace)

w2c :: Word8 -> Char
w2c = decodeByte CodePage437

c2w :: Char -> Word8 
c2w = encodeWithDefault CodePage437 (error "Not encodable") 

bsrec :: BS -> Maybe (Char, BS)
bsrec bs = DBF.first (decodeByte CodePage437) <$> BS.uncons bs 

ps :: String -> IO ()
ps = Prelude.putStr

pattern (:>) :: Char -> BS -> BS
pattern x :> xs <- (bsrec -> Just (x, xs))

substBv :: [(BS, Term)] -> BS -> Term
substBv [] s = Var s
substBv ((t, x) : txs) s = if t == s then x else substBv txs s

substTerm :: [(BS, Term)] -> Term -> Term
-- substTerm txs (Par k) = Par k
substTerm txs (Var t) = substBv txs t
substTerm txs (Fun f xs) = Fun f $ L.map (substTerm txs) xs

substForm :: [(BS, Term)] -> Form -> Form
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
par k = Fun (Idx k) [] 

varPars :: Int -> [BS] -> (Int, [(BS, Term)])
varPars k [] = (k, [])
varPars k (v : vs) =
  let (m, vxs) = varPars (k + 1) vs in
  (m, (v, par k) : vxs)

listPars :: Int -> [BS] -> (Int, [Term])
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

substitute :: (Monad m, Alternative m) => [BS] -> [Term] -> Form -> m Form
substitute vs xs f = do
  vxs <- zipM vs xs
  return $ substForm vxs f

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

bs2int :: BS -> Maybe Int
bs2int ('-' :> t) = textToNat t <&> negate
bs2int t = textToNat t

textToNat :: BS -> Maybe Int
textToNat (c :> t) = do
  k <- charToInt c
  textToNatCore k t
textToNat _ = Nothing

textToNatCore :: Int -> BS -> Maybe Int
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

isPerm :: (Eq a) => [a] -> [a] -> Bool
isPerm xs ys = L.null (xs \\ ys) && L.null (ys \\ xs)

mark :: Int -> IO ()
mark k = print $ "Marking checkpoint " <> show k

pbs :: BS -> IO ()
pbs t = Prelude.putStr $ show t

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
zt = Fun (Reg "c") []

isPos :: Form -> Bool
isPos = not . isNeg

isNeg :: Form -> Bool
isNeg (Not _) = True
isNeg _ = False

isbt :: Form -> Bool
isbt = not . isNeg

isGndTerm :: [BS] -> Term -> Bool
isGndTerm vs (Var v) = v `elem` vs
isGndTerm vs (Fun f xs) = L.all (isGndTerm vs) xs

isGndAtom :: Form -> Bool
isGndAtom (Eq x y) = isGndTerm [] x && isGndTerm [] y
isGndAtom (Rel _ xs) = L.all (isGndTerm []) xs
isGndAtom _ = False

isGndLit :: Form -> Bool
isGndLit (Not f) = isGndAtom f
isGndLit f = isGndAtom f

isGndForm :: [BS] -> Form -> Bool
isGndForm vs (Rel _ xs) = L.all (isGndTerm vs) xs
isGndForm vs (Eq x y) = isGndTerm vs x && isGndTerm vs y
isGndForm vs (Not f) = isGndForm vs f
isGndForm vs (Or fs) = L.all (isGndForm vs) fs
isGndForm vs (And fs) = L.all (isGndForm vs) fs
isGndForm vs (Imp f g) = isGndForm vs f && isGndForm vs g
isGndForm vs (Iff f g) = isGndForm vs f && isGndForm vs g
isGndForm vs (Fa ws f) = isGndForm (vs ++ ws) f 
isGndForm vs (Ex ws f) = isGndForm (vs ++ ws) f 

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

varInt :: BS -> Term -> Bool
varInt v (Var w) = v == w
varInt v (Fun f xs) = L.any (varInt v) xs

varInf :: BS -> Form -> Bool
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

breakVar :: Term -> Maybe BS
breakVar (Var v) = Just v
breakVar _ = Nothing

isConstant :: Term -> Bool
isConstant (Fun _ []) = True
isConstant _ = False

breakFa :: Form -> Maybe ([BS], Form)
breakFa (Fa vs f) = return (vs, f)
breakFa _ = mzero

breakEx :: Form -> Maybe ([BS], Form)
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

tfuns :: Term -> Set Funct
tfuns (Fun f xs) = S.insert f $ S.unions $ L.map tfuns xs
tfuns _ = S.empty

fsfuns :: [Form] -> Set Funct
fsfuns = S.unions . L.map ffuns

ffuns :: Form -> Set Funct
ffuns (Rel _ xs) = S.unions $ L.map tfuns xs
ffuns (Or fs) = S.unions $ L.map ffuns fs
ffuns (And fs) = S.unions $ L.map ffuns fs
ffuns (Eq x y) = S.unions $ L.map tfuns [x, y]
ffuns (Imp f g) = S.unions $ L.map ffuns [f, g]
ffuns (Iff f g) = S.unions $ L.map ffuns [f, g]
ffuns (Not f) = ffuns f
ffuns (Fa _ f) = ffuns f
ffuns (Ex _ f) = ffuns f

guardMsg :: (Alternative m, Monad m) => String -> Bool -> m ()
guardMsg _ True  = return ()
guardMsg s False = error s

claVars :: Form -> IO (Set BS)
claVars (Fa vs f) = S.union (S.fromList vs) <$> claVars f
claVars (Or fs) = S.unions <$> mapM claVars fs
claVars f = guard (isLit f) >> return S.empty

formVars :: Form -> Set BS
formVars (Fa vs f) = S.union (S.fromList vs) $ formVars f
formVars (Ex vs f) = S.union (S.fromList vs) $ formVars f
formVars (Imp f g) = S.union (formVars f) (formVars g)
formVars (Iff f g) = S.union (formVars f) (formVars g)
formVars (Not f) = formVars f
formVars (Or fs) = S.unions $ L.map formVars fs
formVars (And fs) = S.unions $ L.map formVars fs
formVars _ = S.empty

varsInt :: [BS] -> Term -> Bool
varsInt vs (Var v) = v `elem` vs
varsInt vs (Fun f xs) = L.any (varsInt vs) xs
-- varsInt vs _ = False

varsInf :: [BS] -> Form -> Bool
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

substVar :: BS -> Term -> Term -> Term
substVar v x (Var w) = if v == w then x else Var w
-- substVar v x (Par m) = Par m
substVar v x (Fun f xs) = Fun f $ L.map (substVar v x) xs

hasVar :: BS -> Term -> Bool
hasVar v (Var w) = v == w
hasVar v (Fun _ xs) = L.any (hasVar v) xs

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

formPreds :: Form -> Set Funct
formPreds (Eq _ _) = S.empty
formPreds (Rel r _) = S.singleton r
formPreds (Not f) = formPreds f
formPreds (Or fs) = L.foldl S.union S.empty $ L.map formPreds fs
formPreds (And fs) = L.foldl S.union S.empty $ L.map formPreds fs
formPreds (Imp f g) = S.union (formPreds f) (formPreds g)
formPreds (Iff f g) = S.union (formPreds f) (formPreds g)
formPreds (Fa _ f) = formPreds f
formPreds (Ex _ f) = formPreds f

bs2str :: BS -> String 
bs2str = L.map w2c . BS.unpack

str2bs :: String -> BS
str2bs = BS.pack . L.map c2w

unquote :: BS -> Maybe BS
unquote ('\'' :> bs) = do 
  (bs', '\'') <- DBF.second w2c <$> BS.unsnoc bs
  return bs'
unquote _ = Nothing

isSkolemTerm :: [BS] -> Term -> Bool
isSkolemTerm vs (Fun _ xs) =
  case mapM breakVar xs of
    Just ws -> isPerm vs ws -- sublist vs ws && sublist ws vs
    _ -> False
isSkolemTerm _ _ = False

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

ppSQ :: Builder -> Builder
ppSQ t = "'" <> t <> "'"

ppHeadHex :: Int -> Builder
ppHeadHex 0 = "g"
ppHeadHex 2 = "h"
ppHeadHex 1 = "i"
ppHeadHex 3 = "j"
ppHeadHex 4 = "k"
ppHeadHex 5 = "l"
ppHeadHex 6 = "m"
ppHeadHex 7 = "n"
ppHeadHex 8 = "o"
ppHeadHex 9 = "p"
ppHeadHex 10 = "q"
ppHeadHex 11 = "r"
ppHeadHex 12 = "s"
ppHeadHex 13 = "t"
ppHeadHex 14 = "u"
ppHeadHex 15 = "v"
ppHeadHex _ = error "out of head hex bounds"

ppTailHex :: Int -> Builder
ppTailHex 0 = "0"
ppTailHex 2 = "1"
ppTailHex 1 = "2"
ppTailHex 3 = "3"
ppTailHex 4 = "4"
ppTailHex 5 = "5"
ppTailHex 6 = "6"
ppTailHex 7 = "7"
ppTailHex 8 = "8"
ppTailHex 9 = "9"
ppTailHex 10 = "a"
ppTailHex 11 = "b"
ppTailHex 12 = "c"
ppTailHex 13 = "d"
ppTailHex 14 = "e"
ppTailHex 15 = "f"
ppTailHex _ = error "out of tail hex bounds"

ppMarkHex :: Int -> Builder
ppMarkHex k = 
  if k < 16 
    then ppHeadHex k 
    else ppMarkHex (k `div` 16) <> ppTailHex (k `rem` 16)

ppNat :: Int -> Builder
ppNat 0 = "0"
ppNat 2 = "2"
ppNat 1 = "1"
ppNat 3 = "3"
ppNat 4 = "4"
ppNat 5 = "5"
ppNat 6 = "6"
ppNat 7 = "7"
ppNat 8 = "8"
ppNat 9 = "9"
ppNat k = ppNat (k `div` 10) <> ppNat (k `rem` 10)

ppInt :: Int -> Builder
ppInt k = if k < 0 then "-" <> ppNat (- k) else ppNat k

skip :: (Monad m) => m ()
skip = return ()

complementary :: SignForm -> SignForm -> Bool
complementary (True, f) (False, g) = f == g 
complementary _ _ = False

proofRootNode :: Proof -> Node
proofRootNode (Id_ ni nt nf) = ni
proofRootNode (Cut_ ni _ p q) = ni
proofRootNode (FunC_ ni xs nm) = ni
proofRootNode (RelC_ ni xs nt nf) = ni
proofRootNode (EqR_ ni nm) = ni
proofRootNode (EqS_ ni nt nf) = ni
proofRootNode (EqT_ ni nxy nyz nxz) = ni
proofRootNode (NotT_ ni nm p) = ni
proofRootNode (NotF_ ni nm p) = ni
proofRootNode (OrT_ ni nm ps) = ni
proofRootNode (OrF_ ni nm k p) = ni
proofRootNode (AndT_ ni nm k p) = ni
proofRootNode (AndF_ ni nm ps) = ni
proofRootNode (ImpT_ ni nm p q) = ni
proofRootNode (ImpFA_ ni nm p) = ni
proofRootNode (ImpFC_ ni nm p) = ni
proofRootNode (IffTO_ ni nm p) = ni
proofRootNode (IffTR_ ni nm p) = ni
proofRootNode (IffF_ ni nm p q) = ni
proofRootNode (FaT_ ni nm xs p) = ni
proofRootNode (FaF_ ni nm k p) = ni
proofRootNode (ExT_ ni nm k p) = ni
proofRootNode (ExF_ ni nm xs p) = ni
proofRootNode (RelD_ ni _ p) = ni
proofRootNode (AoC_ ni _ _ p) = ni
proofRootNode (Open_ ni) = ni

proofRN :: Proof -> BS
proofRN p = 
  case proofRootNode p of 
    (ni, _, _) -> ni 

proofRSF :: Proof -> (Bool, Form)
proofRSF p = 
  case proofRootNode p of 
    (_, b, f) -> (b, f)

functIdxLT :: Int -> Funct -> Bool
functIdxLT k (Reg _) = True
functIdxLT k (Idx m) = m < k

termIdxLT :: Int -> Term -> Bool
termIdxLT _ (Var _) = True
termIdxLT k (Fun f xs) = functIdxLT k f && L.all (termIdxLT k) xs 

formIdxLT :: Int -> Form -> Bool
formIdxLT k (Eq x y) = termIdxLT k x && termIdxLT k y
formIdxLT k (Rel r xs) = functIdxLT k r && L.all (termIdxLT k) xs 
formIdxLT k (Not f) = formIdxLT k f
formIdxLT k (Or fs) = L.all (formIdxLT k) fs
formIdxLT k (And fs) = L.all (formIdxLT k) fs
formIdxLT k (Imp f g) = formIdxLT k f && formIdxLT k g
formIdxLT k (Iff f g) = formIdxLT k f && formIdxLT k g
formIdxLT k (Fa _ f) = formIdxLT k f 
formIdxLT k (Ex _ f) = formIdxLT k f 

checkRelD :: Int -> Form -> Maybe Int
checkRelD k (Fa vs (Iff (Rel (Idx m) xs) f)) = do 
  guard $ k <= m
  ws <- cast $ mapM breakVar xs
  guard $ sublist ws vs
  guard $ isGndForm ws f
  guard $ formIdxLT k f
  return $ m + 1
checkRelD k (Iff (Rel (Idx m) []) f) = do 
  guard $ k <= m
  guard $ isGndForm [] f
  guard $ formIdxLT k f
  return $ m + 1
checkRelD _ _ = mzero

distintList :: (Ord a) => [a] -> Bool
distintList xs = S.size (S.fromList xs) == L.length xs

checkSkolemTerm :: [BS] -> Int -> Term -> Maybe Int
checkSkolemTerm vs k (Var _) = mzero
checkSkolemTerm vs k (Fun (Reg _) _) = mzero
checkSkolemTerm vs k (Fun (Idx m) xs) = do
  guard $ k <= m
  ws <- cast $ mapM breakVar xs
  guard $ sublist ws vs
  return $ m + 1

checkAoC :: Int -> Term -> Form -> Maybe Int
checkAoC k x (Fa vs (Imp (Ex [w] f) g)) = do
  guard $ distintList (w : vs)
  k' <- checkSkolemTerm vs k x 
  guard $ substForm [(w, x)] f == g
  return k'
checkAoC k x (Fa vs (Imp (Ex (w : ws) f) g)) = do
  guard $ distintList (w : ws ++ vs)
  k' <- checkSkolemTerm vs k x 
  guard $ substForm [(w, x)] (Ex ws f) == g
  return k'
checkAoC k x (Imp (Ex [w] f) g) = do
  k' <- checkSkolemTerm [] k x
  guard $ substForm [(w, x)] f == g
  return k'
checkAoC k x (Imp (Ex (w : ws) f) g) = do
  guard $ distintList (w : ws)
  k' <- checkSkolemTerm [] k x
  guard $ substForm [(w, x)] (Ex ws f) == g
  return k'
checkAoC k xs _ = mzero

breakTrueEq :: (Bool, Form) -> Maybe (Term, Term)
breakTrueEq (True, Eq x y) = Just (x, y)
breakTrueEq _ = mzero

rootNode :: Proof -> Node 
rootNode (Id_ ni _ _) = ni
rootNode (Cut_ ni _ _ _) = ni
rootNode (FunC_ ni _ _) = ni
rootNode (RelC_ ni _ _ _) = ni
rootNode (EqR_ ni _) = ni
rootNode (EqS_ ni _ _) = ni
rootNode (EqT_ ni _ _ _) = ni
rootNode (NotT_ ni _ _) = ni
rootNode (NotF_ ni _ _) = ni
rootNode (OrT_ ni _ _) = ni
rootNode (OrF_ ni _ _ _) = ni
rootNode (AndT_ ni _ _ _) = ni
rootNode (AndF_ ni _ _) = ni
rootNode (ImpT_ ni _ _ _) = ni
rootNode (ImpFA_ ni _ _) = ni
rootNode (ImpFC_ ni _ _) = ni
rootNode (IffTO_ ni _ _) = ni
rootNode (IffTR_ ni _ _) = ni
rootNode (IffF_ ni _ _ _) = ni
rootNode (FaT_ ni _ _ _) = ni
rootNode (FaF_ ni _ _ _) = ni
rootNode (ExT_ ni _ _ _) = ni
rootNode (ExF_ ni _ _ _) = ni
rootNode (RelD_ ni _ _) = ni
rootNode (AoC_ ni _ _ _) = ni
rootNode (Open_ ni) = ni

conjecturize :: BS -> Form -> Form
conjecturize "conjecture" f = Not f
conjecturize _ f = f

univClose :: Form -> Form
univClose f =
  case formFreeVars f of
    [] -> f
    vs -> Fa vs f

mergeVars :: [BS] -> [BS] -> [BS]
mergeVars vs ws = vs ++ (ws \\ vs)

termVars :: Term -> [BS]
termVars (Var v) = [v]
termVars (Fun _ ts) = foldl mergeVars [] (L.map termVars ts)

formFreeVars :: Form -> [BS]
formFreeVars (Rel _ ts) = foldl mergeVars [] (L.map termVars ts)
formFreeVars (Eq t s) = mergeVars (termVars t) (termVars s)
formFreeVars (Not f) = formFreeVars f
formFreeVars (And fs) = foldl mergeVars [] (L.map formFreeVars fs)
formFreeVars (Or  fs) = foldl mergeVars [] (L.map formFreeVars fs)
formFreeVars (Imp f g) = mergeVars (formFreeVars f) (formFreeVars g)
formFreeVars (Iff f g) = mergeVars (formFreeVars f) (formFreeVars g)
formFreeVars (Fa vs f) = formFreeVars f \\ vs
formFreeVars (Ex vs f) = formFreeVars f \\ vs

assemble' :: Sol -> Node -> Inf -> IO Proof
assemble' mp ni (Id nt nf) = return $ Id_ ni nt nf
assemble' mp ni (FunC nms nm) = return $ FunC_ ni nms nm
assemble' mp ni (RelC nms nt nf) = return $ RelC_ ni nms nt nf
assemble' mp ni (EqR nm) = return $ EqR_ ni nm
assemble' mp ni (EqS nt nf) = return $ EqS_ ni nt nf
assemble' mp ni (EqT nxy nyz nxz) = return $ EqT_ ni nxy nyz nxz
assemble' mp ni (Cut f nf nt) = do
  pf <- assemble mp nf
  pt <- assemble mp nt
  return $ Cut_ ni f pf pt
assemble' mp ni (NotT nh nc) = NotT_ ni nh <$> assemble mp nc
assemble' mp ni (NotF nh nc) = NotF_ ni nh <$> assemble mp nc
assemble' mp ni (OrT nh ncs) = do
  ps <- mapM (assemble mp) ncs
  return $ OrT_ ni nh ps
assemble' mp ni (OrF nh k nc) = OrF_ ni nh k <$> assemble mp nc
assemble' mp ni (AndT nh k nc) = AndT_ ni nh k <$> assemble mp nc
assemble' mp ni (AndF nh ncs) = do
  ps <- mapM (assemble mp) ncs
  return $ AndF_ ni nh ps
assemble' mp ni (ImpT nh na nc) = do
  pa <- assemble mp na
  pc <- assemble mp nc
  return $ ImpT_ ni nh pa pc
assemble' mp ni (ImpFA nh nc) = ImpFA_ ni nh <$> assemble mp nc
assemble' mp ni (ImpFC nh nc) = ImpFC_ ni nh <$> assemble mp nc
assemble' mp ni (IffTO nh nc) = IffTO_ ni nh <$> assemble mp nc
assemble' mp ni (IffTR nh nc) = IffTR_ ni nh <$> assemble mp nc
assemble' mp ni (IffF nh no nr) = do
  po <- assemble mp no
  pr <- assemble mp nr
  return $ IffF_ ni nh po pr
assemble' mp ni (FaT nh xs nc) = FaT_ ni nh xs <$> assemble mp nc
assemble' mp ni (FaF nh k nc) = FaF_ ni nh k <$> assemble mp nc
assemble' mp ni (ExT nh k nc) = ExT_ ni nh k <$> assemble mp nc
assemble' mp ni (ExF nh xs nc) = ExF_ ni nh xs <$> assemble mp nc
assemble' mp ni (RelD f nc) = RelD_ ni f <$> assemble mp nc
assemble' mp ni (AoC x f nc) = AoC_ ni x f <$> assemble mp nc
assemble' mp ni Open = return $ Open_ ni

assemble :: Sol -> BS -> IO Proof
assemble mp nm = do
  (b, f, i) <- cast (HM.lookup nm mp)
  assemble' mp (nm, b, f) i

hasOpen :: Proof -> Bool
hasOpen Id_ {} = False
hasOpen (Cut_ _ _ pf pt) = hasOpen pf || hasOpen pt
hasOpen (RelD_ _ f p) =  hasOpen p
hasOpen (AoC_ _ x f p) = hasOpen p
hasOpen (Open_ _) = True
hasOpen (FunC_ _ nts nf) = False
hasOpen (RelC_ _ nts nt nf) = False
hasOpen (EqR_ ni nf) = False
hasOpen (EqS_ ni nt nf) = False
hasOpen (EqT_ ni nxy nyz nxz) = False
hasOpen (NotT_ ni nm p) = hasOpen p
hasOpen (NotF_ ni nm p) = hasOpen p
hasOpen (OrT_ ni nm ps) = L.any hasOpen ps
hasOpen (OrF_ ni nm k p) = hasOpen p
hasOpen (AndT_ ni nm k p) = hasOpen p
hasOpen (AndF_ ni nm ps) = L.any hasOpen ps
hasOpen (ImpT_ ni nm pa pc) = hasOpen pa || hasOpen pc
hasOpen (ImpFA_ ni nm p) = hasOpen p
hasOpen (ImpFC_ ni nm p) = hasOpen p
hasOpen (IffTO_ ni nm p) = hasOpen p
hasOpen (IffTR_ ni nm p) = hasOpen p
hasOpen (IffF_ ni nm po pr) = hasOpen po || hasOpen pr
hasOpen (FaT_ ni nm xs p) = hasOpen p
hasOpen (FaF_ ni nm k p) = hasOpen p
hasOpen (ExT_ ni nm k p) = hasOpen p
hasOpen (ExF_ ni nm xs p) = hasOpen p

countOpen :: Proof -> (Int, Int)
countOpen (OrT_ _ _ []) = (0, 0)
countOpen (AoC_ _ _ _ p) = countOpen p
countOpen (RelD_ _ _ p) = countOpen p
countOpen (Cut_ _ _ pf pt) = 
  let (co, ct) = countOpen pt in 
  let incr = if hasOpen pf then 1 else 0 in
  (co + incr, ct + 1)
countOpen p = error $ "Final step is not BotT : " ++ show p

proofCheck ::  Int -> Branch -> SignForm -> Proof -> IO ()
proofCheck k bch sf prf = do
  let nm = proofRN prf
  let bch' = HM.insert nm sf bch 
  proofCheck' k bch' prf

hasDup :: (Eq a) => [a] -> Bool
hasDup [] = False
hasDup (x : xs) = x `elem` xs || hasDup xs

proofCheck' :: Int -> Branch -> Proof -> IO ()
proofCheck' _ bch (Id_ _ nt nf) = do 
  tf <- cast $ HM.lookup nt bch
  ff <- cast $ HM.lookup nf bch
  guard $ complementary tf ff
proofCheck' k bch (Cut_ _ f pt pf) = do 
  proofCheck k bch (False, f) pt
  proofCheck k bch (True, f) pf
proofCheck' k bch (RelD_ _ f prf) = do 
  k' <- cast $ checkRelD k f
  proofCheck k' bch (True, f) prf
proofCheck' k bch (AoC_ _ x f prf) = do 
  k' <- cast $ checkAoC k x f
  proofCheck k' bch (True, f) prf
proofCheck' k bch (OrT_ _ nm prfs) = do 
  (True, Or fs) <- cast $ HM.lookup nm bch 
  mapM2 (proofCheck k bch . (True,)) fs prfs
  skip
proofCheck' k bch (OrF_ _ nm m prf) = do 
  (False, Or fs) <- cast $ HM.lookup nm bch 
  f <- cast $ nth m fs 
  proofCheck k bch (False, f) prf
proofCheck' k bch (AndT_ _ nm m prf) = do 
  (True, And fs) <- cast $ HM.lookup nm bch 
  f <- cast $ nth m fs 
  proofCheck k bch (True, f) prf
proofCheck' k bch (AndF_ _ nm prfs) = do 
  (False, And fs) <- cast $ HM.lookup nm bch 
  mapM2 (proofCheck k bch . (False,)) fs prfs
  skip
proofCheck' k bch (ImpT_ _ nm pa pc) = do 
  (True, Imp f g) <- cast $ HM.lookup nm bch 
  proofCheck k bch (False, f) pa
  proofCheck k bch (True, g) pc
proofCheck' k bch (ImpFA_ _ nm prf) = do 
  (False, Imp f _) <- cast $ HM.lookup nm bch 
  proofCheck k bch (True, f) prf
proofCheck' k bch (ImpFC_ _ nm prf) = do 
  (False, Imp _ g) <- cast $ HM.lookup nm bch 
  proofCheck k bch (False, g) prf
proofCheck' k bch (IffTO_ _ nm prf) = do 
  (True, Iff f g) <- cast $ HM.lookup nm bch 
  proofCheck k bch (True, Imp f g) prf
proofCheck' k bch (IffTR_ _ nm prf) = do 
  (True, Iff f g) <- cast $ HM.lookup nm bch 
  proofCheck k bch (True, Imp g f) prf
proofCheck' k bch (IffF_ _ nm po pr) = do 
  (False, Iff f g) <- cast $ HM.lookup nm bch 
  proofCheck k bch (False, Imp f g) po
  proofCheck k bch (False, Imp g f) pr
proofCheck' k bch (NotT_ _ nm prf) = do 
  (True, Not f) <- cast $ HM.lookup nm bch 
  proofCheck k bch (False, f) prf
proofCheck' k bch (NotF_ _ nm prf) = do 
  (False, Not f) <- cast $ HM.lookup nm bch 
  proofCheck k bch (True, f) prf
proofCheck' k bch (FunC_  _ nms nm) = do 
  (False, Eq (Fun f xs) (Fun g ys)) <- cast $ HM.lookup nm bch 
  guard $ f == g
  seqs <- cast $ mapM (`HM.lookup` bch) nms
  xys <- cast $ mapM breakTrueEq seqs
  xys' <- zipM xs ys
  guard $ xys == xys'
proofCheck' k bch (RelC_  _ nms nt nf) = do 
  (True, Rel r xs) <- cast $ HM.lookup nt bch 
  (False, Rel s ys) <- cast $ HM.lookup nf bch 
  guard $ r == s
  seqs <- cast $ mapM (`HM.lookup` bch) nms
  xys <- cast $ mapM breakTrueEq seqs
  xys' <- zipM xs ys
  guard $ xys == xys'
proofCheck' k bch (EqR_ _ nm) = do 
  (False, Eq x y) <- cast $ HM.lookup nm bch
  guard $ x == y
proofCheck' k bch (EqS_ _ nt nf) = do 
  (True, Eq x y) <- cast $ HM.lookup nt bch 
  (False, Eq y' x') <- cast $ HM.lookup nf bch 
  guard $ x == x' && y == y'
proofCheck' k bch (EqT_ _ nxy nyz nxz) = do 
  (True, Eq x y) <- cast $ HM.lookup nxy bch 
  (True, Eq y' z) <- cast $ HM.lookup nyz bch 
  (False, Eq x' z') <- cast $ HM.lookup nxz bch 
  guard $ x == x' && y == y' && z == z'
proofCheck' k bch (FaT_ _ nm xs prf) = do 
  (True, Fa vs f) <- cast $ HM.lookup nm bch 
  vxs <- zipM vs xs 
  let f' = substForm vxs f
  proofCheck k bch (True, f') prf
proofCheck' k bch (FaF_ _ nm ms prf) = do 
  guard $ L.all (k <=) ms && not (hasDup ms) 
  (False, Fa vs f) <- cast $ HM.lookup nm bch 
  let k' = maximum ms + 1
  let xs = L.map par ms
  f' <- substitute vs xs f
  proofCheck k' bch (False, f') prf
proofCheck' k bch (ExT_ _ nm ms prf) = do 
  guard $ L.all (k <=) ms && not (hasDup ms) 
  (True, Ex vs f) <- cast $ HM.lookup nm bch 
  let k' = maximum ms + 1
  let xs = L.map par ms
  f' <- substitute vs xs f
  proofCheck k' bch (True, f') prf
proofCheck' k bch (ExF_ _ nm xs prf) = do 
  (False, Ex vs f) <- cast $ HM.lookup nm bch 
  vxs <- zipM vs xs 
  let f' = substForm vxs f
  proofCheck k bch (False, f') prf
proofCheck' _ _ (Open_ _) = skip

(<$$>) :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
(<$$>) f g h = do
  x <- g
  f x <$> h

(<$$$>) :: (Monad m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
(<$$$>) f g h i = do
  x <- g
  (f x <$$> h) i