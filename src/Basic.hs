{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# LANGUAGE LambdaCase #-}

module Basic where

import Types
import Data.Text as T (Text, uncons, unpack)
import Data.List as L
import Data.Map as HM ( Map, insert, lookup, empty, map, member, mapMaybe, toList, fromListWithKey, delete )
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList )
import Control.Monad.Fail as MF (MonadFail, fail)
-- import Control.Monad.Plus as MP 
import Control.Applicative as A
import Data.Functor ((<&>))
-- import Data.Hashable (Hashable)

pattern (:>) :: Char -> Text -> Text
pattern x :> xs <- (T.uncons -> Just (x, xs))

substBv :: [(Text, Term)] -> Text -> Term
substBv [] s = Bv s
substBv ((t, x) : txs) s = if t == s then x else substBv txs s

substTerm :: [(Text, Term)] -> Term -> Term
substTerm txs (Fv k) = Fv k
substTerm txs (Par k) = Par k
substTerm txs (Bv t) = substBv txs t
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

listFvs :: Int -> [Text] -> (Int, [Term])
listFvs k [] = (k, [])
listFvs k (_ : vs) =
  let (m, xs) = listFvs (k + 1) vs in
  (m, Fv k : xs)

listPars :: Int -> [Text] -> (Int, [Term])
listPars k [] = (k, [])
listPars k (_ : vs) =
  let (m, xs) = listPars (k + 1) vs in
  (m, Par k : xs)

zipM :: (Monad m, Alternative m) => [a] -> [b] -> m [(a, b)]
zipM [] [] = return []
zipM (x : xs) (y : ys) = zipM xs ys <&> ((x, y) :)
zipM xs ys = A.empty

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

(?>) :: Maybe a -> (a -> b) -> b -> b
(?>) (Just x) f _ = f x
(?>) Nothing _ y = y

isPerm :: (Eq a) => [a] -> [a] -> Bool
isPerm xs ys = null (xs \\ ys) && null (ys \\ xs)

mark :: Int -> IO ()
mark k = print $ "Marking checkpoint " <> show k

pt :: Text -> IO ()
pt t = Prelude.putStr $ unpack t