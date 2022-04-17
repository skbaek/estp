module Basic where

import Types 
import Data.Text (Text)
import Data.List as L
import Data.HashMap.Lazy as HM ( HashMap, insert, lookup, empty, map, member, mapMaybe, toList, fromListWithKey, delete )
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList )
import Control.Monad.Fail as MF (MonadFail, fail)
import Data.Functor ((<&>))

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

zipM :: (Monad m, MonadFail m, Show a, Show b) => [a] -> [b] -> m [(a, b)]
zipM [] [] = return []
zipM (x : xs) (y : ys) = zipM xs ys <&> ((x, y) :)
zipM xs ys = MF.fail $ "zip fail:\n" ++ show xs ++ "\n" ++ show ys
