{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module BT where
import Basic
import Control.Monad -- as M (guard, foldM, foldM_ ,(>=>))
import Control.Monad.Fail as MF ( MonadFail(..) )
import Control.Applicative ( Alternative((<|>)) )
import Data.Functor ((<&>))
import Debug.Trace ( trace )
import Data.Text as T

type BT a = [a]

pick :: (MonadFail m, Alternative m) => [a] -> m a
pick [] = MF.fail ""
pick (x : xs) = return x <|> pick xs

pluck :: [a] -> BT (a, [a])
pluck [] = []
pluck (x : xs) = 
  return (x, xs) <|>
  do (x', xs') <- pluck xs
     return (x', x : xs') 

(!>=) :: [a] -> (a -> [b]) -> [b] -> [b]
(!>=) xs f ys =
  case xs of
    (x : _) -> f x
    [] -> ys

(!>) :: [a] -> [b] -> [b] -> [b]
(!>) xs ys = xs !>= const ys

(<!>) :: [a] -> [a] -> [a]
(<!>) xs = xs !>= return

fails :: BT a -> BT ()
fails [] = return ()
fails _ = []

commit :: BT a -> BT a
commit [] = []
commit (x : _) = [x]

traceBT :: Text -> BT ()
traceBT t = trace (T.unpack t) $ return ()

instance MonadCast [] where
  cast = \case 
           [] -> MF.fail ""
           (x : xs) -> return x