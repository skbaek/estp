{-# LANGUAGE OverloadedStrings #-}

module Sat where 

-- import System.Environment
import System.Process
import Types
import Basic
import PP
import Data.Text as T
import Data.List as L
import Data.Text.IO as TIO
import Data.Functor ((<&>))
import Control.Monad as M (guard, foldM, foldM_, (>=>), mzero)

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

intToLit :: [Form] -> Int -> Maybe Form
intToLit as k =
  if k < 0
  then nth (abs k - 1) as <&> Not
  else nth (k - 1) as

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

sat :: [Form] -> IO [Lrat]
sat fs = do
  Prelude.putStr "Premises:\n"
  mapM_ (\ f_ -> Prelude.putStr $ unpack $ ppForm f_ <> "\n") fs
  lss <- cast $ mapM formToLits fs
  as <- cast $ mapM litToAtom (L.concat lss) <&> nub
  nss <- cast $ mapM (litsToNums as) lss
  let max = L.length as
  let head = "p cnf " <> ppInt max <> " " <> ppInt (L.length nss)
  let body = L.map (\ ns -> T.intercalate " " $ L.map ppInt $ ns ++ [0]) nss
  TIO.writeFile "temp.cnf" $ T.intercalate "\n" $ head : body
  print "Running cadical..."
  runCommand "cadical -q temp.cnf temp.drat" >>= waitForProcess
  print "Running drat-trim..."
  runCommand "drat-trim temp.cnf temp.drat -L temp.lrat" >>= waitForProcess
  t <- TIO.readFile "temp.lrat"
  runCommand "rm temp.*" >>= waitForProcess
  let lns = L.map T.words $ T.lines t
  cast $ mapM (textsToLrat as) lns