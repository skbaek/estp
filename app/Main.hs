{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use infix" #-}

module Main where

import Types (BS, Branch) 
import Basic (ps, top, proofCheck, assemble)
import Parse (linearize, readTptp, check, estp, ign, runParser, convert) 
import PP (ppElab, ppListNl)
import System.Environment (getArgs)
import Data.List (intercalate)
import Data.Map as M (map, empty) 
import Data.ByteString as BS (readFile)
import Data.ByteString.Builder as BD (writeFile)
import System.IO (Handle, openFile, IOMode(WriteMode))

checkPerm :: Branch -> BS -> IO ()
checkPerm bch bs = do 
  estp <- runParser (ign >> estp) bs
  prf <- assemble estp "root"
  proofCheck 0 bch (True, top) prf

checkOrd :: Branch -> BS -> IO ()
checkOrd bch = runParser (ign >> check 0 bch "root" True top)

mainArgs :: Bool -> [String] -> IO ()
mainArgs vb ("check" : pnm : enm : _) = do
  prob <- readTptp pnm empty
  let bch = M.map (True,) prob
  bs <- BS.readFile enm
  checkOrd bch bs -- <|> checkPerm bch bs
  ps "Proof checked.\n"
mainArgs vb ("sort" : onm : nnm : _) = do
  bs <- BS.readFile onm
  estp <- runParser (ign >> estp) bs
  prf <- assemble estp "root"
  let estp' = linearize prf
  BD.writeFile nnm $ ppListNl ppElab $ linearize prf
mainArgs _ ("convert" : pnm : onm : nnm : _) = do
  prob <- readTptp pnm empty
  let bch = M.map (True,) prob
  bs <- BS.readFile onm
  h <- openFile nnm WriteMode
  convert h bch "root" True top bs
  return ()
mainArgs _ args = error $ "Invalid main args : " ++ intercalate "; " args
-- mainArgs vb ("count" : pnm : enm : _) = do
--   prob <- readTptp pnm HM.empty
--   let bch = HM.map (True,) prob
--   bs <- BS.readFile enm
--   (oc, tc) <- runParser (count (0, 0) bch) bs
--   BS.appendFile "count.pl" $ toByteString' $ "count('" <> BD.string7 enm <> "', " <> ppInt oc <> ", " <> ppInt tc <> ").\n"
-- mainArgs vb ("assemble" : enm : anm : _) =
--   assembleOrd enm anm <|> assemblePerm vb enm anm
-- mainArgs vb ("check" : pnm : anm : _) = do
--   when vb $ ps $ "Reading TPTP : " ++ pnm ++ " ...\n"
--   tptp <- readTptp pnm HM.empty
--   let bch = HM.map (True,) tptp
--   when vb $ ps $ "Reading ASTP : " ++ anm ++ " ...\n"
--   bs <- BS.readFile anm
--   runParser (proofCheck vb 0 bch True top) bs
--   ps "Proof checked.\n"
-- mainArgs vb ("extract" : pnm : anm : enm : _) = do
--   when vb $ ps $ "Reading TPTP : " ++ pnm ++ " ...\n"
--   tptp <- readTptp pnm HM.empty
--   let bch = HM.map (True,) tptp
--   when vb $ ps $ "Reading ASTP : " ++ anm ++ " ...\n"
--   prf <- BS.readFile anm >>= runParser (proof bch True (And []))
--   BD.writeFile enm $ ppListNl ppElab $ linearize prf

main :: IO ()
main = do
  args <- getArgs 
  let vb = "silent" `notElem` args
  mainArgs vb args