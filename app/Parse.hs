{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE InstanceSigs #-}

module Parse where

import Types
import Basic
import PP (ppSign, ppElab, ppApp, writeForm, ppList, ppTerm, ppInf, ft, ppForm)

import qualified Data.ByteString as BS
  (drop, uncons, unsnoc, break, splitAt, cons, null, readFile, isPrefixOf, stripPrefix, head)
import Data.ByteString.Builder (Builder, hPutBuilder)
import Data.Char (isDigit, isLower, isUpper, isAlphaNum)
import Data.List (elem, foldl, map, sortBy, (\\), any)
import Control.Applicative (Alternative, empty, (<|>))
import System.Environment (getEnv, unsetEnv)
import Control.Monad ( MonadPlus(mzero), guard, when, mzero, foldM)
import Data.Functor ((<&>))
import Data.Set (Set, member)
import Data.String (fromString)
import Debug.Trace (trace)
import Data.Maybe (fromJust)
import Data.Map as M (Map, empty, lookup, insert)
import qualified Data.Bifunctor as DBF
import System.IO (Handle, hClose)



{- Parser -}

newtype Parser a = Parser { parse :: BS -> Maybe (a, BS) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser cs) = Parser $ \ s ->
    case cs s of
      Nothing -> Nothing
      Just (x, t) -> Just (f x, t)

instance Applicative Parser where
  pure :: a -> Parser a
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser $ \ s ->
    case cs1 s of
      Nothing -> Nothing
      Just (f,t) ->
        case cs2 t of
          Nothing -> Nothing
          Just (x,r) -> Just (f x, r)

instance Monad Parser where
  return :: a -> Parser a
  return = unit
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=)  = bind

instance Alternative Parser where
  empty :: Parser a
  empty = failure
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) = option

instance MonadFail Parser where
  fail :: String -> Parser a
  fail _ = failure

failure :: Parser a
failure = Parser (const Nothing)

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    Nothing -> parse q s
    res     -> res




item :: Parser Char
item = Parser (fmap (DBF.first w2c) . BS.uncons)

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \ s ->
  case parse p s of
    Nothing -> Nothing
    Just (x,t) -> parse (f x) t

unit :: a -> Parser a
unit a = Parser (\s -> Just (a,s))

cutParse :: Parser a -> (a -> Parser b) -> Parser b -> Parser b
cutParse p q r = Parser $ \ s ->
  case parse p s of
    Nothing -> parse r s
    Just (x,t) -> parse (q x) t

commitParse :: Parser a -> Parser b -> Parser b -> Parser b
commitParse p q = cutParse p (const q)

-- -- | One or more.
-- some :: Alternative f => f a -> f [a]
-- some v = some_v
--   where
--     star_v = some_v <|> pure []
--     some_v = (:) <$> v <*> starPlus

ordPlusL :: Parser a -> Parser [a]
ordPlusL p = do
  x <- p
  (x :) <$> ordStarL p

-- | Zero or more.
ordStarL :: Parser a -> Parser [a]
ordStarL p = ordPlusL p <|> pure []

permStarL :: Parser a -> Parser [a]
permStarL p = star p (:) []

star :: Parser a -> (a -> b -> b) -> b -> Parser b
star p f ys = plus p f ys <|> pure ys

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
  then unit c
  else failure

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)

char :: Char -> Parser Char
char c = satisfy (c ==)

notChar :: Char -> Parser Char
notChar c = satisfy (c /=)

peek :: BS -> Parser ()
peek s = Parser $ \ t ->
  if BS.isPrefixOf s t
    then Just ((), t)
    else Nothing

string :: BS -> Parser BS
string s = Parser (fmap (s,) . BS.stripPrefix s)

notBS :: BS -> Parser ()
notBS s = Parser $ \ t ->
  case parse (string s) t of
    Nothing -> Just ((),t)
    _  -> Nothing

litRet :: BS -> Parser BS
litRet s = lit s >> unit s

lit :: BS -> Parser ()
lit s = string s >> ws

space :: Parser ()
space = oneOf " \n\r" >> nil

ws :: Parser ()
ws = permStarL space >> nil

digit :: Parser Char
digit = satisfy isDigit

nil :: Parser ()
nil = Parser $ \ s -> Just (() , s)

eof :: Parser ()
eof = Parser  $ \ t ->
  if BS.null t
    then Just (() , t)
    else Nothing

plus :: Parser a -> (a -> b -> b) -> b -> Parser b
plus p f ys = do
  x <- p
  star p f (f x ys)

-- plusL :: Parser a -> Parser [a]
-- plusL p = plus p (:) []

newline :: Parser ()
newline = char '\n' >> nil

untilChar :: Char -> Parser ()
untilChar c = (char c >> nil) <|> (item >> untilChar c)

untilNlOrEof :: Parser ()
untilNlOrEof = eof <|> untilChar '\n' <|> (item >> untilNlOrEof)

comment :: Parser ()
comment = char '%' >> untilNlOrEof

blockCommentEnd :: Parser ()
blockCommentEnd = (string "*/" >> nil) <|> (item >> blockCommentEnd)

blockComment :: Parser ()
blockComment = string "/*" >> blockCommentEnd

ignOnce :: Parser ()
ignOnce = space <|> comment <|> blockComment

ign :: Parser ()
ign = permStarL ignOnce >> nil

lowerAlpha :: Parser Char
lowerAlpha = satisfy isLower

upperAlpha :: Parser Char
upperAlpha = satisfy isUpper

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isAlphaNum c || c == '_'

alphaNumeric :: Parser Char
alphaNumeric = satisfy isAlphaNumeric

lowerWord :: Parser BS
lowerWord = do
  c <- lowerAlpha
  cs <- ordStarL alphaNumeric
  ign
  unit $ fromString $ c : cs

upperWord :: Parser BS
upperWord = do
  c <- upperAlpha
  cs <- ordStarL alphaNumeric
  ign
  unit $ fromString $ c : cs

atomicWord :: Parser BS
atomicWord = lowerWord <|> singleQuoted

name :: Parser BS
name = atomicWord <|> integer

sign :: Parser Char
sign = char '-' <|> char '+'

integer :: Parser BS
integer = signedInteger <|> decimal

signedInteger :: Parser BS
signedInteger = do
  c <- sign
  t <- decimal
  unit $ BS.cons (c2w c) t

nonZeroNumeric :: Parser Char
nonZeroNumeric = oneOf ['1', '2', '3', '4', '4', '5', '6', '7', '8', '9']

numeric :: Parser Char
numeric = char '0' <|> nonZeroNumeric

positiveDecimal :: Parser BS
positiveDecimal = do
  c <- nonZeroNumeric
  cs <- ordStarL numeric
  unit $ fromString $ c : cs

decimal :: Parser BS
decimal = litRet "0" <|> positiveDecimal

isUnescapedSqChar :: Char -> Bool
isUnescapedSqChar c = c /= '\'' && c /= '\\'

sqChar :: Parser Char
sqChar = satisfy isUnescapedSqChar <|> (char '\\' >> char '\'' <|> char '\\')

isDoChar :: Char -> Bool
isDoChar c = c /= '"' && c /= '\\'

singleQuoted :: Parser BS
singleQuoted = do
  char '\''
  s <- ordPlusL sqChar
  char '\''
  let bs = fromString s
  ws
  unit $ "'" <> bs <> "'"

distinctObject :: Parser BS
distinctObject = do
  char '"'
  s <- ordPlusL $ satisfy isDoChar
  char '"'
  ws
  unit $ fromString $  "\"" ++ s ++ "\""

delimiter :: Parser ()
delimiter = lit ")" <|> lit ","

peekDelimiter :: Parser ()
peekDelimiter = peek ")" <|> peek ","

connective :: Parser BS
connective =
  litRet "&" <|>
  litRet "|" <|>
  litRet "<=>" <|>
  litRet "<~>" <|>
  litRet "=>" <|>
  litRet "<=" <|>
  litRet "=" <|>
  litRet "!="

usefulInfo :: Parser (Maybe [Gent])
usefulInfo =
  (peek ")" >> unit Nothing)
    <|> ( do lit ","
             ts <- generalTermList
             unit $ Just ts )

annotations :: Parser Ant
annotations =
  do { peek ")" ; unit Nothing } <|>
  do {
    lit "," ;
    t <- generalTerm ;
    u <- usefulInfo ;
    unit $ Just (t, u)
  }

conjunction :: Parser [Form]
conjunction = do
  f <- formLazy
  (peekDelimiter >> unit [f])
    <|> (do lit "&"
            fs <- conjunction
            unit (f : fs) )

disjunction :: Parser [Form]
disjunction = do
  f <- formLazy
  (peekDelimiter >> unit [f])
    <|> (do lit "|"
            fs <- disjunction
            unit (f : fs) )

formClose :: Form -> BS -> Parser Form
formClose f "|" = do { fs <- disjunction ; unit (Or $ f : fs) }
formClose f "&" = do { fs <- conjunction ; unit (And $ f : fs) }
formClose f "=>"  = do { g <- formLazy ; unit $ Imp f g }
formClose f "<="  = do { g <- formLazy ; unit $ Imp g f }
formClose f "<=>" = do { g <- formLazy ; unit $ Iff f g }
formClose f "<~>" = do { g <- formLazy ; unit $ Not $ Iff f g }
formClose _ _ = failure

parenFormLazy :: Parser Form
parenFormLazy = do
  lit "("
  f <- form
  lit ")"
  unit f

notformLazy :: Parser Form
notformLazy = do
  lit "~"
  f <- formLazy
  unit $ Not f

var :: Parser BS
var = upperWord

commaSepCore :: Parser a -> Parser [a]
commaSepCore p = commitParse (lit ",") (do { x <- p ; xs <- commaSepCore p ; unit $ x : xs }) (unit [])

commaSepPlus :: Parser a -> Parser [a]
commaSepPlus p = do
  x <- p
  xs <- commaSepCore p
  unit (x : xs)

commaSepStar :: Parser a -> Parser [a]
commaSepStar p = commaSepPlus p <|> unit []

vars :: Parser [BS]
vars = do
  lit "["
  vs <- commaSepPlus var
  lit "]"
  unit vs

faformLazy :: Parser Form
faformLazy = do
  lit "!"
  vs <- vars
  lit ":"
  f <- formLazy
  unit $ Fa vs f

exformLazy :: Parser Form
exformLazy = do
  lit "?"
  vs <- vars
  lit ":"
  f <- formLazy
  unit $ Ex vs f

infixOp :: Parser BS
infixOp = (notBS "=>" >> litRet "=") <|> litRet "!="

regFunctor :: Parser Funct
regFunctor = do
  tx <- atomicWord <|> do { lit "$" ; w <- atomicWord ; unit $ BS.cons (c2w '$') w } <|> distinctObject
  return $ Reg tx

idxFunctor :: Parser Funct
idxFunctor = do
  tx <- singleQuoted
  ('#' :> tx') <- cast $ parseSingleQuote tx
  k <- cast $ bs2int tx'
  return $ Idx k

functor :: Parser Funct
functor = idxFunctor <|> regFunctor

arguments :: Parser [Term]
arguments = do { lit "(" ; ts <- terms ; lit ")" ; unit ts } <|> (ws >> unit [])

gargs :: Parser [Gent]
gargs = do { lit "(" ; ts <- commaSepPlus generalTerm ; lit ")" ; unit ts } <|> (ws >> unit [])

terms :: Parser [Term]
terms = commaSepPlus term

term :: Parser Term
term = (upperWord >>= unit . Var) <|> do { f <- functor ; ts <- arguments ; unit $ Fun f ts }

generalTermList :: Parser [Gent]
generalTermList = do
  lit "["
  ts <- commaSepStar generalTerm ;
  lit "]"
  unit ts

generalTerm :: Parser Gent
generalTerm =
  do { ts <- generalTermList ; unit (Genl ts) } <|>
  do { f <- atomicWord ; ts <- gargs ; unit $ GenT f ts } <|>
  do { kt <- integer ; cast (bs2int kt) >>= (unit . Genn) } <|>
  do { v <- upperWord ; unit (Genv v) } <|>
  do { lit "$fof(" ; f <- form ; lit ")" ; unit (GenF f) }

termInfixOpformLazy :: Term -> BS -> Parser Form
termInfixOpformLazy t "=" = do
  s <- term
  unit $ Eq t s
termInfixOpformLazy t "!=" = do
  s <- term
  unit $ Not $ Eq t s
termInfixOpformLazy _ _ = failure

termToAtom :: Term -> Parser Form
termToAtom (Fun r ts) = unit $ Rel r ts
termToAtom _ = failure

termformLazy :: Term -> Parser Form
termformLazy t = cutParse infixOp (termInfixOpformLazy t) (termToAtom t)

verum :: Parser Form
verum = lit "$true" >> unit Top

falsum :: Parser Form
falsum = lit "$false" >> unit Bot

formLazy :: Parser Form
formLazy =
  verum <|> falsum <|> parenFormLazy <|> notformLazy <|>
  faformLazy <|> exformLazy <|> (term >>= termformLazy)

form :: Parser Form
form = do
  f <- formLazy
  (eof >> return f) <|> (peekDelimiter >> return f) <|> (connective >>= formClose f)

-- preInc :: Parser PreInput
-- preInc = do
--   lit "include("
--   w <- singleQuoted
--   lit ")"
--   lit "."
--   ign
--   unit (PreInc w)

inc :: Parser Input
inc = do
  lit "include("
  w <- singleQuoted
  lit ")"
  lit "."
  ign
  unit (IncInput w)


formTextCore :: BS -> Maybe ((), BS)
formTextCore tx = do
  k <- formLength 0 tx
  return ((), BS.drop k tx)

formText :: Parser ()
formText = Parser formTextCore

formLength :: Int -> BS -> Maybe Int
formLength 0 (')' :> _) = Just 0
formLength 0 (',' :> _) = Just 0
formLength 0 ('(' :> bs) = succ <$> formLength 1 bs
formLength 0 ('[' :> bs) = succ <$> formLength 1 bs
formLength 0 (_ :> bs) = succ <$> formLength 0 bs
formLength k (')' :> bs) = succ <$> formLength (k - 1) bs
formLength k (']' :> bs) = succ <$> formLength (k - 1) bs
formLength k ('(' :> bs) = succ <$> formLength (k + 1) bs
formLength k ('[' :> bs) = succ <$> formLength (k + 1) bs
formLength k (_   :> bs) = succ <$> formLength k bs
formLength _ _ = error "Cannot find formula length"

readTptp :: String -> Prob -> IO Prob
readTptp nm bch = do
  -- ps "Reading file as text...\n"
  bs <- BS.readFile nm
  -- ps "Parsing the text read...\n"
  (_, bs') <- cast $ parse ign bs
  parseTptp bs' bch

parseTptp :: BS -> Prob -> IO Prob
parseTptp bs bch = do
  (i,bs') <- cast $ parse input bs
  bch' <- addInput i bch
  if BS.null bs'
  then return bch'
  else parseTptp bs' bch'

addInput :: Input -> Prob -> IO Prob
addInput (IncInput bs) bch = do
  tptp <- getEnv "TPTP"
  bs' <- cast $ unquote bs
  readTptp (tptp ++ "/" ++ bs2str bs') bch
addInput (AnfInput (nm, rl, f, Nothing)) bch = do
  return $ M.insert nm (conjecturize rl $ univClose f) bch
addInput (AnfInput (_, _, _, Just _)) bch = error "Anntation found in Prob"

readEstp :: String -> IO Sol
readEstp n = BS.readFile n >>= runParser (ign >> estp)

starMap :: (Ord k) => Parser (k, v) -> Map k v -> Parser (Map k v)
starMap p = star p (uncurry insert)

plusMap :: (Ord k) => Parser (k, v) -> Map k v -> Parser (Map k v)
plusMap p = plus p (uncurry insert)

elabFormInf :: Parser Inf
elabFormInf = do
  lit "cnf(" <|> lit "fof("
  _ <- name
  lit ","
  _ <- lowerWord
  lit ","
  _ <- formText
  Just (gt, _) <- annotations
  i <- cast $ gentParseInf gt
  lit ")"
  lit "."
  ign
  unit i

elabForm :: Parser (BS, (Bool, Form, Inf))
elabForm = do
  -- (nm, rl, f, Just (GenT "inference" [gt], Nothing)) <- anf
  (nm, rl, f, Just (gt, _)) <- anf
  sgn <- cast $ textParseBool rl
  inf <- cast $ gentParseInf gt
  unit (nm, (sgn, f, inf))

textToIdxFunct :: BS -> Maybe Funct
textToIdxFunct tx = do
  ('#' :> tx') <- cast $ parseSingleQuote tx
  cast (Idx <$> bs2int tx')

textParseFunct :: BS -> Maybe Funct
textParseFunct tx = textToIdxFunct tx <|> return (Reg tx)

gentParseTerm :: Gent -> Maybe Term
gentParseTerm (GenT tx ts) = do
  f <- textParseFunct tx
  xs <- mapM gentParseTerm ts
  return $ Fun f xs
gentParseTerm (Genv v) = return $ Var v
gentParseTerm _ = mzero

gentParseBS :: Gent-> Maybe BS
gentParseBS (GenT t []) = return t
gentParseBS _ = mzero

gentParseInt :: Gent -> Maybe Int
gentParseInt (Genn k) = Just k
gentParseInt _ = Nothing

gentParseInf :: Gent -> Maybe Inf
gentParseInf (GenT "cut" [GenF f, gtf, gtt]) = do
  nf <- gentParseBS gtf
  nt <- gentParseBS gtt
  return $ Cut f nf nt
gentParseInf (GenT "id" [gt0, gt1]) = do
  m <- gentParseBS gt0
  n <- gentParseBS gt1
  return $ Id m n
gentParseInf (GenT "iffto" [gth, gtc]) = do
  nh <- gentParseBS gth
  nc <- gentParseBS gtc
  return $ IffTO nh nc
gentParseInf (GenT "ifftr" [gth, gtc]) = do
  nh <- gentParseBS gth
  nc <- gentParseBS gtc
  return $ IffTR nh nc
gentParseInf (GenT "ifff" [gth, gt0, gt1]) = do
  nh <- gentParseBS gth
  n0 <- gentParseBS gt0
  n1 <- gentParseBS gt1
  return $ IffF nh n0 n1
gentParseInf (GenT "impfa" [gth, gtc]) = do
  nh <- gentParseBS gth
  nc <- gentParseBS gtc
  return $ ImpFA nh nc
gentParseInf (GenT "impfc" [gth, gtc]) = do
  nh <- gentParseBS gth
  nc <- gentParseBS gtc
  return $ ImpFC nh nc
gentParseInf (GenT "impt" [gth, gt0, gt1]) = do
  nh <- gentParseBS gth
  n0 <- gentParseBS gt0
  n1 <- gentParseBS gt1
  return $ ImpT nh n0 n1
gentParseInf (GenT "bott" [gt]) = do
  nm <- gentParseBS gt
  return $ BotT nm 
gentParseInf (GenT "topf" [gt]) = do
  nm <- gentParseBS gt
  return $ TopF nm 
gentParseInf (GenT "ort" [gt, Genl gts]) = do
  nm <- gentParseBS gt
  nms <- mapM gentParseBS gts
  return $ OrT nm nms
gentParseInf (GenT "orf" [gth, Genn k, gtc]) = do
  nh <- gentParseBS gth
  nc <- gentParseBS gtc
  return $ OrF nh k nc
gentParseInf (GenT "andt" [gth, Genn k, gtc]) = do
  nh <- gentParseBS gth
  nc <- gentParseBS gtc
  return $ AndT nh k nc
gentParseInf (GenT "andf" [gt, Genl gts]) = do
  nm <- gentParseBS gt
  nms <- mapM gentParseBS gts
  return $ AndF nm nms
gentParseInf (GenT "faf" [gth, Genl gts, gtc]) = do
  nh <- gentParseBS gth
  ks <- mapM gentParseInt gts
  nc <- gentParseBS gtc
  return $ FaF nh ks nc
gentParseInf (GenT "ext" [gth, Genl gts, gtc]) = do
  nh <- gentParseBS gth
  ks <- mapM gentParseInt gts
  nc <- gentParseBS gtc
  return $ ExT nh ks nc
gentParseInf (GenT "fat" [gth, Genl gts, gtc]) = do
  nh <- gentParseBS gth
  xs <- mapM gentParseTerm gts
  nc <- gentParseBS gtc
  return $ FaT nh xs nc
gentParseInf (GenT "exf" [gth, Genl gts, gtc]) = do
  nh <- gentParseBS gth
  xs <- mapM gentParseTerm gts
  nc <- gentParseBS gtc
  return $ ExF nh xs nc
gentParseInf (GenT "nott" [gth, gtc]) = do
  nh <- gentParseBS gth
  nc <- gentParseBS gtc
  return $ NotT nh nc
gentParseInf (GenT "notf" [gth, gtc]) = do
  nh <- gentParseBS gth
  nc <- gentParseBS gtc
  return $ NotF nh nc
gentParseInf (GenT "eqr" [gt]) = EqR <$> gentParseBS gt
gentParseInf (GenT "eqs" gts) = do
  [nm0, nm1] <- mapM gentParseBS gts
  return $ EqS nm0 nm1
gentParseInf (GenT "eqt" gts) = do
  [nm0, nm1, nm2] <- mapM gentParseBS gts
  return $ EqT nm0 nm1 nm2
gentParseInf (GenT "func" [Genl gts, gt]) = do
  nms <- mapM gentParseBS gts
  nm <- gentParseBS gt
  return $ FunC nms nm
gentParseInf (GenT "relc" [Genl gts, gt0, gt1]) = do
  nms <- mapM gentParseBS gts
  m <- gentParseBS gt0
  n <- gentParseBS gt1
  return $ RelC nms m n
gentParseInf (GenT "aoc" [gtx, GenF f, gtn]) = do
  x <- gentParseTerm gtx
  nm <- gentParseBS gtn
  return $ AoC x f nm
gentParseInf (GenT "reld" [GenF f, gt]) = RelD f <$> gentParseBS gt
gentParseInf (GenT "open" []) = return Open
gentParseInf t = error $ "inf reader : " <> fromString (show t)

textParseBool :: BS -> Maybe Bool
textParseBool "true" = return True
textParseBool "false" = return False
textParseBool _ = error "Cannot read Bool"

estp :: Parser Sol
estp = starMap elabForm M.empty

parseSingleQuote :: BS -> Maybe BS
parseSingleQuote ('\'' :> t) = do
  (t', '\'') <- DBF.second w2c <$> BS.unsnoc t
  return t'
parseSingleQuote _ = mzero

anf :: Parser Anf
anf = do
  n <- (lit "cnf(" <|> lit "fof(") >> name
  r <- lit "," >> lowerWord
  f <- lit "," >> form
  a <- annotations
  lit ")" >> lit "." >> ign
  unit (n, r, f, a)

input :: Parser Input
input = (AnfInput <$> anf) <|> inc

runParser :: Parser a -> BS -> IO a
runParser p bs = do
  (rst, rem) <- cast $ parse p bs
  guard $ BS.null rem
  return rst

rule :: Parser BS
rule =
  litRet "O" <|> litRet "I" <|> litRet "C" <|> litRet "D" <|>
  litRet "A" <|> litRet "F" <|> litRet "R" <|>
  litRet "=R" <|> litRet "=S" <|> litRet "=T" <|>
  litRet "~T" <|> litRet "~F" <|>
  litRet "|T" <|> litRet "|F" <|>
  litRet "&T" <|> litRet "&F" <|>
  litRet ">T" <|> litRet ">FA" <|> litRet ">FC" <|>
  litRet "^TO" <|> litRet "^TR" <|> litRet "^F" <|>
  litRet "!T" <|> litRet "!F" <|>
  litRet "?T" <|> litRet "?F"


fetch :: (MonadFail m) => Branch -> BS -> m SignForm
fetch bch nm = cast (M.lookup nm bch)

pguard :: String -> Bool -> Parser ()
pguard tx True = skip
pguard tx False = error tx

linearize :: Proof -> [Elab]
linearize (Id_ ni nt nf) = [(ni, Id nt nf)]
linearize (TopF_ ni np) = [(ni, TopF np)]
linearize (BotT_ ni np) = [(ni, BotT np)]
linearize (Cut_ ni f p q) = (ni, Cut f (proofRN p) (proofRN q)) : linearize p ++ linearize q
linearize (FunC_ ni xs nm) = [(ni, FunC xs nm)]
linearize (RelC_ ni xs nt nf) = [(ni, RelC xs nt nf)]
linearize (EqR_ ni nm) = [(ni, EqR nm)]
linearize (EqS_ ni nt nf) = [(ni, EqS nt nf)]
linearize (EqT_ ni nxy nyz nxz) = [(ni, EqT nxy nyz nxz)]
linearize (NotT_ ni nm p) = (ni, NotT nm (proofRN p)) : linearize p
linearize (NotF_ ni nm p) = (ni, NotF nm (proofRN p)) : linearize p
linearize (OrT_ ni nm ps) = (ni, OrT nm (map proofRN ps)) : concatMap linearize ps
linearize (OrF_ ni nm k p) = (ni, OrF nm k (proofRN p)) : linearize p
linearize (AndT_ ni nm k p) = (ni, AndT nm k (proofRN p)) : linearize p
linearize (AndF_ ni nm ps) = (ni, AndF nm (map proofRN ps)) : concatMap linearize ps
linearize (ImpT_ ni nm p q) = (ni, ImpT nm (proofRN p) (proofRN q)) : linearize p ++ linearize q
linearize (ImpFA_ ni nm p) = (ni, ImpFA nm (proofRN p)) : linearize p
linearize (ImpFC_ ni nm p) = (ni, ImpFC nm (proofRN p)) : linearize p
linearize (IffTO_ ni nm p) = (ni, IffTO nm (proofRN p)) : linearize p
linearize (IffTR_ ni nm p) = (ni, IffTR nm (proofRN p)) : linearize p
linearize (IffF_ ni nm p q) = (ni, IffF nm (proofRN p) (proofRN q)) : linearize p ++ linearize q
linearize (FaT_ ni nm xs p) = (ni, FaT nm xs (proofRN p)) : linearize p
linearize (FaF_ ni nm k p) = (ni, FaF nm k (proofRN p)) : linearize p
linearize (ExT_ ni nm k p) = (ni, ExT nm k (proofRN p)) : linearize p
linearize (ExF_ ni nm xs p) = (ni, ExF nm xs (proofRN p)) : linearize p
linearize (RelD_ ni f p) = (ni, RelD f (proofRN p)) : linearize p
linearize (AoC_ ni x f p) = (ni, AoC x f (proofRN p)) : linearize p
linearize (Open_ ni) = [(ni, Open)]

orCount :: Parser Bool -> Parser Bool -> Parser Bool
orCount p q = do
  bp <- p
  bq <- q
  unit $ bp || bq

count :: (Int, Int) -> Branch -> Parser (Int, Int)
count pr@(oc, tc) bch_ = do
  trace "Counting...\n" skip
  (nm, (sgn, f, i)) <- elabForm
  let bch = M.insert nm (sgn, f) bch_
  case i of
    OrT _ _ -> unit pr
    Cut {} -> do 
      b <- subCount bch False
      count (oc + if b then 1 else 0, tc + 1) bch
    RelD _ _ -> count pr bch
    AoC {} -> count pr bch
    _ -> error "invalid proof structure"

subCount :: Branch -> Bool -> Parser Bool
subCount bch_ op = do
  (nm, (sgn, f, i)) <- elabForm
  let bch = M.insert nm (sgn, f) bch_
  case i of
    Id nt nf -> unit op
    TopF _ -> unit op
    BotT _ -> unit op
    Cut f nf nt -> subCount bch op >>= subCount bch
    RelD f nm -> subCount bch op
    AoC x f nm -> subCount bch op
    Open -> unit True
    FunC nms nm -> unit op
    RelC nts nt nf -> unit op
    EqR nm -> unit op
    EqS nt nf -> unit op
    EqT nxy nyz nxz -> unit op
    NotT nm _ -> subCount bch op
    NotF nm _ -> subCount bch op
    OrT nm _ -> do
      (True, Or fs) <- fetch bch nm
      foldM (\ b_ _ -> subCount bch b_) op fs
    OrF nm m _ -> subCount bch op
    AndT nm m _ -> subCount bch op
    AndF nm _ -> do
      (False, And fs) <- fetch bch nm
      foldM (\ b_ _ -> subCount bch b_) op fs
    ImpT nm _ _ -> subCount bch op >>= subCount bch
    ImpFA nm _ -> subCount bch op
    ImpFC nm _ -> subCount bch op
    IffTO nm _ -> subCount bch op
    IffTR nm _ -> subCount bch op
    IffF nm _ _ -> subCount bch op >>= subCount bch
    FaT nm xs _ -> subCount bch op
    FaF nm m _ -> subCount bch op
    ExT nm m _ -> subCount bch op
    ExF nm xs _ -> subCount bch op
  
check :: Int -> Branch -> BS -> Bool -> Form -> Parser ()
check k b0 n0 s0 f0 = do
  -- trace (bs2str $ "Inserting formula : " <> n0 <> "\n") skip
  let b = insert n0 (s0, f0) b0
  i <- elabFormInf
  -- trace (bd2str $ "Checking inf : " <> ppInf i <> "\n\n") skip
  case i of
    Id nt nf -> do
      (True, f) <- fetch b nt
      (False, f') <- fetch b nf
      guard (f == f')
    TopF np -> do 
      (False, Top) <- fetch b np 
      skip
    BotT np -> do 
      (True, Bot) <- fetch b np 
      skip
    Cut f nf nt -> do
      check k b nf False f
      check k b nt True f
    RelD f n -> do
      k' <- cast $ checkRelD k f
      check k' b n True f
    AoC x f n -> do
      k' <- cast $ checkAoC k x f
      check k' b n True f
    Open -> return ()
    FunC nts nf -> do
      eqns <- mapM (fetch b) nts
      (False, Eq (Fun f xs) (Fun g ys)) <- fetch b nf
      xys <- cast $ mapM breakTrueEq eqns
      xys' <- zipM xs ys
      guard (f == g && xys == xys')
    RelC nts nt nf -> do
      eqns <- mapM (fetch b) nts
      (True, Rel r xs) <- fetch b nt
      (False, Rel s ys) <- fetch b nf
      xys <- cast $ mapM breakTrueEq eqns
      xys' <- zipM xs ys
      guard (r == s && xys == xys')
    EqR n -> do
      (False, Eq x y) <- fetch b n
      guard (x == y)
    EqS nt nf -> do
      (True, Eq x y) <- fetch b nt
      (False, Eq y' x') <- fetch b nf
      guard (x == x' && y == y')
    EqT nxy nyz nxz -> do
      (True, Eq x y) <- fetch b nxy
      (True, Eq y' z) <- fetch b nyz
      (False, Eq x' z') <- fetch b nxz
      guard (x == x' && y == y' && z == z')
    NotT nh n -> do
      (True, Not g) <- fetch b nh
      check k b n False g
    NotF nh n -> do
      (False, Not f) <- fetch b nh
      check k b n True f
    OrT nh ns -> do
      guard $ not $ null ns
      (True, Or fs) <- fetch b nh
      mapM2 (flip (check k b) True) ns fs >> skip
    OrF nh m n -> do
      (False, Or fs) <- fetch b nh
      f <- cast $ nth m fs
      check k b n False f
    AndT nh m n -> do
      (True, And fs) <- fetch b nh
      f <- cast $ nth m fs
      check k b n True f
    AndF nh ns -> do
      guard $ not $ null ns
      (False, And gs) <- fetch b nh
      mapM2 (flip (check k b) False) ns gs >> skip
    ImpT nh na nc -> do
      (True, Imp f g) <- fetch b nh
      check k b na False f
      check k b nc True g
    ImpFA nh n -> do
      (False, Imp f _) <- fetch b nh
      check k b n True f
    ImpFC nh n -> do
      (False, Imp _ g) <- fetch b nh
      check k b n False g
    IffTO nh n -> do
      (True, Iff f g) <- fetch b nh
      check k b n True (f ==> g)
    IffTR nh n -> do
      (True, Iff f g) <- fetch b nh
      check k b n True (g ==> f)
    IffF nh no nr -> do
      (False, Iff f g) <- fetch b nh
      check k b no False (f ==> g)
      check k b nr False (g ==> f)
    FaT nh xs n -> do
      guard $ not $ null xs
      (True, Fa vs f) <- fetch b nh
      f' <- substitute vs xs f
      check k b n True f'
    FaF nh ks n -> do
      guard $ not $ null ks
      (False, Fa vs f) <- fetch b nh
      guard (all (k <=) ks && not (hasDup ks))
      f' <- substitute vs (map par ks) f
      check (maximum ks + 1) b n False f'
    ExT nh ks n -> do
      guard $ not $ null ks
      (True, Ex vs f) <- fetch b nh
      guard (all (k <=) ks && not (hasDup ks))
      f' <- substitute vs (map par ks) f
      check (maximum ks + 1) b n True f'
    ExF nh xs n -> do
      guard $ not $ null xs
      (False, Ex vs f) <- fetch b nh
      f' <- substitute vs xs f
      check k b n False f'

checkBar :: [BS] -> Term -> Bool
checkBar vs (Var _) = False
checkBar vs (Fun (Reg _) _) = False
checkBar vs (Fun (Idx m) xs) = 
  case mapM breakVar xs of 
    Just ws -> vs == ws
    _ -> False

checkFoo :: Term -> Form -> Bool
checkFoo x (Fa vs (Imp (Ex [w] f) g)) = 
  checkBar vs x &&  substForm [(w, x)] f == g
checkFoo x (Fa vs (Imp (Ex (w : ws) f) g)) = 
  checkBar vs x &&  substForm [(w, x)] (Ex ws f) == g
checkFoo x (Imp (Ex [w] f) g) = do
  checkBar [] x && substForm [(w, x)] f == g
checkFoo x (Imp (Ex (w : ws) f) g) = do
  checkBar [] x && substForm [(w, x)] (Ex ws f) == g
checkFoo x _ = False

findInst :: [BS] -> [Int] -> BS -> Term
findInst (v : vs) (k : ks) w 
  | v == w = par k
  | otherwise = findInst vs ks w
findInst _ _ _ = error "no match"

errorIf :: Bool -> String -> a -> a
errorIf False str _ = error str
errorIf _ _ x = x

expandAoC :: Elab -> Builder
expandAoC elb@(ni, AoC x g nm) 
  | checkFoo x g = ppElab elb
  | otherwise = 
    case (x, g) of 
      (Fun (Idx k) vws, Fa vs body) -> 
        -- let body = Imp (Ex [w] ga) gc in
        let ws = fromJust (mapM breakVar vws) in
        let h = Fa ws body in
        let nm0 = nm <> "_skolem_well_formed" in
        let nm1 = nm <> "_skolem_ill_formed" in
        let nm2 = nm <> "_skolem_ill_formed_inst" in
        let nm3 = nm <> "_skolem_well_formed_inst" in
        let ks = rangeOver (k + 1) vs in
        let xs = map (findInst vs ks) ws in
        let g' = fromJust (substitute vs (map par ks) body) in
        let h' = fromJust (substitute ws xs body) in
        errorIf (g' == h') "Substitution results do not match"
          ppElab (ni, AoC x h nm0) <>
          ppElab ((nm0, True, h), Cut g nm1 nm) <> 
          ppElab ((nm1, False, g), FaF nm1 ks nm2) <> 
          ppElab ((nm2, False, g'), FaT nm0 xs nm3) <> 
          ppElab ((nm3, True, h'), Id nm3 nm2) 
      _ -> error $ "Not an AoC\nx : " ++ bd2str (ppTerm x) ++ "\ng : " ++ bd2str (ppForm g)

expandAoC elb = ppElab elb
  -- ppApp "fof" [ft nm, ppSign sgn, writeForm f, ppInf i] <> ".\n"

convert :: Handle -> (Elab -> Builder) -> BS -> IO ()
convert h cv bs 
  | BS.null bs = skip
  | otherwise = do
    ((nm, (sgn, f, i)), bs') <- cast $ parse elabForm bs
    hPutBuilder h $ cv ((nm, sgn, f), i)
    convert h cv bs'
