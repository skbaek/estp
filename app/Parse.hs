{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE InstanceSigs #-}

module Parse where

import Types
import Basic

import qualified Data.ByteString as BS
  (uncons, unsnoc, break, splitAt, cons, null, readFile, isPrefixOf, stripPrefix, head)
import Data.Char (isDigit, isLower, isUpper, isAlphaNum)
import Data.List (elem, foldl, map, sortBy, (\\))
import Control.Applicative (Alternative, empty, (<|>))
import System.Environment (getEnv, unsetEnv)
import Control.Monad ( MonadPlus(mzero), guard, when, mzero )
import Data.Functor ((<&>))
import Data.Set (Set, member)
import Data.String (fromString)
import Debug.Trace (trace)
import Data.Map as M (Map, empty, lookup, insert)
import qualified Data.Bifunctor as DBF



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
verum = lit "$true" >> unit (And [])

falsum :: Parser Form
falsum = lit "$false" >> unit (Or [])

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

-- preInput :: Parser PreInput
-- preInput = preAnf <|> preInc
-- 
-- preAnf :: Parser PreInput
-- preAnf = do
--   lit "cnf(" <|> lit "fof("
--   n <- name
--   lit ","
--   r <- lowerWord
--   lit ","
--   bs <- preForm
--   a <- annotations
--   lit ")"
--   lit "."
--   ign
--   unit (PreAnf n r bs a)
-- 
-- preForm :: Parser BS
-- preForm = Parser textParsePreForm




textParsePreForm :: BS -> Maybe (BS, BS)
textParsePreForm tx = do
  k <- formLength 0 tx
  return $ BS.splitAt (fromIntegral k) tx

formLength :: Int -> BS -> Maybe Int
formLength 0 tx =
  case bsrec tx of
    Just (')', tx') -> Just 0
    Just ('(', tx') -> succ <$> formLength 1 tx'
    Just ('[', tx') -> succ <$> formLength 1 tx'
    Just (_, tx') -> succ <$> formLength 0 tx'
    _ -> Nothing
formLength k tx =
  case bsrec tx of
    Just (')', tx') -> succ <$> formLength (k - 1) tx'
    Just (']', tx') -> succ <$> formLength (k - 1) tx'
    Just ('(', tx') -> succ <$> formLength (k + 1) tx'
    Just ('[', tx') -> succ <$> formLength (k + 1) tx'
    Just (_, tx') ->   succ <$> formLength k tx'
    _ -> Nothing


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

elabForm :: Parser (BS, (Bool, Form, Inf))
elabForm = do
  (nm, rl, f, Just (GenT "inference" [gt], Nothing)) <- anf
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
  return $ OrT nm []
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
gentParseInf (GenT "topf" [gt]) = do
  nm <- gentParseBS gt
  return $ AndF nm []
gentParseInf (GenT "andf" [gt, Genl gts]) = do
  nm <- gentParseBS gt
  nms <- mapM gentParseBS gts
  return $ AndF nm nms
gentParseInf (GenT "faf" [gth, Genn k, gtc]) = do
  nh <- gentParseBS gth
  nc <- gentParseBS gtc
  return $ FaF nh k nc
gentParseInf (GenT "ext" [gth, Genn k, gtc]) = do
  nh <- gentParseBS gth
  nc <- gentParseBS gtc
  return $ ExT nh k nc
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
  lit "cnf(" <|> lit "fof("
  n <- name
  lit ","
  r <- lowerWord
  lit ","
  f <- form
  a <- annotations
  lit ")"
  lit "."
  ign
  unit (n, r, f, a)

input :: Parser Input
input = (AnfInput <$> anf) <|> inc

runParser :: Parser a -> BS -> IO a
runParser p bs = do
  (rst, rem) <- cast $ parse p bs
  guard $ BS.null rem
  return rst

stext :: Parser BS
stext = Parser $
  \ tx ->
    case BS.break ((== '.') . w2c) tx of
      (pfx, _ :> sfx) -> Just (pfx, sfx)
      _ -> error "No full stop found"

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

proofCheck :: Bool -> Int -> Branch -> Bool -> Form -> Parser ()
proofCheck vb k bch sgn f = do
  nm <- stext
  let bch' = M.insert nm (sgn, f) bch
  r <- rule <|> error "cannot read rule"
  proofCheck' vb k bch' r

fetch :: (MonadFail m) => Branch -> BS -> m SignForm
fetch bch nm = cast (M.lookup nm bch)

pguard :: String -> Bool -> Parser ()
pguard tx True = skip
pguard tx False = error tx

sform :: Parser Form
sform = item >>= sform'

sterm :: Parser Term
sterm = item >>= sterm'

sterm' :: Char -> Parser Term
sterm' '$' = Var <$> stext
sterm' '@' = do
  f <- getFunct
  Fun f <$> slist sterm
sterm' _ = error "cannot get term"

slist :: Parser a -> Parser [a]
slist p = (char '.' >> unit []) <|>
  ( do char ','
       x <- p
       xs <- slist p
       unit (x : xs) )

getInt :: Parser Int
getInt = do
  tx <- stext
  cast $ bs2int tx

getFunct :: Parser Funct
getFunct = (char '#' >> (Idx <$> getInt)) <|> (Reg <$> stext)
sform' :: Char -> Parser Form
sform' '=' = do
  x <- sterm
  y <- sterm
  unit $ Eq x y
sform' '@' = do
  r <- getFunct
  Rel r <$> slist sterm
sform' '~' = Not <$> sform
sform' '|' = Or <$> slist sform
sform' '&' = And <$> slist sform
sform' '>' = do
  f <- sform
  g <- sform
  unit $ Imp f g
sform' '^' = do
  f <- sform
  g <- sform
  unit $ Iff f g
sform' '!' = do
  vs <- slist stext
  Fa vs <$> sform
sform' '?' = do
  vs <- slist stext
  Ex vs <$> sform
sform' c = error $ "invalid head character : " <> [c]

proofCheck' :: Bool -> Int -> Branch -> BS -> Parser ()
proofCheck' vb k bch "I" = do
  nt <- stext
  nf <- stext
  ft <- fetch bch nt
  ff <- fetch bch nf
  pguard "id-fail" $ complementary ft ff
proofCheck' vb k bch "C" = do
  f <- sform
  proofCheck vb k bch False f
  proofCheck vb k bch True f
proofCheck' vb k bch "D" = do
  f <- sform
  k' <- cast $ checkRelD k f
  proofCheck vb k' bch True f
proofCheck' vb k bch "A" = do
  x <- sterm
  f <- sform
  k' <- cast $ checkAoC k x f
  proofCheck vb k' bch True f
proofCheck' vb k bch "O" = skip
proofCheck' vb k bch "F" = do
  eqns <- slist stext >>= mapM (fetch bch)
  (False, Eq (Fun f xs) (Fun g ys)) <- stext >>= fetch bch
  pguard "function symbol mismatch" $ f == g
  xys <- cast $ mapM breakTrueEq eqns
  xys' <- zipM xs ys
  pguard "arguments mismatch" $ xys == xys'
proofCheck' vb k bch "R" = do
  eqns <- slist stext >>= mapM (fetch bch)
  (True, Rel r xs) <- stext >>= fetch bch
  (False, Rel s ys) <- stext >>= fetch bch
  pguard "relation symbol mismatch" $ r == s
  xys <- cast $ mapM breakTrueEq eqns
  xys' <- zipM xs ys
  pguard "arguments mismatch" $ xys == xys'
proofCheck' vb k bch "=R" = do
  (False, Eq x y) <- stext >>= fetch bch
  guard $ x == y
proofCheck' vb k bch "=S" = do
  (True, Eq x y) <- stext >>= fetch bch
  (False, Eq y' x') <- stext >>= fetch bch
  guard $ x == x' && y == y'
proofCheck' vb k bch "=T" = do
  (True, Eq x y) <- stext >>= fetch bch
  (True, Eq y' z) <- stext >>= fetch bch
  (False, Eq x' z') <- stext >>= fetch bch
  guard $ x == x' && y == y' && z == z'
proofCheck' vb k bch "~T" = do
  (True, Not f) <- stext >>= fetch bch
  proofCheck vb k bch False f
proofCheck' vb k bch "~F" = do
  (False, Not f) <- stext >>= fetch bch
  proofCheck vb k bch True f
proofCheck' vb k bch "|T" = do
  nm <- stext
  (True, Or fs) <- fetch bch nm
  mapM_ (proofCheck vb k bch True) fs
proofCheck' vb k bch "|F" = do
  (False, Or fs) <- stext >>= fetch bch
  m <- getInt
  f <- cast $ nth m fs
  proofCheck vb k bch False f
proofCheck' vb k bch "&T" = do
  (True, And fs) <- stext >>= fetch bch
  m <- getInt
  f <- cast $ nth m fs
  proofCheck vb k bch True f
proofCheck' vb k bch "&F" = do
  (False, And fs) <- stext >>= fetch bch
  mapM_ (proofCheck vb k bch False) fs
proofCheck' vb k bch ">T" = do
  (True, Imp f g) <- stext >>= fetch bch
  proofCheck vb k bch False f
  proofCheck vb k bch True g
proofCheck' vb k bch ">FA" = do
  nm <- stext
  (sgn, fg) <- fetch bch nm
  case (sgn, fg) of
    (False, Imp f g) -> proofCheck vb k bch True f
    _ -> error "not false imp"
proofCheck' vb k bch ">FC" = do
  (False, Imp _ g) <- stext >>= fetch bch
  proofCheck vb k bch False g
proofCheck' vb k bch "^TO" = do
  (True, Iff f g) <- stext >>= fetch bch
  proofCheck vb k bch True (f ==> g)
proofCheck' vb k bch "^TR" = do
  (True, Iff f g) <- stext >>= fetch bch
  proofCheck vb k bch True (g ==> f)
proofCheck' vb k bch "^F" = do
  (False, Iff f g) <- stext >>= fetch bch
  proofCheck vb k bch False (f ==> g)
  proofCheck vb k bch False (g ==> f)
proofCheck' vb k bch "!T" = do
  (True, Fa vs f) <- stext >>= fetch bch
  xs <- slist sterm
  vxs <- zipM vs xs
  let f' = substForm vxs f
  proofCheck vb k bch True f'
proofCheck' vb k bch "!F" = do
  (False, Fa vs f) <- stext >>= fetch bch
  m <- getInt
  guard $ k <= m
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "!F-fail : cannot zip"
  let f' = substForm vxs f
  proofCheck vb k' bch False f'
proofCheck' vb k bch "?T" = do
  (True, Ex vs f) <- stext >>= fetch bch
  m <- getInt
  pguard "index check failed for ?T" (k <= m)
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "?T-fail : cannot zip"
  let f' = substForm vxs f
  proofCheck vb k' bch True f'
proofCheck' vb k bch "?F" = do
  nm <- stext
  (False, Ex vs f) <- fetch bch nm
  xs <- slist sterm
  vxs <- zipM vs xs
  let f' = substForm vxs f
  proofCheck vb k bch False f'
proofCheck' vb k bch _ = error "impossible case"

proof :: Branch -> Bool -> Form -> Parser Proof
proof bch sgn f = do
  nm <- stext
  let bch' = M.insert nm (sgn, f) bch
  r <- rule <|> error "cannot read rule"
  proof' bch' (nm, sgn, f) r

(<$$>) :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
(<$$>) f g h = do 
  x <- g 
  y <- h 
  return (f x y)

proof' :: Branch -> Node -> BS -> Parser Proof
proof' bch ni "I" = (Id_ ni <$$> stext) stext
proof' bch ni "C" = do
  f <- sform
  pf <- proof bch False f
  pt <- proof bch True f
  return $ Cut_ ni f pf pt
proof' bch ni "D" = do
  f <- sform
  p <- proof bch True f
  return $ RelD_ ni f p
proof' bch ni "A" = do
  x <- sterm
  f <- sform
  p <- proof bch True f
  return $ AoC_ ni x f p
proof' bch ni "O" = return $ Open_ ni
proof' bch ni "F" = do
  nms <- slist stext
  nm <- stext
  return $ FunC_ ni nms nm
proof' bch ni "R" = do
  nms <- slist stext
  nt <- stext
  nf <- stext
  return $ RelC_ ni nms nt nf
proof' bch ni "=R" = do
  nf <- stext
  return $ EqR_ ni nf
proof' bch ni "=S" = do
  nt <- stext
  nf <- stext
  return $ EqS_ ni nt nf
proof' bch ni "=T" = do
  nxy <- stext
  nyz <- stext
  nxz <- stext
  return $ EqT_ ni nxy nyz nxz
proof' bch ni "~T" = do
  nm <- stext
  (True, Not f) <- fetch bch nm
  p <- proof bch False f
  return $ NotT_ ni nm p
proof' bch ni "~F" = do
  nm <- stext
  (False, Not f) <- fetch bch nm
  p <- proof bch True f
  return $ NotF_ ni nm p
proof' bch ni "|T" = do
  nm <- stext
  (True, Or fs) <- fetch bch nm
  ps <- mapM (proof bch True) fs
  return $ OrT_ ni nm ps
proof' bch ni "|F" = do
  nm <- stext
  (False, Or fs) <- fetch bch nm
  m <- getInt
  f <- cast $ nth m fs
  p <- proof bch False f
  return $ OrF_ ni nm m p
proof' bch ni "&T" = do
  nm <- stext
  (True, And fs) <- fetch bch nm
  m <- getInt
  f <- cast $ nth m fs
  p <- proof bch True f
  return $ AndT_ ni nm m p
proof' bch ni "&F" = do
  nm <- stext
  (False, And fs) <- fetch bch nm
  ps <- mapM (proof bch False) fs
  return $ AndF_ ni nm ps
proof' bch ni ">T" = do
  nm <- stext
  (True, Imp f g) <- fetch bch nm
  pa <- proof bch False f
  pc <- proof bch True g
  return $ ImpT_ ni nm pa pc
proof' bch ni ">FA" = do
  nm <- stext
  (False, Imp f _) <- fetch bch nm
  p <- proof bch True f
  return $ ImpFA_ ni nm p
proof' bch ni ">FC" = do
  nm <- stext
  (False, Imp _ g) <- fetch bch nm
  p <- proof bch False g
  return $ ImpFC_ ni nm p
proof' bch ni "^TO" = do
  nm <- stext
  (True, Iff f g) <- fetch bch nm
  p <- proof bch True (f ==> g)
  return $ IffTO_ ni nm p
proof' bch ni "^TR" = do
  nm <- stext
  (True, Iff f g) <- fetch bch nm
  p <- proof bch True (g ==> f)
  return $ IffTR_ ni nm p
proof' bch ni "^F" = do
  nm <- stext
  (False, Iff f g) <- fetch bch nm
  po <- proof bch False (f ==> g)
  pr <- proof bch False (g ==> f)
  return $ IffF_ ni nm po pr
proof' bch ni "!T" = do
  nm <- stext
  (True, Fa vs f) <- fetch bch nm
  xs <- slist sterm
  vxs <- zipM vs xs
  let f' = substForm vxs f
  p <- proof bch True f'
  return $ FaT_ ni nm xs p
proof' bch ni "!F" = do
  nm <- stext
  (False, Fa vs f) <- fetch bch nm
  m <- getInt
  let (_, vxs) = varPars m vs
  let f' = substForm vxs f
  p <- proof bch False f'
  return $ FaF_ ni nm m p
proof' bch ni "?T" = do
  nm <- stext
  (True, Ex vs f) <- fetch bch nm
  m <- getInt
  let (_, vxs) = varPars m vs
  let f' = substForm vxs f
  p <- proof bch True f'
  return $ ExT_ ni nm m p
proof' bch ni "?F" = do
  nm <- stext
  (False, Ex vs f) <- fetch bch nm
  xs <- slist sterm
  vxs <- zipM vs xs
  let f' = substForm vxs f
  p <- proof bch False f'
  return $ ExF_ ni nm xs p
proof' bch ni _ = error "invalid rule"