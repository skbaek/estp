{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Types
import Basic ( et, cast, readInt, unquote )

import Data.Text.Lazy as T
    ( cons, drop, isPrefixOf, length, null, pack, uncons, Text, unpack )
import Data.Char (isDigit, isLower, isUpper, isAlphaNum)
import Data.List (elem, foldl, map, (\\))
import Control.Applicative (Alternative, empty, (<|>))
import Data.Text.Lazy.IO as TIO
import System.Environment (getEnv)

newtype Parser a = Parser { parse :: Text -> Maybe (a, Text) }

item :: Parser Char
item = Parser uncons

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \ s ->
  case parse p s of
    Nothing -> Nothing
    Just (x,t) -> parse (f x) t

unit :: a -> Parser a
unit a = Parser (\s -> Just (a,s))

skip :: Parser ()
skip = unit ()

instance Functor Parser where
  fmap f (Parser cs) = Parser $ \ s ->
    case cs s of
      Nothing -> Nothing
      Just (x, t) -> Just (f x, t)

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser $ \ s ->
    case cs1 s of
      Nothing -> Nothing
      Just (f,t) ->
        case cs2 t of
          Nothing -> Nothing
          Just (x,r) -> Just (f x, r)

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance Alternative Parser where
  empty = failure
  (<|>) = option

instance MonadFail Parser where
  fail _ = failure

failure :: Parser a
failure = Parser (const Nothing)

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    Nothing -> parse q s
    res     -> res

cutParse :: Parser a -> (a -> Parser b) -> Parser b -> Parser b
cutParse p q r = Parser $ \ s ->
  case parse p s of
    Nothing -> parse r s
    Just (x,t) -> parse (q x) t

commitParse :: Parser a -> Parser b -> Parser b -> Parser b
commitParse p q = cutParse p (const q)

-- | One or more.
some :: Alternative f => f a -> f [a]
some v = some_v
  where
    star_v = some_v <|> pure []
    some_v = (:) <$> v <*> star_v

-- | Zero or more.
star :: Alternative f => f a -> f [a]
star v = star_v
  where
    star_v = some_v <|> pure []
    some_v = (:) <$> v <*> star_v

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

peak :: Text -> Parser ()
peak s = Parser $ \ t ->
  if T.isPrefixOf s t
    then Just ((), t)
    else Nothing

string :: Text -> Parser Text
string s = Parser $ \ t ->
  if T.isPrefixOf s t
    then Just (s, T.drop (T.length s) t)
    else Nothing

notText :: Text -> Parser ()
notText s = Parser $ \ t ->
  case parse (string s) t of
    Nothing -> Just ((),t)
    _  -> Nothing

litRet :: Text -> Parser Text
litRet s = lit s >> unit s

lit :: Text -> Parser ()
lit s = string s >> ws

space :: Parser ()
space = oneOf " \n\r" >> nil

ws :: Parser ()
ws = star space >> nil

digit :: Parser Char
digit = satisfy isDigit

nil :: Parser ()
nil = Parser $ \ s -> Just (() , s)

eof :: Parser ()
eof = Parser  $ \ t ->
  if T.null t
    then Just (() , t)
    else Nothing

plus :: Parser a -> Parser [a]
plus p = do
  x <- p
  xs <- star p
  unit (x : xs)

newline :: Parser ()
newline = char '\n' >> nil

untilChar :: Char -> Parser ()
untilChar c = (char c >> nil) <|> (item >> untilChar c)

untilNlOrEof :: Parser ()
untilNlOrEof = eof <|> untilChar '\n' <|> (item >> untilNlOrEof)

comment :: Parser ()
comment = char '%' >> untilNlOrEof -- untilChar '\n' 

blockCommentEnd :: Parser ()
blockCommentEnd = (string "*/" >> nil) <|> (item >> blockCommentEnd)

blockComment :: Parser ()
blockComment = string "/*" >> blockCommentEnd

ignOnce :: Parser ()
ignOnce = space <|> comment <|> blockComment

ign :: Parser ()
ign = star ignOnce >> nil

lowerAlpha :: Parser Char
lowerAlpha = satisfy isLower

upperAlpha :: Parser Char
upperAlpha = satisfy isUpper

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isAlphaNum c || c == '_'

alphaNumeric :: Parser Char
alphaNumeric = satisfy isAlphaNumeric

lowerWord :: Parser Text
lowerWord = do
  c <- lowerAlpha
  cs <- star alphaNumeric
  ign
  unit $ pack $ c : cs

upperWord :: Parser Text
upperWord = do
  c <- upperAlpha
  cs <- star alphaNumeric
  ign
  unit $ pack $ c : cs

atomicWord :: Parser Text
atomicWord = lowerWord <|> singleQuoted

name :: Parser Text
name = atomicWord <|> integer

sign :: Parser Char
sign = char '-' <|> char '+'

integer :: Parser Text
integer = signedInteger <|> decimal

signedInteger :: Parser Text
signedInteger = do
  c <- sign
  t <- decimal
  unit $ T.cons c t

nonZeroNumeric :: Parser Char
nonZeroNumeric = oneOf ['1', '2', '3', '4', '4', '5', '6', '7', '8', '9']

numeric :: Parser Char
numeric = char '0' <|> nonZeroNumeric

positiveDecimal :: Parser Text
positiveDecimal = do
  c <- nonZeroNumeric
  cs <- star numeric
  unit $ pack $ c : cs

decimal :: Parser Text
decimal = litRet "0" <|> positiveDecimal

isUnescapedSqChar :: Char -> Bool
isUnescapedSqChar c = c /= '\'' && c /= '\\'

sqChar :: Parser Char
sqChar = satisfy isUnescapedSqChar <|> (char '\\' >> char '\'' <|> char '\\')

isDoChar :: Char -> Bool
isDoChar c = c /= '"' && c /= '\\'

singleQuoted :: Parser Text
singleQuoted = do
  char '\''
  s <- plus sqChar
  char '\''
  ws
  unit $ "'" <> pack s <> "'"

distinctObject :: Parser Text
distinctObject = do
  char '"'
  s <- plus $ satisfy isDoChar
  char '"'
  ws
  unit $ pack $  "\"" ++ s ++ "\""

delimiter :: Parser ()
delimiter = lit ")" <|> lit ","

peakDelimiter :: Parser ()
peakDelimiter = peak ")" <|> peak ","

connective :: Parser Text
connective =
  -- litRet "," <|>
  -- litRet ")" <|>
  litRet "&" <|>
  litRet "|" <|>
  litRet "<=>" <|>
  litRet "<~>" <|>
  litRet "=>" <|>
  litRet "<=" <|>
  litRet "=" <|>
  litRet "!="

usefulInfo :: Parser (Maybe [Gterm])
usefulInfo = 
  (peak ")" >> unit Nothing) 
    <|> ( do lit "," 
             ts <- generalTermList
             unit $ Just ts )

annotations :: Parser Ant
annotations =
  do { peak ")" ; unit Nothing } <|>
  do { 
    lit "," ;
    t <- generalTerm ; 
    u <- usefulInfo ; 
    unit $ Just (t, u)
  }

conjunction :: Parser [Form]
conjunction = do
  f <- formLazy
  (peakDelimiter >> unit [f]) 
    <|> (do lit "&" 
            fs <- conjunction 
            unit (f : fs) )

disjunction :: Parser [Form]
disjunction = do
  f <- formLazy
  (peakDelimiter >> unit [f]) 
    <|> (do lit "|" 
            fs <- disjunction 
            unit (f : fs) )

formClose :: Form -> Text -> Parser Form
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

var :: Parser Text
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

vars :: Parser [Text]
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

infixOp :: Parser Text
infixOp = (notText "=>" >> litRet "=") <|> litRet "!="

functor :: Parser Text
functor = atomicWord <|> do { lit "$" ; w <- atomicWord ; unit $ cons '$' w } <|> distinctObject

arguments :: Parser [Term]
arguments = do { lit "(" ; ts <- terms ; lit ")" ; unit ts } <|> (ws >> unit [])

gargs :: Parser [Gterm]
gargs = do { lit "(" ; ts <- commaSepPlus generalTerm ; lit ")" ; unit ts } <|> (ws >> unit [])

terms :: Parser [Term]
terms = commaSepPlus term

term :: Parser Term
term = (upperWord >>= unit . Var) <|> do { f <- functor ; ts <- arguments ; unit $ Fun f ts }

generalTermList :: Parser [Gterm]
generalTermList = do 
  lit "[" 
  ts <- commaSepStar generalTerm ;
  lit "]" 
  unit ts

generalTerm :: Parser Gterm
generalTerm =
  do { ts <- generalTermList ; unit (Glist ts) } <|>
  do { f <- functor ; ts <- gargs ; unit $ Gfun f ts } <|>
  do { kt <- integer ; cast (readInt kt) >>= (unit . Gnum) } <|>
  do { v <- upperWord ; unit (Gvar v) }

termInfixOpformLazy :: Term -> Text -> Parser Form
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
formLazy = verum <|> falsum <|> parenFormLazy <|> notformLazy <|> faformLazy <|> exformLazy <|> (term >>= termformLazy)

form :: Parser Form
form = do
  f <- formLazy
  (peakDelimiter >> return f) <|> (connective >>= formClose f) 

inc :: Parser Input
inc = do
  lit "include("
  w <- singleQuoted
  lit ")"
  lit "."
  ign
  unit (Inc w)

cnf :: Parser Input
cnf = do
  lit "cnf("
  n <- name
  lit ","
  r <- lowerWord
  lit ","
  f <- univClose <$> form
  a <- annotations
  lit ")"
  lit "."
  ign
  unit (Cnf n r (conjecturize r f) a)

fof :: Parser Input
fof = do
  lit "fof("
  n <- name
  lit ","
  r <- lowerWord
  lit ","
  f <- form
  a <- annotations
  lit ")"
  lit "."
  ign
  unit (Fof n r (conjecturize r f) a)

input :: Parser Input
input = cnf <|> fof <|> inc

prob :: Parser Prob
prob = star input

runParser :: Parser a -> Text -> a
runParser m s =
  case parse m s of
    Just (res, empty) -> res
    _                 -> error "Parser error."

run :: Text -> Prob
run = runParser (ign >> prob)

univClose :: Form -> Form
univClose f =
  case formBvs f of
    [] -> f
    vs -> Fa vs f

conjecturize :: Text -> Form -> Form
conjecturize "conjecture" = Not
conjecturize _ = id

mergeVars :: [Text] -> [Text] -> [Text]
mergeVars vs ws = vs ++ (ws \\ vs)

termBvs :: Term -> [Text]
-- termBvs (Par _) = []
termBvs (Var v) = [v]
termBvs (Fun _ ts) = foldl mergeVars [] (map termBvs ts)

formBvs :: Form -> [Text]
formBvs (Rel _ ts) = foldl mergeVars [] (map termBvs ts)
formBvs (Eq t s) = mergeVars (termBvs t) (termBvs s)
formBvs (Not f) = formBvs f
formBvs (And fs) = foldl mergeVars [] (map formBvs fs)
formBvs (Or  fs) = foldl mergeVars [] (map formBvs fs)
formBvs (Imp f g) = mergeVars (formBvs f) (formBvs g)
formBvs (Iff f g) = mergeVars (formBvs f) (formBvs g)
formBvs (Fa vs f) = vs ++ (formBvs f \\ vs)
formBvs (Ex vs f) = vs ++ (formBvs f \\ vs)

parseInput :: Input -> IO [AF]
parseInput (Inc s) = do
  tptp <- getEnv "TPTP"
  s' <- cast $ unquote s
  parseName $ tptp ++ "/" ++ unpack s'
parseInput (Cnf n r f t) = return [(n, r, f, t)]
parseInput (Fof n r f t) = return [(n, r, f, t)]

parseText :: Text -> IO [AF]
parseText t =
  case parse input t of
    Just (i,s) -> do
      pfx <- parseInput i
      if T.null s
      then return pfx
      else do
        sfx <- parseText s
        return (pfx ++ sfx)
    _ -> et ("Failed to parse input : " <> t)

parseName :: String -> IO [AF]
parseName n = do
  t <- TIO.readFile n
  case parse ign t of
    Just (i,s) -> parseText s
    _ -> ioError $ userError "Read filename, but failed to parse content"