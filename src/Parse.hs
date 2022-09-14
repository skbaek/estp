{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Types
import Basic ( et, cast, readInt )

import Data.Text as T
    ( cons, drop, isPrefixOf, length, null, pack, uncons, Text )
import Data.Char (isDigit, isLower, isUpper, isAlphaNum)
import Data.List (elem, foldl, map, (\\))
import Control.Applicative (Alternative, empty, (<|>))
import Data.Text.IO as TIO
import System.Environment (getEnv)
import Data.Text (unpack)

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

connective :: Parser Text
connective =
  litRet "," <|>
  litRet ")" <|>
  litRet "&" <|>
  litRet "|" <|>
  litRet "<=>" <|>
  litRet "<~>" <|>
  litRet "=>" <|>
  litRet "<=" <|>
  litRet "=" <|>
  litRet "!="

annotations :: Parser Ant
annotations =
  do { lit "." ; unit Nothing } <|>
  do { t <- generalTerm ; lit ")" ; lit "." ; unit $ Just t }

junction :: Bool -> Parser [Form]
junction b = do
  f <- lform
  c <- connective
  case (b , c) of
    (_, ",") -> unit [f]
    (_, ")") -> unit [f]
    (True,  "&") -> do { fs <- junction b ; unit $ f : fs }
    (False, "|") -> do { fs <- junction b ; unit $ f : fs }
    _   -> failure

gformClose :: Form -> Text -> Parser Form
gformClose f "," = unit f
gformClose f ")" = unit f
gformClose f "|" = do { fs <- junction False ; unit (Or $ f : fs) }
gformClose f "&" = do { fs <- junction True ; unit (And $ f : fs) }
gformClose f "=>"  = do { g <- lform ; delimiter ; unit $ Imp f g }
gformClose f "<="  = do { g <- lform ; delimiter ; unit $ Imp g f }
gformClose f "<=>" = do { g <- lform ; delimiter ; unit $ Iff f g }
gformClose f "<~>" = do { g <- lform ; delimiter ; unit $ Not $ Iff f g }
gformClose _ _ = failure

parenLform :: Parser Form
parenLform = do
  lit "("
  f <- gform
  unit f

notLform :: Parser Form
notLform = do
  lit "~"
  f <- lform
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

faLform :: Parser Form
faLform = do
  lit "!"
  vs <- vars
  lit ":"
  f <- lform
  unit $ Fa vs f

exLform :: Parser Form
exLform = do
  lit "?"
  vs <- vars
  lit ":"
  f <- lform
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

generalTerm :: Parser Gterm
generalTerm =
  do { lit "[" ; ts <- commaSepStar generalTerm ; lit "]" ; unit (Glist ts) } <|>
  do { f <- functor ; ts <- gargs ; unit $ Gfun f ts } <|>
  do { kt <- integer ; cast (readInt kt) >>= (unit . Gnum) }

termInfixOpLform :: Term -> Text -> Parser Form
termInfixOpLform t "=" = do
  s <- term
  unit $ Eq t s
termInfixOpLform t "!=" = do
  s <- term
  unit $ Not $ Eq t s
termInfixOpLform _ _ = failure

termToAtom :: Term -> Parser Form
termToAtom (Fun r ts) = unit $ Rel r ts
termToAtom _ = failure

termLform :: Term -> Parser Form
termLform t = cutParse infixOp (termInfixOpLform t) (termToAtom t)

verum :: Parser Form
verum = lit "$true" >> unit (And [])

falsum :: Parser Form
falsum = lit "$false" >> unit (Or [])

lform :: Parser Form
lform = verum <|> falsum <|> parenLform <|> notLform <|> faLform <|> exLform <|> (term >>= termLform)

gform :: Parser Form
gform = do
  f <- lform
  c <- connective
  gformClose f c

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
  f <- univClose <$> gform
  a <- annotations
  ign
  unit (Cnf n (conjecturize r f) a)

fof :: Parser Input
fof = do
  lit "fof("
  n <- name
  lit ","
  r <- lowerWord
  lit ","
  f <- gform
  a <- annotations
  ign
  unit (Fof n (conjecturize r f) a)

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

parseInput :: Input -> IO [AnForm]
parseInput (Inc s) = do
  tptp <- getEnv "TPTP"
  parseName $ tptp ++ "/" ++ unpack s
parseInput (Cnf n f t) = return [Af n f t]
parseInput (Fof n f t) = return [Af n f t]

parseText :: Text -> IO [AnForm]
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

parseName :: String -> IO [AnForm]
parseName n = do
  t <- TIO.readFile n
  case parse ign t of
    Just (i,s) -> parseText s
    _ -> ioError $ userError "Read filename, but failed to parse content"