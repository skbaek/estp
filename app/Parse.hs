{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Types
import Basic -- ( et, cast, readInt, unquote )

import Data.Text.Lazy as T
    ( cons, drop, isPrefixOf, length, null, pack, uncons, unsnoc, Text, unpack, take, splitAt, splitOn, break )
import Data.Char (isDigit, isLower, isUpper, isAlphaNum)
import Data.List (elem, foldl, map, sortBy, (\\))
import Control.Applicative (Alternative, empty, (<|>))
import Data.Text.Lazy.IO as TIO ( readFile )
import System.Environment (getEnv, unsetEnv)
import Control.Monad as M ( MonadPlus(mzero), guard, when, mzero )
import Data.Map as HM (lookup, insert)
import Data.Functor ((<&>))
import Debug.Trace (trace)

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

regFunctor :: Parser Funct
regFunctor = do
  tx <- atomicWord <|> do { lit "$" ; w <- atomicWord ; unit $ cons '$' w } <|> distinctObject
  return $ Reg tx

idxFunctor :: Parser Funct
idxFunctor = do
  -- ('#' :> tx) <- singleQuoted
  tx <- singleQuoted
  ('#' :> tx') <- cast $ unsq tx
  k <- cast $ readInt tx'
  return $ Idx k

functor :: Parser Funct
-- functor = atomicWord <|> do { lit "$" ; w <- atomicWord ; unit $ cons '$' w } <|> distinctObject
functor = idxFunctor <|> regFunctor 
 -- do { tx <- regFunctor ; return $ Reg tx } <|> do {}

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
  do { f <- atomicWord ; ts <- gargs ; unit $ Gfun f ts } <|>
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
  (eof >> return f) <|> (peakDelimiter >> return f) <|> (connective >>= formClose f)

preInc :: Parser PreInput
preInc = do
  lit "include("
  w <- singleQuoted
  lit ")"
  lit "."
  ign
  unit (PreInc w)

inc :: Parser Input
inc = do
  lit "include("
  w <- singleQuoted
  lit ")"
  lit "."
  ign
  unit (Inc w)

preFof :: Parser PreInput
preFof = do
  lit "fof("
  n <- name
  lit ","
  r <- lowerWord
  lit ","
  ft <- formText
  lit ")"
  lit "."
  ign
  unit (PreFof n r ft)


formLength :: Int -> Text -> Maybe Int
formLength 0 tx =
  case uncons tx of
    Just (')', tx') -> Just 0
    Just ('(', tx') -> succ <$> formLength 1 tx'
    Just (_, tx') -> succ <$> formLength 0 tx'
    _ -> Nothing
formLength k tx =
  case uncons tx of
    Just (')', tx') -> succ <$> formLength (k - 1) tx'
    Just ('(', tx') -> succ <$> formLength (k + 1) tx'
    Just (_, tx') ->   succ <$> formLength k tx'
    _ -> Nothing

formTextCore :: Text -> Maybe (Text, Text)
formTextCore tx = do
  k <- formLength 0 tx
  return $ T.splitAt (fromIntegral k) tx

formText :: Parser Text
formText = Parser formTextCore

preCnf :: Parser PreInput
preCnf = do
  lit "cnf("
  n <- name
  lit ","
  r <- lowerWord
  lit ","
  ft <- formText
  lit ")"
  lit "."
  ign
  unit (PreCnf n r ft)

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

preInput :: Parser PreInput
preInput = preCnf <|> preFof <|> preInc

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
-- conjecturize "conjecture" (Not f) = f
conjecturize "conjecture" f = Not f
conjecturize _ f = f

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

parsePreInput :: PreInput -> IO [PreAF]
parsePreInput (PreInc s) = do
  tptp <- getEnv "TPTP"
  s' <- cast $ unquote s
  parsePreName $ tptp ++ "/" ++ unpack s'
parsePreInput (PreCnf n r f) = return [CnfAF n r f]
parsePreInput (PreFof n r f) = return [FofAF n r f]

parseInput :: Input -> IO [AF]
parseInput (Inc s) = do
  tptp <- getEnv "TPTP"
  s' <- cast $ unquote s
  parseName $ tptp ++ "/" ++ unpack s'
parseInput (Cnf n r f t) = return [(n, r, f, t)]
parseInput (Fof n r f t) = return [(n, r, f, t)]


parsePreText :: Text -> IO [PreAF]
parsePreText t =
  case parse preInput t of
    Just (i,s) -> do
      pfx <- parsePreInput i
      if T.null s
      then return pfx
      else do
        sfx <- parsePreText s
        return (pfx ++ sfx)
    _ -> et ("Failed to parse input : " <> t)

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



parsePreName :: String -> IO [PreAF]
parsePreName n = do
  t <- TIO.readFile n
  case parse ign t of
    Just (i,s) -> parsePreText s
    _ -> ioError $ userError "Read filename, but failed to parse content"

parseForm :: Text -> Form
parseForm tx =
  case parse form tx of
    Just (f, tx') -> if T.null tx' then f else et ("parse-form failed, case 1 : " <> tx)
    _ -> et $ "parse-form failed, case 2 : " <> tx

parseName :: String -> IO [AF]
parseName n = do
  pt "Reading file as text...\n"
  t <- TIO.readFile n
  pt "Parsing the text read...\n"
  case parse ign t of
    Just (i,s) -> parseText s
    _ -> ioError $ userError "Read filename, but failed to parse content"

afToEf :: AF -> IO Elab
afToEf (nm, sgn, f, Just (Gfun "inference" [gt], gts)) = do
  pl <- textToBool sgn
  -- ep <- cast $ readEp nm
  i <- gTermToInf gt
  mtx <- gTermsToMaybeText gts
  return ((nm, pl, f), i, mtx)
afToEf af = et "cannot read AF into Elab" -- <> tlt (fmtAF af)

textToBool :: Text -> IO Bool
textToBool "true" = return bt
textToBool "false" = return bf
textToBool _ = et "Cannot read Boolarity"

gTermsToMaybeText :: Maybe [Gterm] -> IO (Maybe Text)
gTermsToMaybeText Nothing = return nt
gTermsToMaybeText (Just [Gfun tx []]) = return $ Just tx
gTermsToMaybeText _ = et "Cannot extact maybe text"

gTermToText :: Gterm -> IO Text
gTermToText (Gfun t []) = return t
gTermToText _ = mzero

textToIdxFunct :: Text -> IO Funct
textToIdxFunct tx = do
  ('#' :> tx') <- cast $ unsq tx
  cast (Idx <$> readInt tx')

textToFunct :: Text -> IO Funct
textToFunct tx = textToIdxFunct tx <|> return (Reg tx)

gTermToTerm :: Gterm -> IO Term
gTermToTerm (Gfun tx ts) = do
  f <- textToFunct tx
  xs <- mapM gTermToTerm ts
  return $ Fun f xs
gTermToTerm (Gvar v) = return $ Var v
gTermToTerm _ = mzero

gTermToInf :: Gterm -> IO Inf
gTermToInf (Gfun "cut" [gtf, gtt]) = do
  nf <- gTermToText gtf
  nt <- gTermToText gtt
  return $ Cut nf nt

gTermToInf (Gfun "id" [gt0, gt1]) = do
  m <- gTermToText gt0
  n <- gTermToText gt1
  return $ Id m n

gTermToInf (Gfun "iffto" [gth, gtc]) = do
  nh <- gTermToText gth
  nc <- gTermToText gtc
  return $ IffTO nh nc

gTermToInf (Gfun "ifftr" [gth, gtc]) = do
  nh <- gTermToText gth
  nc <- gTermToText gtc
  return $ IffTR nh nc

gTermToInf (Gfun "ifff" [gth, gt0, gt1]) = do
  nh <- gTermToText gth
  n0 <- gTermToText gt0
  n1 <- gTermToText gt1
  return $ IffF nh n0 n1

gTermToInf (Gfun "impfa" [gth, gtc]) = do
  nh <- gTermToText gth
  nc <- gTermToText gtc
  return $ ImpFA nh nc
gTermToInf (Gfun "impfc" [gth, gtc]) = do
  nh <- gTermToText gth
  nc <- gTermToText gtc
  return $ ImpFC nh nc

gTermToInf (Gfun "impt" [gth, gt0, gt1]) = do
  nh <- gTermToText gth
  n0 <- gTermToText gt0
  n1 <- gTermToText gt1
  return $ ImpT nh n0 n1

gTermToInf (Gfun "bott" [gt]) = do
  nm <- gTermToText gt
  return $ OrT nm []

gTermToInf (Gfun "ort" [gt, Glist gts]) = do
  nm <- gTermToText gt
  nms <- mapM gTermToText gts
  return $ OrT nm nms

gTermToInf (Gfun "orf" [gth, Gnum k, gtc]) = do
  nh <- gTermToText gth
  nc <- gTermToText gtc
  return $ OrF nh k nc

gTermToInf (Gfun "andt" [gth, Gnum k, gtc]) = do
  nh <- gTermToText gth
  nc <- gTermToText gtc
  return $ AndT nh k nc

gTermToInf (Gfun "topf" [gt]) = do
  nm <- gTermToText gt
  return $ AndF nm []

gTermToInf (Gfun "andf" [gt, Glist gts]) = do
  nm <- gTermToText gt
  nms <- mapM gTermToText gts
  return $ AndF nm nms

gTermToInf (Gfun "faf" [gth, Gnum k, gtc]) = do
  nh <- gTermToText gth
  nc <- gTermToText gtc
  return $ FaF nh k nc

gTermToInf (Gfun "ext" [gth, Gnum k, gtc]) = do
  nh <- gTermToText gth
  nc <- gTermToText gtc
  return $ ExT nh k nc

gTermToInf (Gfun "fat" [gth, Glist gts, gtc]) = do
  nh <- gTermToText gth
  xs <- mapM gTermToTerm gts
  nc <- gTermToText gtc
  return $ FaT nh xs nc

gTermToInf (Gfun "exf" [gth, Glist gts, gtc]) = do
  nh <- gTermToText gth
  xs <- mapM gTermToTerm gts
  nc <- gTermToText gtc
  return $ ExF nh xs nc

gTermToInf (Gfun "nott" [gth, gtc]) = do
  nh <- gTermToText gth
  nc <- gTermToText gtc
  return $ NotT nh nc

gTermToInf (Gfun "notf" [gth, gtc]) = do
  nh <- gTermToText gth
  nc <- gTermToText gtc
  return $ NotF nh nc

gTermToInf (Gfun "eqr" [gt]) = EqR <$> gTermToText gt
gTermToInf (Gfun "eqs" gts) = do
  [nm0, nm1] <- mapM gTermToText gts
  return $ EqS nm0 nm1
gTermToInf (Gfun "eqt" gts) = do
  [nm0, nm1, nm2] <- mapM gTermToText gts
  return $ EqT nm0 nm1 nm2
gTermToInf (Gfun "func" [Glist gts, gt]) = do
  nms <- mapM gTermToText gts
  nm <- gTermToText gt
  return $ FunC nms nm
gTermToInf (Gfun "relc" [Glist gts, gt0, gt1]) = do
  nms <- mapM gTermToText gts
  m <- gTermToText gt0
  n <- gTermToText gt1
  return $ RelC nms m n
gTermToInf (Gfun "aoc" [gtx, gtn]) = do
  x <- gTermToTerm gtx
  nm <- gTermToText gtn
  return $ AoC x nm

gTermToInf (Gfun "reld" [gt]) = RelD <$> gTermToText gt
gTermToInf (Gfun "open" []) = return Open
gTermToInf t = et $ "inf reader : " <> pack (show t)

readEpAux :: Text -> Maybe (Int, Int)
readEpAux t =
  case T.splitOn "." t of
    [t0, t1] -> do
      k <- cast $ readInt t0
      m <- cast $ readInt t1
      return (k, m)
    _ -> mzero

unsq :: Text -> Maybe Text
unsq ('\'' :> t) = do
  (t', '\'') <- T.unsnoc t
  return t'
unsq _ = mzero

-- readEp :: Text -> Maybe EP
-- readEp t = do
--   (t' : ts') <- T.splitOn ":" <$> unsq t
--   -- (ts', t') <- cast $ Main.unsnoc ts
--   k <- cast $ readInt t'
--   l <- mapM readEpAux ts'
--   return (k, l)

estpToElabs :: String -> IO [Elab]
estpToElabs estp = do 
  pt "Reading ESTP file...\n"
  xs <- parseName estp 
  pt "Transcribing AFs to EFs...\n"
  mapM afToEf xs

afToStep :: AF -> IO Step
afToStep (n, _, g, Just (Gfun "file" [_, Gfun m []], _)) = return (n, "file", [m], g)
afToStep (n, _, g, Just (Gfun "introduced" [Gfun "predicate_definition_introduction" [],
  Glist [Gfun "new_symbols" [Gfun "naming" [],Glist [Gfun r []]]]], _)) =
    return (n, "predicate_definition_introduction", [], g)
afToStep (n, _, g, Just (Gfun "introduced" [Gfun "avatar_definition" [],
  Glist [Gfun "new_symbols" [Gfun "naming" [], Glist [Gfun r []]]]], _)) =
    return (n, "avatar_definition", [], g)
afToStep (n, _, g, Just (Gfun "introduced" [Gfun "choice_axiom" [], Glist []], _)) =
  return (n, "choice_axiom", [], g)
afToStep (n, _, g, Just (Gfun "inference" [Gfun "avatar_sat_refutation" [], _, Glist l], _)) = do
  txs <- cast (mapM gFunFunctor l)
  return (n, "avatar_sat_refutation", txs, g)
afToStep (n, _, g, Just (Gfun "inference" [Gfun r [], _, Glist l], _)) = do
  txs <- cast (mapM gFunFunctor l)
  return (n, r, txs, g)
afToStep _ = error "AF-to-step failure"

sortAfs :: [AF] -> [AF]
sortAfs = sortBy compareAfs

compareAfs :: AF -> AF -> Ordering
compareAfs (m :> ms, _, _, _) (n :> ns, _, _, _) =
  case compare m n of
    EQ ->
      case (readInt ms, readInt ns) of
        (Just i, Just j) -> compare i j
        _ -> et "Cannot compare step names"
    other -> other
compareAfs _ _ = LT

gFunFunctor :: Gterm -> Maybe Text
gFunFunctor (Gfun t []) = return t
gFunFunctor _ = Nothing

tstpToSteps :: String -> IO [Step]
tstpToSteps tstp = parseName tstp >>= mapM afToStep . sortAfs
-- parseNameToElabs :: String -> IO [Elab]

-- proof :: Parser Proof
-- proof = do 
--   nm <- getText 
--   -- when vb $ trace ("Node name : " ++ unpack nm ++ "\n") skip
--   -- let bch' = HM.insert nm (sgn, f) bch
--   r <- getRule <|> error "cannot read rule"
--   proof' r
-- 
pcheck :: Bool -> Int -> Branch -> Bool -> Form -> Parser ()
pcheck vb k bch sgn f = do 
  when vb $ trace "Getting node name...\n" skip
  nm <- getText 
  when vb $ trace ("Node name : " ++ unpack nm ++ "\n") skip
  let bch' = HM.insert nm (sgn, f) bch
  r <- getRule <|> error "cannot read rule"
  when vb $ trace ("Rule : " ++ T.unpack r ++ "\n") skip
  pcheck' vb k bch' r

pguard :: Text -> Bool -> Parser ()
pguard tx True = skip
pguard tx False = et tx

getText :: Parser Text
getText = Parser $ 
  \ tx -> 
    case T.break (== '.') tx of 
      (pfx, _ :> sfx) -> Just (pfx, sfx)
      _ -> error "No full stop found"

getInt :: Parser Int
getInt = do 
  tx <- getText 
  cast $ readInt tx

getFunct :: Parser Funct
getFunct = (char '#' >> (Idx <$> getInt)) <|> (Reg <$> getText)

getList :: Parser a -> Parser [a]
getList p = (char '.' >> unit []) <|> 
  ( do char ','
       x <- p 
       xs <- getList p 
       unit (x : xs) )

getSign :: Parser Bool
getSign = (lit "T" >> return True) <|> (lit "F" >> return False)

getRule :: Parser Text
getRule = 
  litRet "O" <|>
  litRet "I" <|>
  litRet "C" <|>
  litRet "D" <|>
  litRet "A" <|>
  litRet "F" <|>
  litRet "R" <|>
  litRet "=R" <|>
  litRet "=S" <|>
  litRet "=T" <|>
  litRet "~T" <|>
  litRet "~F" <|>
  litRet "|T" <|>
  litRet "|F" <|>
  litRet "&T" <|>
  litRet "&F" <|>
  litRet ">T" <|>
  litRet ">FA" <|>
  litRet ">FC" <|>
  litRet "^TO" <|>
  litRet "^TR" <|>
  litRet "^F" <|>
  litRet "!T" <|>
  litRet "!F" <|>
  litRet "?T" <|>
  litRet "?F" 

getTerms :: Parser [Term]
getTerms = getList getTerm

getTerm :: Parser Term
getTerm = item >>= getTerm'

getTerm' :: Char -> Parser Term
getTerm' '$' = Var <$> getText 
getTerm' '@' = do
  f <- getFunct 
  Fun f <$> getTerms 
getTerm' _ = et "cannot get term"

getForms :: Parser [Form]
getForms = getList getForm

getForm :: Parser Form
getForm = item >>= getForm'

getForm' :: Char -> Parser Form
getForm' '=' = do 
  x <- getTerm
  y <- getTerm
  unit $ Eq x y
getForm' '@' = do 
  r <- getFunct 
  Rel r <$> getTerms 
getForm' '~' = Not <$> getForm
getForm' '|' = Or <$> getForms
getForm' '&' = And <$> getForms
getForm' '>' = do 
  f <- getForm
  g <- getForm
  unit $ Imp f g
getForm' '^' = do 
  f <- getForm
  g <- getForm
  unit $ Iff f g
getForm' '!' = do 
  vs <- getList getText 
  Fa vs <$> getForm 
getForm' '?' = do 
  vs <- getList getText 
  Ex vs <$> getForm 
getForm' c = error $ "invalid head character : " <> [c]

fetch :: (MonadFail m) => Branch -> Text -> m SignForm
fetch bch nm = cast (HM.lookup nm bch)

pcheck' :: Bool -> Int -> Branch -> Text -> Parser ()
pcheck' vb k bch "I" = do
  nt <- getText 
  nf <- getText 
  ft <- fetch bch nt
  ff <- fetch bch nf
  pguard "id-fail" $ complementary ft ff
pcheck' vb k bch "C" = do
  f <- getForm 
  pcheck vb k bch False f
  pcheck vb k bch True f
pcheck' vb k bch "D" = do
  f <- getForm 
  k' <- cast $ checkRelD k f 
  pcheck vb k' bch True f 
pcheck' vb k bch "A" = do
  x <- getTerm 
  f <- getForm 
  k' <- cast $ checkAoC k x f
  pcheck vb k' bch True f 
pcheck' vb k bch "O" = skip
pcheck' vb k bch "F" = do 
  eqns <- getList getText >>= mapM (fetch bch)
  (False, Eq (Fun f xs) (Fun g ys)) <- getText >>= fetch bch 
  pguard "function symbol mismatch" $ f == g
  xys <- cast $ mapM breakTrueEq eqns
  xys' <- zipM xs ys
  pguard "arguments mismatch" $ xys == xys'
pcheck' vb k bch "R" = do 
  eqns <- getList getText >>= mapM (fetch bch)
  (True, Rel r xs) <- getText >>= fetch bch 
  (False, Rel s ys) <- getText >>= fetch bch 
  pguard "relation symbol mismatch" $ r == s
  xys <- cast $ mapM breakTrueEq eqns
  xys' <- zipM xs ys
  pguard "arguments mismatch" $ xys == xys'
pcheck' vb k bch "=R" = do 
  (False, Eq x y) <- getText >>= fetch bch
  guard $ x == y
pcheck' vb k bch "=S" = do 
  (True, Eq x y) <- getText >>= fetch bch
  (False, Eq y' x') <- getText >>= fetch bch
  guard $ x == x' && y == y'
pcheck' vb k bch "=T" = do 
  (True, Eq x y) <- getText >>= fetch bch
  (True, Eq y' z) <- getText >>= fetch bch
  (False, Eq x' z') <- getText >>= fetch bch
  guard $ x == x' && y == y' && z == z'
pcheck' vb k bch "~T" = do 
  (True, Not f) <- getText >>= fetch bch 
  pcheck vb k bch False f
pcheck' vb k bch "~F" = do 
  (False, Not f) <- getText >>= fetch bch 
  pcheck vb k bch True f
pcheck' vb k bch "|T" = do 
  -- (True, Or fs) <- getText >>= fetch bch
  
  nm <- getText 
  (True, Or fs) <- fetch bch nm

  mapM_ (pcheck vb k bch True) fs
pcheck' vb k bch "|F" = do 
  (False, Or fs) <- getText >>= fetch bch
  m <- getInt 
  f <- cast $ nth m fs 
  pcheck vb k bch False f 
pcheck' vb k bch "&T" = do 
  (True, And fs) <- getText >>= fetch bch
  m <- getInt 
  f <- cast $ nth m fs 
  pcheck vb k bch True f 
pcheck' vb k bch "&F" = do 
  (False, And fs) <- getText >>= fetch bch
  mapM_ (pcheck vb k bch False) fs
pcheck' vb k bch ">T" = do 
  (True, Imp f g) <- getText >>= fetch bch
  pcheck vb k bch False f 
  pcheck vb k bch True g 
pcheck' vb k bch ">FA" = do 
  nm <- getText
  (sgn, fg) <- fetch bch nm
  case (sgn, fg) of 
    (False, Imp f g) -> pcheck vb k bch True f 
    _ -> error "not false imp" 
pcheck' vb k bch ">FC" = do 
  (False, Imp _ g) <- getText >>= fetch bch
  pcheck vb k bch False g
pcheck' vb k bch "^TO" = do 
  (True, Iff f g) <- getText >>= fetch bch
  pcheck vb k bch True (f ==> g) 
pcheck' vb k bch "^TR" = do 
  (True, Iff f g) <- getText >>= fetch bch
  pcheck vb k bch True (g ==> f) 
pcheck' vb k bch "^F" = do 
  (False, Iff f g) <- getText >>= fetch bch
  pcheck vb k bch False (f ==> g) 
  pcheck vb k bch False (g ==> f) 

pcheck' vb k bch "!T" = do 
  (True, Fa vs f) <- getText >>= fetch bch
  xs <- getList getTerm
  vxs <- zipM vs xs 
  let f' = substForm vxs f
  pcheck vb k bch True f'

pcheck' vb k bch "!F" = do 
  (False, Fa vs f) <- getText >>= fetch bch
  m <- getInt 
  guard $ k <= m
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "!F-fail : cannot zip"
  let f' = substForm vxs f
  pcheck vb k' bch False f' 

pcheck' vb k bch "?T" = do 
  (True, Ex vs f) <- getText >>= fetch bch
  m <- getInt 
  -- trace ("k = " <> unpack (tlt (ppInt k)) <> "\n") skip
  -- trace ("m = " <> unpack (tlt (ppInt m)) <> "\n") skip
  pguard "index check failed for ?T" (k <= m)
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "?T-fail : cannot zip"
  let f' = substForm vxs f
  pcheck vb k' bch True f' 

pcheck' vb k bch "?F" = do 
  nm <- getText 
  (False, Ex vs f) <- fetch bch nm
  xs <- getList getTerm
  vxs <- zipM vs xs 
  let f' = substForm vxs f
  pcheck vb k bch False f'

pcheck' vb k bch _ = error "impossible case"