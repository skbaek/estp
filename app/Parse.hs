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
import Control.Monad as M ( MonadPlus(mzero), guard, when, mzero )
import Data.Map as HM (lookup, insert)
import Data.Functor ((<&>))
import Data.Set (Set, member)
import Data.String (fromString)
import Debug.Trace (trace)
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



{- Parsers -}

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
ws = star space >> nil

digit :: Parser Char
digit = satisfy isDigit

nil :: Parser ()
nil = Parser $ \ s -> Just (() , s)

eof :: Parser ()
eof = Parser  $ \ t ->
  if BS.null t
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

lowerWord :: Parser BS
lowerWord = do
  c <- lowerAlpha
  cs <- star alphaNumeric
  ign
  unit $ fromString $ c : cs

upperWord :: Parser BS
upperWord = do
  c <- upperAlpha
  cs <- star alphaNumeric
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
  cs <- star numeric
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
  s <- plus sqChar
  char '\''
  let bs = fromString s
  ws
  unit $ "'" <> bs <> "'"

distinctObject :: Parser BS
distinctObject = do
  char '"'
  s <- plus $ satisfy isDoChar
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

usefulInfo :: Parser (Maybe [Gterm])
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
  do { kt <- integer ; cast (bs2int kt) >>= (unit . Gnum) } <|>
  do { v <- upperWord ; unit (Gvar v) }

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
  unit (Inc w)

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



{- Others -}

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

nameParseEstp :: String -> IO ESTP
nameParseEstp n = do
  -- ps "Reading file as text...\n"
  bs <- BS.readFile n
  -- ps "Parsing the text read...\n"
  (_, bs') <- cast $ parse ign bs
  textParseEstp bs'

textParseEstp :: BS -> IO ESTP
textParseEstp bs =
  case parse input t of
    Just (i,s) -> do
      pfx <- parseInput i
      if BS.null s
      then return pfx
      else do
        sfx <- parseBS s
        return (pfx ++ sfx)
    _ -> error ("Failed to parse input : " <> show t)

-- nameParsePreAnfs :: String -> IO [PreAnf]
-- nameParsePreAnfs n = do
--   t <- BS.readFile n
--   (_, s) <- cast $ parse ign t 
--   textParsePreAnfs s
-- 
-- textParsePreAnfs :: BS -> IO [PreAnf]
-- textParsePreAnfs t = do
--  (i,s) <- cast $ parse preInput t 
--  _

 --  case parse preInput t of
 --    Just (i,s) -> do
 --      pfx <- parsePreInput i
 --      if BS.null s
 --      then return pfx
 --      else do
 --        sfx <- parsePreBS s
 --        return (pfx ++ sfx)
 --    _ -> error ("Failed to parse input : " <> show t)
parseSingleQuote :: BS -> Maybe BS
parseSingleQuote ('\'' :> t) = do
  (t', '\'') <- DBF.second w2c <$> BS.unsnoc t
  return t'
parseSingleQuote _ = mzero
{- 

anf :: Parser Input
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
  unit (Anf n r f a)

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
  unit (Anf n r (conjecturize r f) a)

input :: Parser Input
input = anf <|> inc


prob :: Parser Prob
prob = star input

runParser :: Parser a -> BS -> a
runParser m s =
  case parse m s of
    Just (res, empty) -> res
    _                 -> error "Parser error."

run :: BS -> Prob
run = runParser (ign >> prob)


parsePreInput :: PreInput -> IO [PreAF]
parsePreInput (PreInc s) = do
  tptp <- getEnv "TPTP"
  s' <- cast $ unquote s
  parsePreName $ tptp ++ "/" ++ bs2str s'
parsePreInput (PreAnf n r f x) = return [(n, r, f, x)]

parseInput :: Input -> IO [AF]
parseInput (Inc s) = do
  tptp <- getEnv "TPTP"
  s<- cast $ unquote s
  ps $ "Axiom name : " ++ show s' ++ "\n"
  parseName $ tptp ++ "/" ++ show s'
parseInput (Anf n r f t) = return [(n, r, f, t)]


parseBS :: BS -> IO [AF]
parseBS t =
  case parse input t of
    Just (i,s) -> do
      pfx <- parseInput i
      if BS.null s
      then return pfx
      else do
        sfx <- parseBS s
        return (pfx ++ sfx)
    _ -> error ("Failed to parse input : " <> show t)


parseForm :: BS -> Form
parseForm tx =
  case parse form tx of
    Just (f, tx') -> if BS.null tx' then f else error ("parse-form failed, case 1 : " <> show tx)
    _ -> error $ "parse-form failed, case 2 : " <> show tx

parseName :: String -> IO [AF]
parseName n = do
  ps "Reading file as text...\n"
  t <- BS.readFile n
  ps "Parsing the text read...\n"
  case parse ign t of
    Just (i,s) -> parseBS s
    _ -> ioError $ userError "Read filename, but failed to parse content"

gTermsToMaybeBS :: Maybe [Gterm] -> IO (Maybe BS)
gTermsToMaybeBS Nothing = return nt
gTermsToMaybeBS (Just [Gfun tx []]) = return $ Just tx
gTermsToMaybeBS _ = error "Cannot extact maybe text"

gTermToBS :: Gterm -> IO BS
gTermToBS (Gfun t []) = return t
gTermToBS _ = mzero

textToIdxFunct :: BS -> IO Funct
textToIdxFunct tx = do
  ('#' :> tx') <- cast $ parseSingleQuote tx
  cast (Idx <$> bs2int tx')

textToFunct :: BS -> IO Funct
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
  nf <- gTermToBS gtf
  nt <- gTermToBS gtt
  return $ Cut nf nt
gTermToInf (Gfun "id" [gt0, gt1]) = do
  m <- gTermToBS gt0
  n <- gTermToBS gt1
  return $ Id m n
gTermToInf (Gfun "iffto" [gth, gtc]) = do
  nh <- gTermToBS gth
  nc <- gTermToBS gtc
  return $ IffTO nh nc
gTermToInf (Gfun "ifftr" [gth, gtc]) = do
  nh <- gTermToBS gth
  nc <- gTermToBS gtc
  return $ IffTR nh nc
gTermToInf (Gfun "ifff" [gth, gt0, gt1]) = do
  nh <- gTermToBS gth
  n0 <- gTermToBS gt0
  n1 <- gTermToBS gt1
  return $ IffF nh n0 n1
gTermToInf (Gfun "impfa" [gth, gtc]) = do
  nh <- gTermToBS gth
  nc <- gTermToBS gtc
  return $ ImpFA nh nc
gTermToInf (Gfun "impfc" [gth, gtc]) = do
  nh <- gTermToBS gth
  nc <- gTermToBS gtc
  return $ ImpFC nh nc
gTermToInf (Gfun "impt" [gth, gt0, gt1]) = do
  nh <- gTermToBS gth
  n0 <- gTermToBS gt0
  n1 <- gTermToBS gt1
  return $ ImpT nh n0 n1
gTermToInf (Gfun "bott" [gt]) = do
  nm <- gTermToBS gt
  return $ OrT nm []
gTermToInf (Gfun "ort" [gt, Glist gts]) = do
  nm <- gTermToBS gt
  nms <- mapM gTermToBS gts
  return $ OrT nm nms
gTermToInf (Gfun "orf" [gth, Gnum k, gtc]) = do
  nh <- gTermToBS gth
  nc <- gTermToBS gtc
  return $ OrF nh k nc
gTermToInf (Gfun "andt" [gth, Gnum k, gtc]) = do
  nh <- gTermToBS gth
  nc <- gTermToBS gtc
  return $ AndT nh k nc
gTermToInf (Gfun "topf" [gt]) = do
  nm <- gTermToBS gt
  return $ AndF nm []
gTermToInf (Gfun "andf" [gt, Glist gts]) = do
  nm <- gTermToBS gt
  nms <- mapM gTermToBS gts
  return $ AndF nm nms
gTermToInf (Gfun "faf" [gth, Gnum k, gtc]) = do
  nh <- gTermToBS gth
  nc <- gTermToBS gtc
  return $ FaF nh k nc
gTermToInf (Gfun "ext" [gth, Gnum k, gtc]) = do
  nh <- gTermToBS gth
  nc <- gTermToBS gtc
  return $ ExT nh k nc
gTermToInf (Gfun "fat" [gth, Glist gts, gtc]) = do
  nh <- gTermToBS gth
  xs <- mapM gTermToTerm gts
  nc <- gTermToBS gtc
  return $ FaT nh xs nc
gTermToInf (Gfun "exf" [gth, Glist gts, gtc]) = do
  nh <- gTermToBS gth
  xs <- mapM gTermToTerm gts
  nc <- gTermToBS gtc
  return $ ExF nh xs nc
gTermToInf (Gfun "nott" [gth, gtc]) = do
  nh <- gTermToBS gth
  nc <- gTermToBS gtc
  return $ NotT nh nc
gTermToInf (Gfun "notf" [gth, gtc]) = do
  nh <- gTermToBS gth
  nc <- gTermToBS gtc
  return $ NotF nh nc
gTermToInf (Gfun "eqr" [gt]) = EqR <$> gTermToBS gt
gTermToInf (Gfun "eqs" gts) = do
  [nm0, nm1] <- mapM gTermToBS gts
  return $ EqS nm0 nm1
gTermToInf (Gfun "eqt" gts) = do
  [nm0, nm1, nm2] <- mapM gTermToBS gts
  return $ EqT nm0 nm1 nm2
gTermToInf (Gfun "func" [Glist gts, gt]) = do
  nms <- mapM gTermToBS gts
  nm <- gTermToBS gt
  return $ FunC nms nm
gTermToInf (Gfun "relc" [Glist gts, gt0, gt1]) = do
  nms <- mapM gTermToBS gts
  m <- gTermToBS gt0
  n <- gTermToBS gt1
  return $ RelC nms m n
gTermToInf (Gfun "aoc" [gtx, gtn]) = do
  x <- gTermToTerm gtx
  nm <- gTermToBS gtn
  return $ AoC x nm
gTermToInf (Gfun "reld" [gt]) = RelD <$> gTermToBS gt
gTermToInf (Gfun "open" []) = return Open
gTermToInf t = error $ "inf reader : " <> fromString (show t)


sortAfs :: [AF] -> [AF]
sortAfs = sortBy compareAfs

compareAfs :: AF -> AF -> Ordering
compareAfs (m :> ms, _, _, _) (n :> ns, _, _, _) =
  case compare m n of
    EQ ->
      case (bs2int ms, bs2int ns) of
        (Just i, Just j) -> compare i j
        _ -> error "Cannot compare step names"
    other -> other
compareAfs _ _ = LT

proofCheck :: Bool -> Int -> Branch -> Bool -> Form -> Parser ()
proofCheck vb k bch sgn f = do
  nm <- getBS
  let bch' = HM.insert nm (sgn, f) bch
  r <- getRule <|> error "cannot read rule"
  proofCheck' vb k bch' r

pguard :: BS -> Bool -> Parser ()
pguard tx True = skip
pguard tx False = error $ show tx

getBS :: Parser BS
getBS = Parser $
  \ tx ->
    case BS.break ((== '.') . w2c) tx of
      (pfx, _ :> sfx) -> Just (pfx, sfx)
      _ -> error "No full stop found"

getInt :: Parser Int
getInt = do
  tx <- getBS
  cast $ bs2int tx

getFunct :: Parser Funct
getFunct = (char '#' >> (Idx <$> getInt)) <|> (Reg <$> getBS)

getList :: Parser a -> Parser [a]
getList p = (char '.' >> unit []) <|>
  ( do char ','
       x <- p
       xs <- getList p
       unit (x : xs) )

getSign :: Parser Bool
getSign = (lit "T" >> return True) <|> (lit "F" >> return False)

getRule :: Parser BS
getRule =
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

getTerms :: Parser [Term]
getTerms = getList getTerm

getTerm :: Parser Term
getTerm = item >>= getTerm'

getTerm' :: Char -> Parser Term
getTerm' '$' = Var <$> getBS
getTerm' '@' = do
  f <- getFunct
  Fun f <$> getTerms
getTerm' _ = error "cannot get term"

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
  vs <- getList getBS
  Fa vs <$> getForm
getForm' '?' = do
  vs <- getList getBS
  Ex vs <$> getForm
getForm' c = error $ "invalid head character : " <> [c]

fetch :: (MonadFail m) => Branch -> BS -> m SignForm
fetch bch nm = cast (HM.lookup nm bch)

proofCheck' :: Bool -> Int -> Branch -> BS -> Parser ()
proofCheck' vb k bch "I" = do
  nt <- getBS
  nf <- getBS
  ft <- fetch bch nt
  ff <- fetch bch nf
  pguard "id-fail" $ complementary ft ff
proofCheck' vb k bch "C" = do
  f <- getForm
  proofCheck vb k bch False f
  proofCheck vb k bch True f
proofCheck' vb k bch "D" = do
  f <- getForm
  k' <- cast $ checkRelD k f
  proofCheck vb k' bch True f
proofCheck' vb k bch "A" = do
  x <- getTerm
  f <- getForm
  k' <- cast $ checkAoC k x f
  proofCheck vb k' bch True f
proofCheck' vb k bch "O" = skip
proofCheck' vb k bch "F" = do
  eqns <- getList getBS >>= mapM (fetch bch)
  (False, Eq (Fun f xs) (Fun g ys)) <- getBS >>= fetch bch
  pguard "function symbol mismatch" $ f == g
  xys <- cast $ mapM breakTrueEq eqns
  xys' <- zipM xs ys
  pguard "arguments mismatch" $ xys == xys'
proofCheck' vb k bch "R" = do
  eqns <- getList getBS >>= mapM (fetch bch)
  (True, Rel r xs) <- getBS >>= fetch bch
  (False, Rel s ys) <- getBS >>= fetch bch
  pguard "relation symbol mismatch" $ r == s
  xys <- cast $ mapM breakTrueEq eqns
  xys' <- zipM xs ys
  pguard "arguments mismatch" $ xys == xys'
proofCheck' vb k bch "=R" = do
  (False, Eq x y) <- getBS >>= fetch bch
  guard $ x == y
proofCheck' vb k bch "=S" = do
  (True, Eq x y) <- getBS >>= fetch bch
  (False, Eq y' x') <- getBS >>= fetch bch
  guard $ x == x' && y == y'
proofCheck' vb k bch "=T" = do
  (True, Eq x y) <- getBS >>= fetch bch
  (True, Eq y' z) <- getBS >>= fetch bch
  (False, Eq x' z') <- getBS >>= fetch bch
  guard $ x == x' && y == y' && z == z'
proofCheck' vb k bch "~T" = do
  (True, Not f) <- getBS >>= fetch bch
  proofCheck vb k bch False f
proofCheck' vb k bch "~F" = do
  (False, Not f) <- getBS >>= fetch bch
  proofCheck vb k bch True f
proofCheck' vb k bch "|T" = do
  nm <- getBS
  (True, Or fs) <- fetch bch nm
  mapM_ (proofCheck vb k bch True) fs
proofCheck' vb k bch "|F" = do
  (False, Or fs) <- getBS >>= fetch bch
  m <- getInt
  f <- cast $ nth m fs
  proofCheck vb k bch False f
proofCheck' vb k bch "&T" = do
  (True, And fs) <- getBS >>= fetch bch
  m <- getInt
  f <- cast $ nth m fs
  proofCheck vb k bch True f
proofCheck' vb k bch "&F" = do
  (False, And fs) <- getBS >>= fetch bch
  mapM_ (proofCheck vb k bch False) fs
proofCheck' vb k bch ">T" = do
  (True, Imp f g) <- getBS >>= fetch bch
  proofCheck vb k bch False f
  proofCheck vb k bch True g
proofCheck' vb k bch ">FA" = do
  nm <- getBS
  (sgn, fg) <- fetch bch nm
  case (sgn, fg) of
    (False, Imp f g) -> proofCheck vb k bch True f
    _ -> error "not false imp"
proofCheck' vb k bch ">FC" = do
  (False, Imp _ g) <- getBS >>= fetch bch
  proofCheck vb k bch False g
proofCheck' vb k bch "^TO" = do
  (True, Iff f g) <- getBS >>= fetch bch
  proofCheck vb k bch True (f ==> g)
proofCheck' vb k bch "^TR" = do
  (True, Iff f g) <- getBS >>= fetch bch
  proofCheck vb k bch True (g ==> f)
proofCheck' vb k bch "^F" = do
  (False, Iff f g) <- getBS >>= fetch bch
  proofCheck vb k bch False (f ==> g)
  proofCheck vb k bch False (g ==> f)
proofCheck' vb k bch "!T" = do
  (True, Fa vs f) <- getBS >>= fetch bch
  xs <- getList getTerm
  vxs <- zipM vs xs
  let f' = substForm vxs f
  proofCheck vb k bch True f'
proofCheck' vb k bch "!F" = do
  (False, Fa vs f) <- getBS >>= fetch bch
  m <- getInt
  guard $ k <= m
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "!F-fail : cannot zip"
  let f' = substForm vxs f
  proofCheck vb k' bch False f'
proofCheck' vb k bch "?T" = do
  (True, Ex vs f) <- getBS >>= fetch bch
  m <- getInt
  pguard "index check failed for ?T" (k <= m)
  let (k', xs) = listPars m vs
  vxs <- zipM vs xs <|> error "?T-fail : cannot zip"
  let f' = substForm vxs f
  proofCheck vb k' bch True f'
proofCheck' vb k bch "?F" = do
  nm <- getBS
  (False, Ex vs f) <- fetch bch nm
  xs <- getList getTerm
  vxs <- zipM vs xs
  let f' = substForm vxs f
  proofCheck vb k bch False f'
proofCheck' vb k bch _ = error "impossible case"

proof :: Branch -> Bool -> Form -> Parser Proof
proof bch sgn f = do
  nm <- getBS
  -- trace ("Name : " ++ unpack nm ++ "\n") skip
  let bch' = HM.insert nm (sgn, f) bch
  r <- getRule <|> error "cannot read rule"
  -- trace ("Rule : " ++ unpack r ++ "\n") skip
  proof' bch' (nm, sgn, f) r

proof' :: Branch -> Node -> BS -> Parser Proof
proof' bch ni "I" = do
  nt <- getBS
  nf <- getBS
  return $ Id_ ni nt nf
proof' bch ni "C" = do
  f <- getForm
  pf <- proof bch False f
  pt <- proof bch True f
  return $ Cut_ ni pf pt
proof' bch ni "D" = do
  f <- getForm
  p <- proof bch True f
  return $ RelD_ ni p
proof' bch ni "A" = do
  x <- getTerm
  f <- getForm
  p <- proof bch True f
  return $ AoC_ ni x p
proof' bch ni "O" = return $ Open_ ni

proof' bch ni "F" = do
  nms <- getList getBS
  nm <- getBS
  return $ FunC_ ni nms nm
proof' bch ni "R" = do
  nms <- getList getBS
  nt <- getBS
  nf <- getBS
  return $ RelC_ ni nms nt nf
proof' bch ni "=R" = do
  nf <- getBS
  return $ EqR_ ni nf
proof' bch ni "=S" = do
  nt <- getBS
  nf <- getBS
  return $ EqS_ ni nt nf
proof' bch ni "=T" = do
  nxy <- getBS
  nyz <- getBS
  nxz <- getBS
  return $ EqT_ ni nxy nyz nxz
proof' bch ni "~T" = do
  nm <- getBS
  (True, Not f) <- fetch bch nm
  p <- proof bch False f
  return $ NotT_ ni nm p
proof' bch ni "~F" = do
  nm <- getBS
  (False, Not f) <- fetch bch nm
  p <- proof bch True f
  return $ NotF_ ni nm p
proof' bch ni "|T" = do
  nm <- getBS
  (True, Or fs) <- fetch bch nm
  ps <- mapM (proof bch True) fs
  return $ OrT_ ni nm ps
proof' bch ni "|F" = do
  nm <- getBS
  (False, Or fs) <- fetch bch nm
  m <- getInt
  f <- cast $ nth m fs
  p <- proof bch False f
  return $ OrF_ ni nm m p
proof' bch ni "&T" = do
  nm <- getBS
  (True, And fs) <- fetch bch nm
  m <- getInt
  f <- cast $ nth m fs
  p <- proof bch True f
  return $ AndT_ ni nm m p
proof' bch ni "&F" = do
  nm <- getBS
  (False, And fs) <- fetch bch nm
  ps <- mapM (proof bch False) fs
  return $ AndF_ ni nm ps
proof' bch ni ">T" = do
  nm <- getBS
  (True, Imp f g) <- fetch bch nm
  pa <- proof bch False f
  pc <- proof bch True g
  return $ ImpT_ ni nm pa pc
proof' bch ni ">FA" = do
  nm <- getBS
  (False, Imp f _) <- fetch bch nm
  p <- proof bch True f
  return $ ImpFA_ ni nm p
proof' bch ni ">FC" = do
  nm <- getBS
  (False, Imp _ g) <- fetch bch nm
  p <- proof bch False g
  return $ ImpFC_ ni nm p
proof' bch ni "^TO" = do
  nm <- getBS
  (True, Iff f g) <- fetch bch nm
  p <- proof bch True (f ==> g)
  return $ IffTO_ ni nm p
proof' bch ni "^TR" = do
  nm <- getBS
  (True, Iff f g) <- fetch bch nm
  p <- proof bch True (g ==> f)
  return $ IffTR_ ni nm p
proof' bch ni "^F" = do
  nm <- getBS
  (False, Iff f g) <- fetch bch nm
  po <- proof bch False (f ==> g)
  pr <- proof bch False (g ==> f)
  return $ IffF_ ni nm po pr
proof' bch ni "!T" = do
  nm <- getBS
  (True, Fa vs f) <- fetch bch nm
  xs <- getList getTerm
  vxs <- zipM vs xs
  let f' = substForm vxs f
  p <- proof bch True f'
  return $ FaT_ ni nm xs p
proof' bch ni "!F" = do
  nm <- getBS
  (False, Fa vs f) <- fetch bch nm
  m <- getInt
  let (_, vxs) = varPars m vs
  let f' = substForm vxs f
  p <- proof bch False f'
  return $ FaF_ ni nm m p
proof' bch ni "?T" = do
  nm <- getBS
  (True, Ex vs f) <- fetch bch nm
  m <- getInt
  let (_, vxs) = varPars m vs
  let f' = substForm vxs f
  p <- proof bch True f'
  return $ ExT_ ni nm m p
proof' bch ni "?F" = do
  nm <- getBS
  (False, Ex vs f) <- fetch bch nm
  xs <- getList getTerm
  vxs <- zipM vs xs
  let f' = substForm vxs f
  p <- proof bch False f'
  return $ ExF_ ni nm xs p
proof' bch ni _ = error "invalid rule"

-}