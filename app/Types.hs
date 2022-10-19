module Types where 

import Data.Text.Lazy (Text)
import Data.List as L
import Data.Map as HM ( Map, insert, lookup, empty, map, member, mapMaybe, toList, fromListWithKey, delete )
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList )
import Control.Monad.Fail as MF (MonadFail, fail)
import Data.Functor ((<&>))

data Term = Var Text | Fun Text [Term]
  deriving (Show, Eq, Ord)

data Form =
    Rel Text [Term]
  | Eq Term Term
  | Not Form
  | And [Form]
  | Or  [Form]
  | Imp Form Form
  | Iff Form Form
  | Fa [Text] Form
  | Ex [Text] Form
  deriving (Show, Eq, Ord)

(<=>) :: Form -> Form -> Form
(<=>) = Iff

(==>) :: Form -> Form -> Form
(==>) = Imp

(===) :: Term -> Term -> Form
(===) = Eq

data Prf =
    Id' Form
  | EqR' Term
  | EqS' Term Term
  | EqT' Term Term Term
  | FunC' Text [Term] [Term]
  | RelC' Text [Term] [Term]
  | NotT' Form Prf
  | NotF' Form Prf
  | OrT' [(Form, Prf)]
  | OrF' [Form] [Form] Prf
  | AndT' [Form] [Form] Prf
  | AndF' [(Form, Prf)]
  | ImpT' Form Form Prf Prf
  | ImpFA' Form Form Prf
  | ImpFC' Form Form Prf
  | IffTO' Form Form Prf
  | IffTR' Form Form Prf
  | IffF' Form Form Prf Prf
  | FaT' [(Text, Term)] Form Prf
  | FaF' [Text] Int Form Prf
  | ExT' [Text] Int Form Prf
  | ExF' [(Text, Term)] Form Prf
  | Cut' Form Prf Prf
  | Mrk Text Prf 
  | Asm
  deriving (Show)

data Elab =
    Plab Form Prf Text
  | RelD' Text Form Form Prf Text
  | AoC' [Term] Form Form Prf Text
  deriving (Show)

-- | RelD'' Form Form Prf Text
-- | AoC'' Int Form Form Prf Text
-- | ElabFail Form Text

data PreInput =
    PreCnf Text Text Text
  | PreFof Text Text Text
  | PreInc Text
  deriving Show

data Input =
    Cnf Text Text Form Ant
  | Fof Text Text Form Ant
  | Inc Text
  deriving Show

data Gterm =
    Gfun Text [Gterm]
  | Glist [Gterm]
  | Gnum Int
  | Gvar Text
  deriving (Show)

type Ant = Maybe (Gterm, Maybe [Gterm])

type PreAF = (Text, Text, Text)
type AF = (Text, Text, Form, Ant)

type Prob = [Input]

data Dir = 
  Obv | Rev
  deriving (Show, Eq, Ord)

-- data Side = 
--   Lft | Rgt
--   deriving (Show, Eq, Ord)
-- 
-- data Pol = 
--   bt | Neg
--   deriving (Show, Eq, Ord)

data Lrat = Del Int [Int] | Add Int [Form] [Int]
  deriving (Show)

type NSeq = Map Text Form
type Seq = Set Form
type Hyps = (NSeq, Seq)

type VC = (HM.Map Text (Set Text), HM.Map Text (Set Text)) 
type VR = (HM.Map Text Text, HM.Map Text Text) 
type VM = HM.Map Text Term

data Path =
    NewRel Text Int
  | NewFun Text Int
  | NewEq
  | NewFa Bool
  | NewEx Bool
  | NewImpL
  | NewImpR
  | NewIffL
  | NewIffR
  | NewOr Int Int
  | NewAnd Int Int
  | NewNot
  deriving (Ord, Eq)

data PrePath =
    PreRel Text Int
  | PreFun Text Int
  | PreEq
  | PreFa [Text]
  | PreEx [Text]
  | PreImpL
  | PreImpR
  | PreIffL
  | PreIffR
  | PreOr Int Int
  | PreAnd Int Int
  | PreNot
  deriving (Ord, Eq)

type Sig = HM.Map [Path] Int
type Sigs = HM.Map Text Sig


type RSTA = (VM, Maybe (Form, Dir), [Form], [Form], [Form])
type SST = (VM, [Form], [Form], [(Term, Term)])
type EFS = (VM, Maybe Bool, [Form])
type FSTA = (VM, [Form])
type EQFS = (VM, [Form], [(Term, Term)])
type MTT = HM.Map Text Text
type MTI = HM.Map Text Int
type USOL = [Term]

type EP = (Int, [(Int, Int)])
type EF = (EP, Bool, Form, Int, Inf, Maybe Text)

data Inf =
    Id Text Text 
  | Cut
  | FunC [Text] Text | RelC [Text] Text Text
  | EqR Text | EqS Text Text |EqT Text Text Text
  | NotT Text | NotF Text 
  | OrT Text | OrF Text | AndT Text | AndF Text
  | ImpT Text | ImpFA Text | ImpFC Text 
  | IffTO Text | IffTR Text | IffF Text
  | FaT Text [Term] | FaF Text Int 
  | ExT Text Int | ExF Text [Term]
  | RelD | AoC [Term] | Open
  deriving (Show)