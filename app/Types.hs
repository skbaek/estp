module Types where 

import Data.Text (Text)
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
    Ax Form
  | EqR Term
  | EqS Term Term
  | EqT Term Term Term
  | FunC Text [Term] [Term]
  | RelC Text [Term] [Term]
  | NotL Form Prf
  | NotR Form Prf
  | OrL [(Form, Prf)]
  | OrR [Form] [Form] Prf
  | AndL [Form] [Form] Prf
  | AndR [(Form, Prf)]
  | ImpL Form Form Prf Prf
  | ImpRA Form Form Prf
  | ImpRC Form Form Prf
  | IffLO Form Form Prf
  | IffLR Form Form Prf
  | IffR Form Form Prf Prf
  | FaL [(Text, Term)] Form Prf
  | FaR [Text] Int Form Prf
  | ExL [Text] Int Form Prf
  | ExR [(Text, Term)] Form Prf
  | Cut Form Prf Prf
  | Mrk Text Prf 
  | Asm
  deriving (Show)

data Elab =
    Plab Form Prf Text
  | Rdef Text Form Form Prf Text
  | AOC [Term] Form Form Prf Text
  deriving (Show)
-- | ElabFail Form Text

data Input =
    Cnf Text Form (Maybe Gterm)
  | Fof Text Form (Maybe Gterm)
  | Inc Text
  deriving Show

data Gterm =
    Gfun Text [Gterm]
  | Glist [Gterm]
  | Gnum Int
  | Gvar Text
  deriving (Show)

type Ant = Maybe Gterm

data AnForm = Af Text Form (Maybe Gterm)

type Prob = [Input]

data Dir = 
  Obv | Rev
  deriving (Show, Eq, Ord)

data Side = 
  Lft | Rgt
  deriving (Show, Eq, Ord)

data Pol = 
  Pos | Neg
  deriving (Show, Eq, Ord)

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
type EF = (Form, Pol, EP, Int, Inf, Text)

data Inf =
    InfAx Text Text
  | InfEqR Text
  | InfEqS Text Text
  | InfEqT Text Text Text
  -- | InfEqC Text Text Text
  | InfFunC [Text] Text
  | InfRelC [Text] Text Text
  | InfNotL Text
  | InfNotR Text
  | InfOrL Text
  | InfOrR Text 
  | InfAndL Text
  | InfAndR Text
  | InfImpL Text
  | InfImpR Text Side
  | InfIffL Text Dir
  | InfIffR Text
  | InfFaL Text [Term]
  | InfFaR Text Int 
  | InfExL Text Int 
  | InfExR Text [Term]
  | InfCut
  | InfRdef
  | InfAoC [Term]
  | Close
  deriving (Show)