module Types where 

import Data.Text.Lazy (Text)
import Data.List as L
import Data.Map as HM ( Map, insert, lookup, empty, map, member, mapMaybe, toList, fromListWithKey, delete )
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList )
import Control.Monad.Fail as MF (MonadFail, fail)
import Data.Functor ((<&>))

data Funct = Reg Text | Idx Int
  deriving (Show, Eq, Ord)

data Term = Var Text | Fun Funct [Term]
  deriving (Show, Eq, Ord)

data Form =
    Rel Funct [Term]
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

data Input' =
    Ign 
  | Afm Text Text Form Ant
  | Inc' Text
  deriving Show

data Gterm =
    Gfun Text [Gterm]
  | Glist [Gterm]
  | Gnum Int
  | Gvar Text
  deriving (Show)

type Ant = Maybe (Gterm, Maybe [Gterm])

data PreAF = 
    CnfAF Text Text Text
  | FofAF Text Text Text

type AF = (Text, Text, Form, Ant)

type Prob = [Input]

data Inf =
    Id Text Text 
  | FunC [Text] Text | RelC [Text] Text Text
  | EqR Text | EqS Text Text |EqT Text Text Text
  | Cut Text Text
  | NotT Text Text | NotF Text Text
  | OrT Text [Text] | OrF Text Int Text 
  | AndT Text Int Text | AndF Text [Text]
  | ImpT Text Text Text | ImpFA Text Text | ImpFC Text Text
  | IffTO Text Text | IffTR Text Text | IffF Text Text Text
  | FaT Text [Term] Text | FaF Text Int Text
  | ExT Text Int Text | ExF Text [Term] Text
  | RelD Text | AoC Term Text | Open
  deriving (Show)

type Node = (Text, Bool, Form)

data Proof =
    Id_ Node Text Text 
  | Cut_ Node Proof Proof
  | FunC_ Node [Text] Text 
  | RelC_ Node [Text] Text Text
  | EqR_ Node Text 
  | EqS_ Node Text Text 
  | EqT_ Node Text Text Text
  | NotT_ Node Text Proof 
  | NotF_ Node Text Proof 
  | OrT_ Node Text [Proof] 
  | OrF_ Node Text Int Proof
  | AndT_ Node Text Int Proof
  | AndF_ Node Text [Proof]
  | ImpT_ Node Text Proof Proof
  | ImpFA_ Node Text Proof
  | ImpFC_ Node Text Proof
  | IffTO_ Node Text Proof
  | IffTR_ Node Text Proof
  | IffF_ Node Text Proof Proof
  | FaT_ Node Text [Term] Proof
  | FaF_ Node Text Int Proof
  | ExT_ Node Text Int Proof
  | ExF_ Node Text [Term]Proof
  | RelD_ Node Proof
  | AoC_ Node Term Proof 
  | Open_ Node
  deriving (Show)
  
type Elab = (Node, Inf, Maybe Text)
type Branch = HM.Map Text (Bool, Form)
type SignForm = (Bool, Form)
type NTF = Map Text Form
type SFTN = Map (Bool, Form) Text