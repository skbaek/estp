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

type NodeInfo = (Text, Bool, Form)

data Proof =
    Id_ NodeInfo Text Text 
  | Cut_ NodeInfo Proof Proof
  | FunC_ NodeInfo [Text] Text 
  | RelC_ NodeInfo [Text] Text Text
  | EqR_ NodeInfo Text 
  | EqS_ NodeInfo Text Text 
  | EqT_ NodeInfo Text Text Text
  | NotT_ NodeInfo Text Proof 
  | NotF_ NodeInfo Text Proof 
  | OrT_ NodeInfo Text [Proof] 
  | OrF_ NodeInfo Text Int Proof
  | AndT_ NodeInfo Text Int Proof
  | AndF_ NodeInfo Text [Proof]
  | ImpT_ NodeInfo Text Proof Proof
  | ImpFA_ NodeInfo Text Proof
  | ImpFC_ NodeInfo Text Proof
  | IffTO_ NodeInfo Text Proof
  | IffTR_ NodeInfo Text Proof
  | IffF_ NodeInfo Text Proof Proof
  | FaT_ NodeInfo Text [Term] Proof
  | FaF_ NodeInfo Text Int Proof
  | ExT_ NodeInfo Text Int Proof
  | ExF_ NodeInfo Text [Term]Proof
  | RelD_ NodeInfo Proof
  | AoC_ NodeInfo Term Proof 
  | Open_ NodeInfo
  deriving (Show)
  
type Elab = (NodeInfo, Inf, Maybe Text)
type Branch = HM.Map Text (Bool, Form)
type SignForm = (Bool, Form)
type NTF = Map Text Form
type SFTN = Map (Bool, Form) Text