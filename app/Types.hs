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

data Prf =
    Id' Form
  | EqR' Term
  | EqS' Term Term
  | EqT' Term Term Term
  | FunC' Funct [Term] [Term]
  | RelC' Funct [Term] [Term]
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
  -- | RelD' Form Prf 
  -- | AoC' [Term] Form Prf 
  | Mrk Text Prf 
  | Open'
  deriving (Show)

data Stelab =
    InfStep Form Prf Text
  | DefStep Form Form Prf Text
  | AoCStep [Term] Form Form Prf Text
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

data PreAF = 
    CnfAF Text Text Text
  | FofAF Text Text Text

type AF = (Text, Text, Form, Ant)

type Prob = [Input]

data Dir = 
  Obv | Rev
  deriving (Show, Eq, Ord)

data Lrat = Del Int [Int] | Add Int [Form] [Int]
  deriving (Show)

type NTF = Map Text Form
type SFTN = Map (Bool, Form) Text

type VC = (HM.Map Text (Set Text), HM.Map Text (Set Text)) 
type VR = (HM.Map Text Text, HM.Map Text Text) 
type VM = HM.Map Text Term

data Path =
    NewRel Funct Int
  | NewFun Funct Int
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
    PreRel Funct Int
  | PreFun Funct Int
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

type Step = (Text, Text, [Text], Form) -- (name, inference, hyps, conc)

type Invranch = HM.Map (Form, Bool) Text

type Branch = HM.Map Text (Bool, Form)

type Nodes = HM.Map Text (Form, Bool, Int)

type SignForm = (Bool, Form)