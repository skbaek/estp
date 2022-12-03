module Types where 

import qualified Data.ByteString as BS (ByteString)
import Data.Map as HM ( Map, insert, lookup, empty, map, member, mapMaybe, toList, fromListWithKey, delete )
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList )
import Control.Monad.Fail as MF (MonadFail, fail)
import Data.Functor ((<&>))

type BS = BS.ByteString

data Funct = Reg BS | Idx Int
  deriving (Show, Eq, Ord)

data Term = Var BS | Fun Funct [Term]
  deriving (Show, Eq, Ord)

data Form =
    Rel Funct [Term]
  | Eq Term Term
  | Not Form
  | And [Form]
  | Or  [Form]
  | Imp Form Form
  | Iff Form Form
  | Fa [BS] Form
  | Ex [BS] Form
  deriving (Show, Eq, Ord)

(<=>) :: Form -> Form -> Form
(<=>) = Iff

(==>) :: Form -> Form -> Form
(==>) = Imp

(===) :: Term -> Term -> Form
(===) = Eq

-- data PreInput =
--     PreAnf BS BS BS Ant
--   | PreInc BS
--   deriving Show

data Input =
    Anf BS BS Form Ant
  | Inc BS
  deriving Show

data Gterm =
    Gfun BS [Gterm]
  | Glist [Gterm]
  | Gnum Int
  | Gvar BS
  deriving (Show)

type Ant = Maybe (Gterm, Maybe [Gterm])

-- data PreAF =  CnfAF BS BS BS Ant | FofAF BS BS BS Ant

type PreAnf = (BS, BS, BS, Ant)
type Anf = (BS, BS, Form, Ant)

type Prob = [Input]

data Inf =
    Id BS BS 
  | FunC [BS] BS | RelC [BS] BS BS
  | EqR BS | EqS BS BS | EqT BS BS BS
  | Cut BS BS
  | NotT BS BS | NotF BS BS
  | OrT BS [BS] | OrF BS Int BS 
  | AndT BS Int BS | AndF BS [BS]
  | ImpT BS BS BS | ImpFA BS BS | ImpFC BS BS
  | IffTO BS BS | IffTR BS BS | IffF BS BS BS
  | FaT BS [Term] BS | FaF BS Int BS
  | ExT BS Int BS | ExF BS [Term] BS
  | RelD BS | AoC Term BS | Open
  deriving (Show)

type Node = (BS, Bool, Form)
type Node' = (BS, Bool, BS)

data Proof =
    Id_ Node BS BS 
  | Cut_ Node Proof Proof
  | FunC_ Node [BS] BS 
  | RelC_ Node [BS] BS BS
  | EqR_ Node BS 
  | EqS_ Node BS BS 
  | EqT_ Node BS BS BS
  | NotT_ Node BS Proof 
  | NotF_ Node BS Proof 
  | OrT_ Node BS [Proof] 
  | OrF_ Node BS Int Proof
  | AndT_ Node BS Int Proof
  | AndF_ Node BS [Proof]
  | ImpT_ Node BS Proof Proof
  | ImpFA_ Node BS Proof
  | ImpFC_ Node BS Proof
  | IffTO_ Node BS Proof
  | IffTR_ Node BS Proof
  | IffF_ Node BS Proof Proof
  | FaT_ Node BS [Term] Proof
  | FaF_ Node BS Int Proof
  | ExT_ Node BS Int Proof
  | ExF_ Node BS [Term]Proof
  | RelD_ Node Proof
  | AoC_ Node Term Proof 
  | Open_ Node
  deriving (Show)
  
-- type TSTP = Map BS (Bool, Form, Inf)
type ESTP = Map BS (Bool, Form, Inf)

type Elab = (Node, Inf, Maybe BS)
-- type Elab' = (Node', Inf, Maybe BS)
type Branch = HM.Map BS (Bool, Form)
type SignForm = (Bool, Form)