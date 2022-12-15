module Types where 

import Data.ByteString (ByteString)
import Data.Map (Map) 

type BS = ByteString

data Funct = Reg BS | Idx Int
  deriving (Show, Eq, Ord)

data Term = Var BS | Fun Funct [Term]
  deriving (Show, Eq, Ord)

data Form =
    Top | Bot
  | Rel Funct [Term]
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

data Input =
    AnfInput Anf
  | IncInput BS
  deriving Show

data Gent =
    GenT BS [Gent]
  | GenF Form
  | Genl [Gent]
  | Genn Int
  | Genv BS
  deriving (Show)

data Inf =
    Id BS BS | TopF BS | BotT BS
  | FunC [BS] BS | RelC [BS] BS BS
  | EqR BS | EqS BS BS | EqT BS BS BS
  | Cut Form BS BS
  | NotT BS BS | NotF BS BS
  | OrT BS [BS] | OrF BS Int BS 
  | AndT BS Int BS | AndF BS [BS]
  | ImpT BS BS BS | ImpFA BS BS | ImpFC BS BS
  | IffTO BS BS | IffTR BS BS | IffF BS BS BS
  | FaT BS [Term] BS | FaF BS [Int] BS
  | ExT BS [Int] BS | ExF BS [Term] BS
  | RelD Form BS | AoC Term Form BS | Open
  deriving (Show)

data Proof =
    Id_ Node BS BS 
  | TopF_ Node BS
  | BotT_ Node BS
  | Cut_ Node Form Proof Proof
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
  | FaF_ Node BS [Int] Proof
  | ExT_ Node BS [Int] Proof
  | ExF_ Node BS [Term]Proof
  | RelD_ Node Form Proof
  | AoC_ Node Term Form Proof 
  | Open_ Node
  deriving (Show)
  
type Sol = Map BS (Bool, Form, Inf)
type Prob = Map BS Form
type Elab = (Node, Inf)
type Branch = Map BS (Bool, Form)
type SignForm = (Bool, Form)
type Ant = Maybe (Gent, Maybe [Gent])
type Anf = (BS, BS, Form, Ant)
type Node = (BS, Bool, Form)
