module Types where 

import Data.Text (Text)
import Data.List as L
import Data.Map as HM ( Map, insert, lookup, empty, map, member, mapMaybe, toList, fromListWithKey, delete )
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList )
import Control.Monad.Fail as MF (MonadFail, fail)
import Data.Functor ((<&>))

data Term =
    Fv Int
  | Par Int
  | Bv Text
  | Fun Text [Term]
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
  | EqC (Term, Term, Prf) (Term, Term, Prf)
  | FunC Text [(Term, Term, Prf)]
  | RelC Text [(Term, Term, Prf)]
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
  | Asm
  deriving (Show)

data Prf_ =
    Ax_ Form
  | EqR_ Term
  | EqS_ Term Term 
  | EqT_ Term Term Term
  | EqC_ EqGoal EqGoal
  | FunC_ Text [EqGoal]
  | RelC_ Text [EqGoal]
  | NotL_ Form Int
  | NotR_ Form Int
  | OrL_ [Goal]
  | OrR_ [Form] [Form] Int
  | AndL_ [Form] [Form] Int
  | AndR_ [Goal]
  | ImpL_ Form Form Int Int
  | ImpRA_ Form Form Int
  | ImpRC_ Form Form Int
  | IffLO_ Form Form Int
  | IffLR_ Form Form Int
  | IffR_ Form Form Int Int
  | FaL_ [(Text, Term)] Form Int
  | FaR_ [Text] Int Form Int
  | ExL_ [Text] Int Form Int
  | ExR_ [(Text, Term)] Form Int
  | Cut_ Form Int Int
  | Sorry_

type EqGoal = (Term, Term, Int)
type PrvGoal = (Form, Form, Int)
type Goal = (Form, Int)

data Elab =
    Plab Prf
  | Rdef Text Form Prf
  | Tfe Form
  | Nnf Bool Form
  | AOC [Term] Form Prf
  | Lrats [Form] [Lrat]
  deriving (Show)

data Input =
    Cnf Text Form (Maybe Gterm)
  | Fof Text Form (Maybe Gterm)
  | Inc Text
  deriving Show

data Gterm =
    Gfun Text [Gterm]
  | Glist [Gterm]
  deriving (Show)

type Ant = Maybe Gterm

data AnForm = Af Text Form (Maybe Gterm)

type Prob = [Input]

data JMode = Conj | Disj
data Dir = 
  Obv | Rev
  deriving (Show, Eq)
data CMode = Mono | Bi

data Lrat = Del Int [Int] | Add Int [Form] [Int]
  deriving (Show)

type NSeq = Map Text Form
type Seq = Set Form
type Hyps = (NSeq, Seq)
type Bnd = Map Int Term
type Prfs = Map Int Prf_

data Ctx = Ctx {fresh :: Int, binding :: Bnd, proofs :: Prfs}

data Sst = Sst {flits :: [Form], glits :: [Form], eqns :: [(Term, Term)], sbnd :: Bnd}

data InstMode = Same | Perm
  deriving (Eq)

data UniMode = Lax | Pars | ParFvs | Exact
  deriving (Eq)

data BndMode = Mid | End
  deriving (Eq)

data FD =
    AxFD
  | NotFD FD
  | RWFD Dir Form
  | AndFD [FD]
  | OrFD [FD]
  | IffFD FD FD
  | ImpFD FD FD
  | RelFD [TD]
  | EqFD TD TD
  | PermFD
  | WrapFD
  | DropFD
  -- | SymFD
  | ConstFD
  | FaFD [Text] FD
  | ExFD [Text] FD
  | TransFD FD Form FD
  | AlphaFD 
  | DNFD
  deriving (Show)

data TD =
    Refl
  | FunTD [TD]
  | RW Dir Form
  | TransTD TD Term TD
  deriving (Show)

type Bij a b = (HM.Map a b, HM.Map b a)

type VM = Bij Text Text

data JP =
    Waiting Form
  | Building Pr Form
  | Merged FD Form
  deriving Show

data Pr =
    Open Form Form
  | Clos FD VM
  | EqP Dir Term Term Term Term
  | NotP Pr
  | ImpP Pr Pr
  | IffP Pr Pr
  | FaP [Text] Form [Text] Form Pr
  | ExP [Text] Form [Text] Form Pr
  | OrP [JP] [Form] VM
  | AndP [JP] [Form] VM
  -- | OrP [Form] [Form] [(Pr, Form)]
  -- | AndP [Form] [Form] [(Pr, Form)]
  | TransP Pr Form Pr
  deriving Show