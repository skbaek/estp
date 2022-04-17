module Types where 

import Data.Text (Text)
import Data.List as L
import Data.HashMap.Lazy as HM ( HashMap, insert, lookup, empty, map, member, mapMaybe, toList, fromListWithKey, delete )
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

data Prf =
    Ax Form
  | EqR Term
  | EqSL Term Term Prf
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
  deriving (Show)

data Prf_ =
    Ax_ Form
  | EqR_ Term
  | EqSL_ Term Term Int
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
data Dir = Lft | Rgt
data CMode = Mono | Bi

data Lrat = Del Int [Int] | Add Int [Form] [Int]
  deriving (Show)

type NSeq = HashMap Text Form
type Seq = Set Form
type Hyps = (NSeq, Seq)
