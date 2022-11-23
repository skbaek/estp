{-# LANGUAGE OverloadedStrings #-}

module PP where

import Basic
import Data.List as L
import Data.Text.Lazy as T ( concat, intercalate, pack, Text, length, take )
import qualified Data.Text.Lazy as TL (toStrict, intercalate)
import Data.Text.Lazy.Builder as B (Builder, singleton, fromLazyText, toLazyText)
-- import qualified Data.Text.Lazy.Builder.Int as B
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList )
import Text.Printf
import Data.Map as HM

import Types
import Data.Functor.Contravariant (Op(Op))


ppLrat :: Lrat -> Builder
ppLrat (Del k ks) = ppInt k  <> ". Del " <> ppInter " " (L.map ppInt ks)
ppLrat (Add k fs ms) = ppInt k  <> ". Add " <> ppInter " " (L.map ppForm fs) <> ", Using : " <> ppInter  " " (L.map ppInt ms)

ppMapping :: (Text, Term) -> Builder
ppMapping (t, x) = ft t <> " |-> " <> ppTerm x

ppVmap :: (Text, Text) -> Builder
ppVmap (v, w) = ft $  v <> " <-|-> " <> w

-- ppVM :: VM -> Builder
-- ppVM gm = ppListNl (\ (v, x) -> v <> " |-> " <> ppTerm x) (HM.toList gm)

ppVR :: VR -> Builder
ppVR (vw, _) = ppListNl ppVmap (HM.toList vw)

ppVCAux :: HM.Map Text (Set Text) -> Builder
ppVCAux vw = ppListNl (\ (v_, ws_) -> ft v_ <> " |-> " <> ppList ft (S.toList ws_)) (HM.toList vw)

ppVC :: VC -> Builder
ppVC (vws, wvs) = ppVCAux vws <> "-------------------------------------\n" <> ppVCAux wvs

-- ft :: Data.Text.Internal.Lazy.Text -> Builder

ppInter :: Builder -> [Builder] -> Builder
ppInter b [] = mempty
ppInter b [x] = x
ppInter b (x : xs) = x <> b <> ppInter b xs

ppList :: (a -> Builder) -> [a] -> Builder
ppList f xs = "[" <> ppInter ", " (L.map f xs) <> "]"

ppSet :: (a -> Builder) -> Set a -> Builder
ppSet f s = ppList f (S.toList s)

ppSetNl :: (a -> Builder) -> Set a -> Builder
ppSetNl f s = ppListNl f (S.toList s)

ppListNl :: (a -> Builder) -> [a] -> Builder
ppListNl f xs = mconcat (L.map (\ x -> f x <> "\n") xs)

ppHM :: (a -> Builder) -> (b -> Builder) -> HM.Map a b -> Builder
ppHM f g m = ppListNl (\ (a_, b_) -> f a_ <> " |-> " <> g b_) $ HM.toList m

ppTerm :: Term -> Builder
-- ppTerm (Par k) = "#" <> ppInt k
ppTerm (Var x) = ft x
ppTerm (Fun f xs) = ppFunct f <> ppArgs (L.map ppTerm xs)

ppJct :: Bool -> String
ppJct True  = " /\\ "
ppJct False = " \\/ "

ppApp :: Builder -> [Builder] -> Builder
ppApp f ts = f <> ppArgs ts

ppArgs :: [Builder] -> Builder
ppArgs [] = ""
ppArgs ts = "(" <> ppInter ", " ts <> ")"

ppQtf :: Bool -> Builder
ppQtf True  = "!"
ppQtf False = "?"

ppFunct :: Funct -> Builder
ppFunct (Reg tx) = ft tx
ppFunct (Idx k) = "'#" <> ppInt k <> "'"

ppForm :: Form -> Builder
ppForm (Eq t s) = "(" <> ppTerm t <> " = " <> ppTerm s <> ")"
ppForm (Rel r xs) = ppFunct r <> ppArgs (L.map ppTerm xs)
ppForm (Not f) = "~ " <> ppForm f
ppForm (And []) = "$true"
ppForm (Or  []) = "$false"
ppForm (And fs) = "(" <> ppInter " & " (L.map ppForm fs) <> ")"
ppForm (Or  fs) = "(" <> ppInter " | " (L.map ppForm fs) <> ")"
ppForm (Imp f g) = "(" <> ppForm f <> " => " <> ppForm g <> ")"
ppForm (Iff f g) = "(" <> ppForm f <> " <=> " <> ppForm g <> ")"
ppForm (Fa vs f) = "! " <> ppList ft vs <> " : " <> ppForm f
ppForm (Ex vs f) = "? " <> ppList ft vs <> " : " <> ppForm f

-- ppForm' :: Form -> Builder
-- ppForm' (Eq t s) = "(" <> ppTerm t <> " = " <> ppTerm s <> ")"
-- ppForm' (Rel r xs) = ppFunct r <> ppArgs (L.map ppTerm xs)
-- ppForm' (Not f) = "~ " <> ppForm' f
-- ppForm' (And []) = "$true"
-- ppForm' (Or  []) = "$false"
-- ppForm' (And fs) = "(" <> ppInter " & " (L.map ppForm' fs) <> ")"
-- ppForm' (Or  fs) = "(" <> ppInter " | " (L.map ppForm' fs) <> ")"
-- ppForm' (Imp f g) = "(" <> ppForm' f <> " => " <> ppForm' g <> ")"
-- ppForm' (Iff f g) = "(" <> ppForm' f <> " <=> " <> ppForm' g <> ")"
-- ppForm' (Fa vs f) = "! " <> ppList ft vs <> " : " <> ppForm' f
-- ppForm' (Ex vs f) = "? " <> ppList ft vs <> " : " <> ppForm' f

ppFormNl :: Form -> Builder
ppFormNl f = ppInter "\n" (ppFormNlCore f) <> "\n"

ppFormNlCore :: Form -> [Builder]
ppFormNlCore f@(Eq _ _) = [ppForm f]
ppFormNlCore f@(Rel _ _) = [ppForm f]
ppFormNlCore (Not f) = "~" : L.map pad (ppFormNlCore f)
ppFormNlCore (And []) = ["$true"]
ppFormNlCore (Or  []) = ["$false"]
ppFormNlCore (And fs) = "/\\" : L.map pad (L.concatMap ppFormNlCore fs) 
ppFormNlCore (Or  fs) = "\\/" : L.map pad (L.concatMap ppFormNlCore fs)
ppFormNlCore (Imp f g) = "==>" : L.map pad (ppFormNlCore f ++ ppFormNlCore g)
ppFormNlCore (Iff f g) = "<=>" : L.map pad (ppFormNlCore f ++ ppFormNlCore g)
ppFormNlCore (Fa vs f) = ("! " <> ppList ft vs) : L.map pad (ppFormNlCore f)
ppFormNlCore (Ex vs f) = ("? " <> ppList ft vs) : L.map pad (ppFormNlCore f)

ppForms :: [Form] -> Builder
ppForms fs = ppInter "\n" $ L.map ppForm fs

ppObj :: [Form] -> Form -> Builder
ppObj fs g = ppForms fs <> "\n---------------------------------------------------------------------\n" <> ppForm g <> "\n"

-- ppSeq :: Seq -> Builder
-- ppSeq s = ppList ppForm $ S.toList s

ppGterm :: Gterm -> Builder
ppGterm (Gfun f ts) = ft f <> ppArgs (L.map ppGterm ts)
ppGterm (Glist ts) = ppList id $ L.map ppGterm ts
ppGterm (Gnum k) = ppInt k
ppGterm (Gvar v) = ft v

-- ppAnt :: Ant -> Builder
-- ppAnt Nothing = ""
-- ppAnt (Just t) = ppGterm t

-- ppAF :: AF -> String
-- ppAF (n, r, f, a) = printf "%s :: %s :: %s :: %s" n r (tlt $ ppForm f) "" -- (tlt $ ppAnt a)

ppEq :: (Term, Term) -> Builder
ppEq (x, y) = ppForm (Eq x y)

pad :: Builder -> Builder
pad t = "  " <> t

ppPrf :: Int -> Prf -> Builder
ppPrf k p = ppInter "\n" $ ppPrfCore k p

ppPrfCore :: Int -> Prf -> [Builder]
ppPrfCore 0 _ = ["..."]
ppPrfCore k (Id' f) = ["Id' : " <> ppForm f]
ppPrfCore k (NotT' f p) = ("Not-L : " <> ppForm (Not f)) : L.map pad (ppPrfCore (k - 1) p)
ppPrfCore k (NotF' f p) = ("Not-R : " <> ppForm (Not f)) : L.map pad (ppPrfCore (k - 1) p)
ppPrfCore k (Cut' f p0 p1) = ("Cut : " <> ppForm f) : L.map pad (ppPrfCore (k - 1) p0 ++ ppPrfCore (k - 1) p1)
ppPrfCore k (IffF' f g p0 p1) = ("Iff-R : " <> ppForm (f <=> g)) : L.map pad (ppPrfCore (k - 1) p0 ++ ppPrfCore (k - 1) p1)
ppPrfCore k (IffTO' f g p) = ("Iff-LO : " <> ppForm (f <=> g)) : L.map pad (ppPrfCore (k - 1) p)
ppPrfCore k (IffTR' f g p) = ("Iff-LR : " <> ppForm (f <=> g)) : L.map pad (ppPrfCore (k - 1) p)
ppPrfCore k (ImpT' f g p0 p1) = ("Imp-L : " <> ppForm (f ==> g)) : L.map pad (ppPrfCore (k - 1) p0 ++ ppPrfCore (k - 1) p1)
ppPrfCore k (ImpFC' f g p) = ("Imp-RC : " <> ppForm (f ==> g)) : L.map pad (ppPrfCore (k - 1) p)
ppPrfCore k (ImpFA' f g p) = ("Imp-RA : " <> ppForm (f ==> g)) : L.map pad (ppPrfCore (k - 1) p)
ppPrfCore k (Mrk s p) = ("Mark : " <> ft s) : L.map pad (ppPrfCore (k - 1) p)
ppPrfCore k (FunC' f xs ys) = ["Fun-C : ", "  f : " <> ppFunct f, "  xs : " <> ppList ppTerm xs, "  ys : " <> ppList ppTerm ys]
ppPrfCore k (RelC' r xs ys) = ["Rel-C : ", "  r : " <> ppFunct r, "  xs : " <> ppList ppTerm xs, "  ys : " <> ppList ppTerm ys]
ppPrfCore k (OrT' fps) = "Or-L" : L.map pad (L.concatMap (\ (f_, p_) -> ": " <> ppForm f_ : ppPrfCore (k - 1) p_) fps)
ppPrfCore k (OrF' fs fs' p) = ("Or-R : " <> ppForm (Or fs)) : L.map pad (ppPrfCore (k - 1) p)
ppPrfCore k (AndT' fs fs' p) = ("And-L : " <> ppForm (And fs)) : L.map pad (ppPrfCore (k - 1) p)
ppPrfCore k (AndF' fps) = "And-R" : L.map pad (L.concatMap (\ (f_, p_) -> ": " <> ppForm f_ : ppPrfCore (k - 1) p_) fps)
ppPrfCore k (EqS' x y) = ["Eq-S?"]
ppPrfCore k (EqR' x) = ["Eq-R : " <> ppTerm x]
ppPrfCore k (EqT' x y z) = ["Eq-T?"]
ppPrfCore k (FaT' vxs f p) =
  let (vs, xs) = unzip vxs in
  ("Fa-L : " : L.map (pad . ppMapping) vxs) ++ pad (ppForm (Fa vs f)) : L.map pad (ppPrfCore (k - 1) p)
ppPrfCore k (ExF' vxs f p) =
  let (vs, xs) = unzip vxs in
  ("Ex-R : " : L.map (pad . ppMapping) vxs) ++ pad (ppForm (Ex vs f)) : L.map pad (ppPrfCore (k - 1) p)
ppPrfCore k (FaF' vs m f p) = 
  let (_, vxs) = varPars m vs in
  ("Fa-R : " : L.map (pad . ppMapping) vxs) ++ pad (ppForm (Fa vs f)) : L.map pad (ppPrfCore (k - 1) p)
ppPrfCore k (ExT' vs m f p) = 
  let (_, vxs) = varPars m vs in
  ("Ex-L : " : L.map (pad . ppMapping) vxs) ++  pad (ppForm (Ex vs f)) : L.map pad (ppPrfCore (k - 1) p)
ppPrfCore k Open' = ["Open!"]

-- ppTake :: Int -> (a -> Builder) -> a -> Builder
-- ppTake k f x =
--   let t = f x in
--   let l = T.length t in
--   if l <= k
--   then t
--   else T.take k t <> "..."
-- 
-- ppTakeEq :: Int -> (Form, Form) -> Builder
-- ppTakeEq k (f, g) = ppTake k ppForm f <> " <-|-> " <> ppTake k ppForm g

-- ppSearchState :: VC -> [(Form, Form)] -> Builder
-- ppSearchState vc = ppListNl (ppTakeEq 40)

writeTerm :: Term -> Builder
writeTerm (Var x) = ft x
writeTerm (Fun f xs) = ppFunct f <> ppArgs (L.map writeTerm xs)

writeForm :: Form -> Builder
writeForm (Eq t s) = "(" <> writeTerm t <> " = " <> writeTerm s <> ")"
writeForm (Rel r xs) = ppFunct r <> ppArgs (L.map writeTerm xs)
writeForm (Not f) = "~ " <> writeForm f
writeForm (And []) = "$true"
writeForm (Or  []) = "$false"
writeForm (And fs) = "(" <> ppInter " & " (L.map writeForm fs) <> ")"
writeForm (Or  fs) = "(" <> ppInter " | " (L.map writeForm fs) <> ")"
writeForm (Imp f g) = "(" <> writeForm f <> " => " <> writeForm g <> ")"
writeForm (Iff f g) = "(" <> writeForm f <> " <=> " <> writeForm g <> ")"
writeForm (Fa vs f) = "! " <> ppList ft vs <> " : " <> writeForm f
writeForm (Ex vs f) = "? " <> ppList ft vs <> " : " <> writeForm f

ppElab :: Elab -> Builder
ppElab ((nm, sgn, f), i, Nothing) =  ppApp "fof" [ft nm, ppSign sgn, writeForm f, ppApp "inference" [ppInf i]] <> "."
ppElab ((nm, sgn, f), i, Just cmt) = ppApp "fof" [ft nm, ppSign sgn, writeForm f, ppApp "inference" [ppInf i], ppList ft [cmt]] <> "."

fmtAF :: AF -> Builder
fmtAF (nm, rl, f, Nothing) = ppApp "fof" [ft nm, ft rl, ppForm f]
fmtAF (nm, rl, f, Just (t, Nothing)) = ppApp "fof" [ft nm, ft rl, ppForm f, ppGterm t]
fmtAF (nm, rl, f, Just (t, Just ts)) = ppApp "fof" [ft nm, ft rl, ppForm f, ppGterm t, ppList ppGterm ts]

ppPath :: Path -> Builder 
ppPath (NewRel _ _) = "rel"
ppPath (NewFun _ _) = "fun"
ppPath NewEq = "eq"
ppPath (NewFa _) = "fa"
ppPath (NewEx _) = "ex"
ppPath NewImpL = "imp-l"
ppPath NewImpR = "imp-r"
ppPath NewIffL = "iff-l"
ppPath NewIffR = "iff-r"
ppPath (NewOr _ _) = "or"
ppPath (NewAnd _ _) = "and"
ppPath NewNot = "not"

ppSig :: Sig -> Builder
ppSig = ppHM (ppList ppPath) ppInt 
-- ppEP (k, l) = ppSQ $ ppInter ":" $ ppInt k : L.map (\ (m_, n_) -> ppInt m_ <> "." <> ppInt n_) l

-- ppSide :: Side -> Builder
-- ppSide Lft = "lft"
-- ppSide Rgt = "rgt"

ppInf :: Inf -> Builder
ppInf (Id n m) = ppApp "id" [ft n, ft m]
ppInf (Cut nf nt) = ppApp "cut" [ft nf, ft nt]
ppInf (FunC ns m) = ppApp "func" [ppList ft ns, ft m]
ppInf (RelC ns m n) = ppApp "relc" [ppList ft ns, ft m, ft n]
ppInf (EqR nm) = ppApp "eqr" [ft nm]
ppInf (EqS nm0 nm1) = ppApp "eqs" [ft nm0, ft nm1]
ppInf (EqT nm0 nm1 nm2) = ppApp "eqt" [ft nm0, ft nm1, ft nm2]
ppInf (NotT nh nc) = ppApp "nott" [ft nh, ft nc]
ppInf (NotF nh nc) = ppApp "notf" [ft nh, ft nc]

ppInf (OrT nh []) = ppApp "bott" [ft nh]
ppInf (OrT nh ns) = ppApp "ort" [ft nh, ppList ft ns]
ppInf (OrF nh k nc) =  ppApp "orf"  [ft nh, ppInt k, ft nc]

ppInf (AndT nh k nc) = ppApp "andt" [ft nh, ppInt k, ft nc]
ppInf (AndF nh []) = ppApp "topf" [ft nh]
ppInf (AndF nh ns) = ppApp "andf" [ft nh, ppList ft ns]

ppInf (ImpT nh n0 n1) = ppApp "impt" [ft nh, ft n0, ft n1]
ppInf (ImpFA nh nc) = ppApp "impfa" [ft nh, ft nc]
ppInf (ImpFC nh nc) = ppApp "impfc" [ft nh, ft nc]
ppInf (IffTR nh nc) = ppApp "ifftr" [ft nh, ft nc]
ppInf (IffTO nh nc) = ppApp "iffto" [ft nh, ft nc]
ppInf (IffF nh n0 n1) = ppApp "ifff" [ft nh, ft n0, ft n1]
ppInf (FaT nh xs nc) = ppApp "fat" [ft nh, ppList writeTerm xs, ft nc]
ppInf (FaF nh k nc) = ppApp "faf" [ft nh, ppInt k, ft nc]
ppInf (ExT nh k nc) = ppApp "ext" [ft nh, ppInt k, ft nc]
ppInf (ExF nh xs nc) = ppApp "exf" [ft nh, ppList writeTerm xs, ft nc]
ppInf (RelD nc) = ppApp "reld" [ft nc]
ppInf (AoC x nc) = ppApp "aoc" [ppTerm x, ft nc]
ppInf Open = "open"

ppDir :: Dir -> Builder
ppDir Obv = "obv"
ppDir Rev = "rev"

ppSign :: Bool -> Builder
ppSign True = "true"
ppSign False = "false"

ppStelab :: Stelab -> Builder
ppStelab (InfStep f p t) = ppInter "\n" $ ["InfStep", "f :" <> ppForm f, "prf :"] ++ ppPrfCore 20 p ++ ["Notes : " <> ft t]
ppStelab (DefStep f g _ t) = "rdef : " <> ppForm f <> " |- " <> ppForm g <> "\nNotes : " <> ft t
ppStelab (AoCStep xs _ _ _ t) = "AoC :\nxs : " <> ppListNl ppTerm xs <> "\nNotes : " <> ft t

ppSignForm :: (Bool, Form) -> Builder
ppSignForm (True, f) = "[T] " <> ppForm f
ppSignForm (False, f) = "[F] " <> ppForm f

ppStep :: Step -> Builder
ppStep (n, r, ns, f) = 
  ft n <> " :: " <>
  ft r <> " :: " <>
  ppList ft ns <> " :: " <> 
  ppForm f <> "\n"

ppNL :: (a -> Builder) -> (a -> Builder)
ppNL p x = p x <> "\n"


-- Serialization

serInt :: Int -> Builder
serInt k = ppInt k <> "."

serText :: Text -> Builder
serText tx = ft tx <> "."

serSign :: Bool -> Builder
serSign True = "T"
serSign False = "F"

serList :: (a -> Builder) -> [a] -> Builder
serList _ [] = "."
serList s (x : xs) = "," <> s x <> serList s xs

serFunct :: Funct -> Builder
serFunct (Reg t) = serText t
serFunct (Idx k) = "#" <> serInt k

serTerm :: Term -> Builder
serTerm (Var v) = "$" <> serText v
serTerm (Fun f xs) = "@" <> serFunct f <> serList serTerm xs

serForm :: Form -> Builder
serForm (Eq x y) = "=" <> serTerm x <> serTerm y
serForm (Rel r xs) = "@" <> serFunct r <> serList serTerm xs
serForm (Not f) = "~" <> serForm f
serForm (And fs) = "&" <> serList serForm fs
serForm (Or fs)  = "|" <> serList serForm fs
serForm (Imp f g) = ">" <> serForm f <> serForm g
serForm (Iff f g) = "^" <> serForm f <> serForm g
serForm (Fa vs f) = "!" <> serList serText vs <> serForm f
serForm (Ex vs f) = "?" <> serList serText vs <> serForm f

serNodeName :: NodeInfo -> Builder
serNodeName (nm, _, _) = serText nm 

serProof' :: Proof -> Builder
serProof' p = ft (proofRN p) <> serProof p

serSignForm :: (Bool, Form) -> Builder
serSignForm (b, f) = serSign b <> serForm f

serProof :: Proof -> Builder
serProof (Id_ _ nt nf) = "I" <> serText nt <> serText nf
serProof (Cut_ _ pf pt) = 
  case (proofRSF pf, proofRSF pt) of 
    ((False, f), (True, f')) -> 
      if f == f' 
        then "C" <> serForm f <> serProof pf <> serProof pt
        else error "Cut formulas do not match"
    _ -> error "Cut formulas do not have correct signs"
serProof (RelD_ _ p) = "D" <> serSignForm (proofRSF p) <> serProof p
serProof (AoC_ _ x p) = "A" <> serTerm x <> serSignForm (proofRSF p) <> serProof p  
serProof (Open_ _) = "O" 
serProof (FunC_ _ nts nf) = "F" <> serList serText nts <> serText nf
serProof (RelC_ _ nts nt nf) = "R" <> serList serText nts <> serText nt <> serText nf
serProof (EqR_ ni nf) = "=R" <>  serText nf
serProof (EqS_ ni nt nf) = "=S" <>  serText nt <> serText nf
serProof (EqT_ ni nxy nyz nxz) = "=T" <>  serText nxy <> serText nyz <> serText nxz
serProof (NotT_ ni nm p) = "~T" <> serText nm <> serProof p
serProof (NotF_ ni nm p) = "~F" <> serText nm <> serProof p
serProof (OrT_ ni nm ps) = "|T" <>  serText nm <> serList serProof ps
serProof (OrF_ ni nm k p) = "|F" <>  serText nm <> serInt k <> serProof p
serProof (AndT_ ni nm k p) = "&T" <>  serText nm <> serInt k <> serProof p
serProof (AndF_ ni nm ps) = "&F" <>  serText nm <> serList serProof ps
serProof (ImpT_ ni nm pa pc) = ">T" <>  serText nm <> serProof pa <> serProof pc
serProof (ImpFA_ ni nm p) = ">FA" <>  serText nm <> serProof p
serProof (ImpFC_ ni nm p) = ">FC" <>  serText nm <> serProof p
serProof (IffTO_ ni nm p) = "^TO" <>  serText nm <> serProof p
serProof (IffTR_ ni nm p) = "^TR" <>  serText nm <> serProof p
serProof (IffF_ ni nm po pr) = "^F" <>  serText nm <> serProof po <> serProof pr
serProof (FaT_ ni nm xs p) = "!T" <>  serText nm <> serList serTerm xs <> serProof p
serProof (FaF_ ni nm k p) = "!F" <>  serText nm <> serInt k <> serProof p
serProof (ExT_ ni nm k p) = "?T" <>  serText nm <> serInt k <> serProof p
serProof (ExF_ ni nm xs p) = "?F" <>  serText nm <> serList serTerm xs <> serProof p
