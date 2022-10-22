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


ppLrat :: Lrat -> Builder
ppLrat (Del k ks) = ppInt k  <> ". Del " <> ppInter " " (L.map ppInt ks)
ppLrat (Add k fs ms) = ppInt k  <> ". Add " <> ppInter " " (L.map ppForm fs) <> ", Using : " <> ppInter  " " (L.map ppInt ms)

-- ppMapping :: (Text, Term) -> Builder
-- ppMapping (t, x) = t <> " |-> " <> ppTerm x

-- ppVmap :: (Text, Text) -> Builder
-- ppVmap (v, w) = v <> " <-|-> " <> w
-- 
-- ppVM :: VM -> Builder
-- ppVM gm = ppListNl (\ (v, x) -> v <> " |-> " <> ppTerm x) (HM.toList gm)
-- 
-- ppVR :: VR -> Builder
-- ppVR (vw, _) = ppListNl ppVmap (HM.toList vw)
-- 
-- ppVCAux :: HM.Map Text (Set Text) -> Builder
-- ppVCAux vw = ppListNl (\ (v_, ws_) -> v_ <> " |-> " <> ppList id (S.toList ws_)) (HM.toList vw)
-- 
-- ppVC :: VC -> Builder
-- ppVC (vws, wvs) = ppVCAux vws <> "-------------------------------------\n" <> ppVCAux wvs

-- ft :: Data.Text.Internal.Lazy.Text -> Builder

ppInter :: Builder -> [Builder] -> Builder
ppInter b [] = mempty
ppInter b [x] = x
ppInter b (x : xs) = x <> b <> ppInter b xs

ppList :: (a -> Builder) -> [a] -> Builder
ppList f xs = "[" <> ppInter ", " (L.map f xs) <> "]"

ppSet :: (a -> Builder) -> Set a -> Builder
ppSet f s = ppList f (S.toList s)

ppListNl :: (a -> Builder) -> [a] -> Builder
ppListNl f xs = mconcat (L.map (\ x -> f x <> "\n") xs)

ppTerm :: Term -> Builder
-- ppTerm (Par k) = "#" <> ppInt k
ppTerm (Var x) = ft x
ppTerm (Fun "" []) = "·"
ppTerm (Fun f xs) = ft f <> ppArgs (L.map ppTerm xs)

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

ppForm :: Form -> Builder
ppForm (Eq t s) = "(" <> ppTerm t <> " = " <> ppTerm s <> ")"
ppForm (Rel r xs) = ft r <> ppArgs (L.map ppTerm xs)
ppForm (Not f) = "~ " <> ppForm f
ppForm (And []) = "$true"
ppForm (Or  []) = "$false"
ppForm (And fs) = "(" <> ppInter " & " (L.map ppForm fs) <> ")"
ppForm (Or  fs) = "(" <> ppInter " | " (L.map ppForm fs) <> ")"
ppForm (Imp f g) = "(" <> ppForm f <> " => " <> ppForm g <> ")"
ppForm (Iff f g) = "(" <> ppForm f <> " <=> " <> ppForm g <> ")"
ppForm (Fa vs f) = "! " <> ppList ft vs <> " : " <> ppForm f
ppForm (Ex vs f) = "? " <> ppList ft vs <> " : " <> ppForm f

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

ppSeq :: Seq -> Builder
ppSeq s = ppList ppForm $ S.toList s

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

ppProof :: Int -> Prf -> Builder
ppProof k p = ppInter "\n" $ ppPrf k p

ppPrf :: Int -> Prf -> [Builder]
ppPrf 0 _ = ["..."]
ppPrf k (Id' f) = ["Id' : " <> ppForm f]
ppPrf k (NotT' f p) = ("Not-L : " <> ppForm (Not f)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (NotF' f p) = ("Not-R : " <> ppForm (Not f)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (Cut' f p0 p1) = ("Cut : " <> ppForm f) : L.map pad (ppPrf (k - 1) p0 ++ ppPrf (k - 1) p1)
ppPrf k (IffF' f g p0 p1) = ("Iff-R : " <> ppForm (f <=> g)) : L.map pad (ppPrf (k - 1) p0 ++ ppPrf (k - 1) p1)
ppPrf k (IffTO' f g p) = ("Iff-LO : " <> ppForm (f <=> g)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (IffTR' f g p) = ("Iff-LR : " <> ppForm (f <=> g)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (ImpT' f g p0 p1) = ("Imp-L : " <> ppForm (f ==> g)) : L.map pad (ppPrf (k - 1) p0 ++ ppPrf (k - 1) p1)
ppPrf k (ImpFC' f g p) = ("Imp-RC : " <> ppForm (f ==> g)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (ImpFA' f g p) = ("Imp-RA : " <> ppForm (f ==> g)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (Mrk s p) = ("Mark : " <> ft s) : L.map pad (ppPrf (k - 1) p)
ppPrf k (FunC' f xs ys) = ["Fun-C : ", "  f : " <> ft f, "  xs : " <> ppList ppTerm xs, "  ys : " <> ppList ppTerm ys]
ppPrf k (RelC' r xs ys) = ["Rel-C : ", "  r : " <> ft r, "  xs : " <> ppList ppTerm xs, "  ys : " <> ppList ppTerm ys]
ppPrf k (OrT' fps) = "Or-L" : L.map pad (L.concatMap (\ (f_, p_) -> ": " <> ppForm f_ : ppPrf (k - 1) p_) fps)
ppPrf k (OrF' fs fs' p) = ("Or-R : " <> ppForm (Or fs)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (AndT' fs fs' p) = ("And-L : " <> ppForm (And fs)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (AndF' fps) = "And-R" : L.map pad (L.concatMap (\ (f_, p_) -> ": " <> ppForm f_ : ppPrf (k - 1) p_) fps)
ppPrf k (EqS' x y) = ["Eq-S?"]
ppPrf k (EqR' x) = ["Eq-R : " <> ppTerm x]
ppPrf k (EqT' x y z) = ["Eq-T?"]
ppPrf k (FaT' vxs f p) =
  let (vs, xs) = unzip vxs in
  ("Fa-L : " <> ppForm (Fa vs f)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (ExF' vxs f p) =
  let (vs, xs) = unzip vxs in
  ("Ex-R : " <> ppForm (Ex vs f)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (FaF' vs m f p) = 
  "Fa-R : " <> ppForm (Fa vs f) : L.map pad (ppPrf (k - 1) p)
ppPrf k (ExT' vs m f p) = 
  "Ex-L : " <> ppForm (Ex vs f) : L.map pad (ppPrf (k - 1) p)
ppPrf k Asm = ["Asm!"]

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
writeTerm (Fun f xs) = ft f <> ppArgs (L.map writeTerm xs)

writeForm :: Form -> Builder
writeForm (Eq t s) = "(" <> writeTerm t <> " = " <> writeTerm s <> ")"
writeForm (Rel r xs) = ft r <> ppArgs (L.map writeTerm xs)
writeForm (Not f) = "~ " <> writeForm f
writeForm (And []) = "$true"
writeForm (Or  []) = "$false"
writeForm (And fs) = "(" <> ppInter " & " (L.map writeForm fs) <> ")"
writeForm (Or  fs) = "(" <> ppInter " | " (L.map writeForm fs) <> ")"
writeForm (Imp f g) = "(" <> writeForm f <> " => " <> writeForm g <> ")"
writeForm (Iff f g) = "(" <> writeForm f <> " <=> " <> writeForm g <> ")"
writeForm (Fa vs f) = "! " <> ppList ft vs <> " : " <> writeForm f
writeForm (Ex vs f) = "? " <> ppList ft vs <> " : " <> writeForm f

writeEF :: EF -> Builder
writeEF (ep, sgn, f, k, i, Nothing) = ppApp "fof" [ppEP ep, ppSign sgn, writeForm f, ppApp "inference" [ppInt k, ppInf i]] <> "."
writeEF (ep, sgn, f, k, i, Just cmt) = ppApp "fof" [ppEP ep, ppSign sgn, writeForm f, ppApp "inference" [ppInt k, ppInf i], ppList ft [cmt]] <> "."

fmtAF :: AF -> Builder
fmtAF (nm, rl, f, Nothing) = ppApp "fof" [ft nm, ft rl, ppForm f]
fmtAF (nm, rl, f, Just (t, Nothing)) = ppApp "fof" [ft nm, ft rl, ppForm f, ppGterm t]
fmtAF (nm, rl, f, Just (t, Just ts)) = ppApp "fof" [ft nm, ft rl, ppForm f, ppGterm t, ppList ppGterm ts]

ppEP :: EP -> Builder
ppEP (k, l) = ppSQ $ ppInter ":" $ ppInt k : L.map (\ (m_, n_) -> ppInt m_ <> "." <> ppInt n_) l

-- ppSide :: Side -> Builder
-- ppSide Lft = "lft"
-- ppSide Rgt = "rgt"

ppInf :: Inf -> Builder
ppInf Cut = "cut"
ppInf (Id n m) = ppApp "id" [ft n, ft m]
ppInf (FunC ns m) = ppApp "func" [ppList ft ns, ft m]
ppInf (RelC ns m n) = ppApp "relc" [ppList ft ns, ft m, ft n]
ppInf (EqR nm) = ppApp "eqr" [ft nm]
ppInf (EqS nm0 nm1) = ppApp "eqs" [ft nm0, ft nm1]
ppInf (EqT nm0 nm1 nm2) = ppApp "eqt" [ft nm0, ft nm1, ft nm2]
ppInf (NotT nm) = ppApp "nott" [ft nm]
ppInf (NotF nm) = ppApp "notf" [ft nm]
ppInf (ImpFA nm) = ppApp "impfa" [ft nm]
ppInf (ImpFC nm) = ppApp "impfc" [ft nm]
ppInf (IffTR nm) = ppApp "ifftr" [ft nm]
ppInf (IffTO nm) = ppApp "iffto" [ft nm]
ppInf (IffF n) = ppApp "ifff" [ft n]
ppInf (ImpT n) = ppApp "impt" [ft n]
ppInf (OrF n) = ppApp "orf" [ft n]
ppInf (AndT n) = ppApp "andt" [ft n]
ppInf (OrT n) = ppApp "ort" [ft n]
ppInf (AndF n) = ppApp "andf" [ft n]
ppInf (FaT nm xs) = ppApp "fat" [ft nm, ppList writeTerm xs]
ppInf (FaF nm k) = ppApp "faf" [ft nm, ppInt k]
ppInf (ExT nm k) = ppApp "ext" [ft nm, ppInt k]
ppInf (ExF nm xs) = ppApp "exf" [ft nm, ppList writeTerm xs]
ppInf RelD = "reld" 
ppInf (AoC xs) = ppApp "aoc" $ L.map ppTerm xs
ppInf Open = "open"

ppDir :: Dir -> Builder
ppDir Obv = "obv"
ppDir Rev = "rev"

ppSign :: Bool -> Builder
ppSign True = "true"
ppSign False = "false"

ppElab :: Stelab -> Builder
ppElab (Plab f p t) = ppInter "\n" $ ["Plab", "f :" <> ppForm f, "prf :"] ++ ppPrf 20 p ++ ["Notes : " <> ft t]
ppElab (RelD' f g _ t) = "rdef : " <> ppForm f <> " |- " <> ppForm g <> "\nNotes : " <> ft t
ppElab (AoC' xs _ _ _ t) = "AOC :\nxs : " <> ppListNl ppTerm xs <> "\nNotes : " <> ft t


ppSignForm :: (Form, Bool) -> Builder
ppSignForm (f, True) = ppForm f <> " |-"
ppSignForm (f, False) = "|- " <> ppForm f

ppStep :: Step -> Builder
ppStep (n, r, ns, f) = 
  ft n <> " :: " <>
  ft r <> " :: " <>
  ppList ft ns <> " :: " <> 
  ppForm f <> "\n"
