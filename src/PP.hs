{-# LANGUAGE OverloadedStrings #-}

module PP where

import Data.List as L
import Data.Text as T ( concat, intercalate, pack, Text )
import qualified Data.Text.Lazy as TL (toStrict, intercalate)
import Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList )
import Text.Printf
import Data.Map as HM

import Types

ppLrat :: Lrat -> Text
ppLrat (Del k ks) = ppInt k  <> ". Del " <> T.intercalate " " (L.map ppInt ks)
ppLrat (Add k fs ms) = ppInt k  <> ". Add " <> T.intercalate " " (L.map ppForm fs) <> ", Using : " <> T.intercalate  " " (L.map ppInt ms)

ppInt :: Integral a => a -> Text
ppInt = TL.toStrict . B.toLazyText . B.decimal

ppMapping :: (Text, Term) -> Text
ppMapping (t, x) = t <> " |-> " <> ppTerm x

ppList :: (a -> Text) -> [a] -> Text
ppList f xs = "[" <> T.intercalate ", " (L.map f xs) <> "]"

ppListNl :: (a -> Text) -> [a] -> Text
ppListNl f xs = T.concat (L.map (\ x -> f x <> "\n") xs)

ppTerm :: Term -> Text
ppTerm (Fv k) = "$" <> ppInt k
ppTerm (Par k) =  "#" <> ppInt k
ppTerm (Bv x) = x
ppTerm (Fun "" []) = "·"
ppTerm (Fun f xs) = f <> ppArgs (L.map ppTerm xs)

ppJct :: Bool -> String
ppJct True  = " /\\ "
ppJct False = " \\/ "

ppArgs :: [Text] -> Text
ppArgs [] = ""
ppArgs ts = "(" <> T.intercalate ", " ts <> ")"

ppQtf :: Bool -> Text
ppQtf True  = "!"
ppQtf False = "?"

ppForm :: Form -> Text
ppForm (Eq t s) = "(" <> ppTerm t <> " = " <> ppTerm s <> ")"
ppForm (Rel r xs) = r <> ppArgs (L.map ppTerm xs)
ppForm (Not f) = "~ " <> ppForm f
ppForm (And []) = "$true"
ppForm (Or  []) = "$false"
ppForm (And fs) = "(" <> T.intercalate " /\\ " (L.map ppForm fs) <> ")"
ppForm (Or  fs) = "(" <> T.intercalate " \\/ " (L.map ppForm fs) <> ")"
ppForm (Imp f g) = "(" <> ppForm f <> " => " <> ppForm g <> ")"
ppForm (Iff f g) = "(" <> ppForm f <> " <=> " <> ppForm g <> ")"
ppForm (Fa vs f) = "! " <> ppList id vs <> " : " <> ppForm f
ppForm (Ex vs f) = "? " <> ppList id vs <> " : " <> ppForm f

ppForms :: [Form] -> Text
ppForms fs = T.intercalate "\n" $ L.map ppForm fs

ppObj :: [Form] -> Form -> Text
ppObj fs g = ppForms fs <> "\n---------------------------------------------------------------------\n" <> ppForm g <> "\n"

ppSeq :: Seq -> Text
ppSeq s = ppList ppForm $ S.toList s

ppGterm :: Gterm -> Text
ppGterm (Gfun f ts) = f <> ppArgs (L.map ppGterm ts)
ppGterm (Glist ts) = ppList id $ L.map ppGterm ts

ppAnt :: Ant -> Text
ppAnt Nothing  = ""
ppAnt (Just t) = ppGterm t

ppPrvGoal :: PrvGoal -> Text
ppPrvGoal (f, g, _) = ppForm f <> " |- " <> ppForm g

-- ppInput :: Input -> String
-- ppInput (Inc s) = printf "include(%s)" s
-- ppInput (Cnf n f a) = printf "cnf(%s, %s%s)" n (ppForm f) (ppAnt a)
-- ppInput (Fof n f a) = printf "fof(%s, %s%s)" n (ppForm f) (ppAnt a)

ppAnForm :: AnForm -> String
ppAnForm (Af n f a) = printf "%s :: %s :: %s" n (ppForm f) (ppAnt a)

ppEq :: (Term, Term) -> Text
ppEq (x, y) = ppForm (Eq x y)

ppEqGoal :: EqGoal -> Text
ppEqGoal (x, y, _) = ppForm (Eq x y)
-- ppProb :: Prob -> String
-- ppProb ips = L.intercalate "\n" (L.map ppInput ips)

ppBnd :: Bnd -> Text
ppBnd b = ppList (\ (k, x) -> "$" <> ppInt k <> " |-> " <> ppTerm x) (HM.toList b)

pad :: Text -> Text
pad t = "  " <> t

ppProof :: Int -> Prf -> Text
ppProof k p = T.intercalate "\n" $ ppPrf k p

ppPrf :: Int -> Prf -> [Text]
ppPrf 0 _ = ["..."]
ppPrf k (Ax f) = ["Ax : " <> ppForm f]
ppPrf k (NotL f p) = ("Not-L : " <> ppForm (Not f)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (NotR f p) = ("Not-R : " <> ppForm (Not f)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (Cut f p0 p1) = ("Cut : " <> ppForm f) : L.map pad (ppPrf (k - 1) p0 ++ ppPrf (k - 1) p1)
ppPrf k (IffR f g p0 p1) = ("Iff-R : " <> ppForm (f <=> g)) : L.map pad (ppPrf (k - 1) p0 ++ ppPrf (k - 1) p1)
ppPrf k (IffLO f g p) = ("Iff-LO : " <> ppForm (f <=> g)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (IffLR f g p) = ("Iff-LR : " <> ppForm (f <=> g)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (ImpL f g p0 p1) = ("Imp-L : " <> ppForm (f ==> g)) : L.map pad (ppPrf (k - 1) p0 ++ ppPrf (k - 1) p1)
ppPrf k (ImpRC f g p) = ("Imp-RC : " <> ppForm (f ==> g)) : L.map pad (ppPrf (k - 1) p)
ppPrf _ _ = ["?"]

ppPr' :: Pr -> [Text]
ppPr' (Open f g) = [ppForm f <> " <-|-> " <> ppForm g]
ppPr' (ExP vs f ws g p) =
  [
    "ExP:",
    "  f = " <> ppForm (Fa vs f),
    "  g = " <> ppForm (Fa ws g)
  ] ++ L.map pad (ppPr' p)
ppPr' (FaP vs f ws g p) =
  [
    "FaP:",
    "  f = " <> ppForm (Fa vs f),
    "  g = " <> ppForm (Fa ws g)
  ] ++ L.map pad (ppPr' p)
ppPr' (NotP p) = "NotP :" : L.map pad (ppPr' p)
ppPr' (Clos fd _) = "Clos : " : L.map pad (ppFD' fd)
ppPr' (EqP Obv xl xr yl yr) = ["EqP:" <> ppForm (Eq xl xr) <> " =|= " <> ppForm (Eq yl yr)]
ppPr' (EqP Rev xl xr yl yr) = ["EqP:" <> ppForm (Eq xl xr) <> " =|= " <> ppForm (Eq yr yl)]
ppPr' (ImpP pl pr) = "ImpP :" : L.map pad (ppPr' pl ++ ppPr' pr)
ppPr' (IffP pl pr) = "IffP :" : L.map pad (ppPr' pl ++ ppPr' pr)
ppPr' (OrP fs gs pgs) =
  [
    "OrP :",
    "  fs : " <> ppForms fs,
    "  gs : " <> ppForms gs
  ] ++ L.map pad (concatMap (ppPr' . fst) pgs)
ppPr' AndP {} = ["AndP?"]
ppPr' (TransP pl g pr) = "TransP" : L.map pad (ppPr' pl ++ ["Mid : " <> ppForm g] ++ ppPr' pr)

ppPr :: Pr -> Text
ppPr pr = T.intercalate "\n" $ ppPr' pr

ppFD :: FD -> Text
ppFD fd = T.intercalate "\n" $ ppFD' fd

ppFD' :: FD -> [Text]
ppFD' AxFD = ["Ax"]
ppFD' (RWFD Obv f) = ["====> " <> ppForm f]
ppFD' (RWFD Rev f) = ["<==== " <> ppForm f]
ppFD' (NotFD fd) = "NotFD : " : L.map pad (ppFD' fd)
ppFD' (TransFD fdl f fdr) = 
  "TransFD : " : L.map pad (ppFD' fdl ++ ["Mid : " <> ppForm f] ++ ppFD' fdr)
ppFD' (AndFD fds) = "AndFD:" : L.map pad (L.concatMap ppFD' fds)
ppFD' (OrFD fds) = "OrFD:" : L.map pad (L.concatMap ppFD' fds)
ppFD' (ImpFD fdl fdr) = "ImpFD : " : L.map pad (ppFD' fdl ++ ppFD' fdr)
ppFD' (IffFD fdl fdr) = "IffFD : " : L.map pad (ppFD' fdl ++ ppFD' fdr)
ppFD' (RelFD _) = ["?"]
ppFD' (EqFD _ _) = ["?"]
ppFD' PermFD = ["PermFD"]
ppFD' WrapFD = ["WrapFD"]
ppFD' DropFD = ["DropFD"]
-- ppFD' SymFD = ["SymFD"]
ppFD' ConstFD = ["ConstFD?"]
ppFD' (FaFD vs p) = "FaFD :" : L.map pad (ppFD' p)
ppFD' (ExFD _ _) = ["ExFD?"]
ppFD' DNFD = ["DNFD"]
ppFD' AlphaFD = ["AlphaFD"]
