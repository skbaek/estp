{-# LANGUAGE OverloadedStrings #-}

module PP where

import Data.List as L
import Data.Text as T ( concat, intercalate, pack, Text, length, take )
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

ppVmap :: (Text, Text) -> Text
ppVmap (v, w) = v <> " |-> " <> w

ppVM :: VM -> Text
ppVM gm = ppListNl (\ (v, x) -> v <> " |-> " <> ppTerm x) (HM.toList gm)

ppVR :: VR -> Text
ppVR (vw, _) = ppListNl ppVmap (HM.toList vw)

ppVCAux :: HM.Map Text (Set Text) -> Text
ppVCAux vw = ppListNl (\ (v_, ws_) -> v_ <> " |-> " <> ppList id (S.toList ws_)) (HM.toList vw)

ppVC :: VC -> Text
ppVC (vws, wvs) = ppVCAux vws <> "-------------------------------------\n" <> ppVCAux wvs

ppList :: (a -> Text) -> [a] -> Text
ppList f xs = "[" <> T.intercalate ", " (L.map f xs) <> "]"

ppSet :: (a -> Text) -> Set a -> Text
ppSet f s = ppList f (S.toList s)

ppListNl :: (a -> Text) -> [a] -> Text
ppListNl f xs = T.concat (L.map (\ x -> f x <> "\n") xs)

ppTerm :: Term -> Text
ppTerm (Par k) =  "#" <> ppInt k
ppTerm (Var x) = x
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

ppFormNl :: Form -> Text
ppFormNl f = T.intercalate "\n" (ppFormNl' f) <> "\n"

ppFormNl' :: Form -> [Text]
ppFormNl' (Eq t s) = [ppTerm t <> " = " <> ppTerm s]
ppFormNl' (Rel r xs) = [r <> ppArgs (L.map ppTerm xs)]
ppFormNl' (Not f) = "~" : L.map pad (ppFormNl' f)
ppFormNl' (And []) = ["$true"]
ppFormNl' (Or  []) = ["$false"]
ppFormNl' (And fs) = "/\\" : L.map pad (L.concatMap ppFormNl' fs) 
ppFormNl' (Or  fs) = "\\/" : L.map pad (L.concatMap ppFormNl' fs)
ppFormNl' (Imp f g) = "==>" : L.map pad (ppFormNl' f ++ ppFormNl' g)
ppFormNl' (Iff f g) = "<=>" : L.map pad (ppFormNl' f ++ ppFormNl' g)
ppFormNl' (Fa vs f) = ("! " <> ppList id vs) : L.map pad (ppFormNl' f)
ppFormNl' (Ex vs f) = ("? " <> ppList id vs) : L.map pad (ppFormNl' f)

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

ppAnForm :: AnForm -> String
ppAnForm (Af n f a) = printf "%s :: %s :: %s" n (ppForm f) (ppAnt a)

ppEq :: (Term, Term) -> Text
ppEq (x, y) = ppForm (Eq x y)

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
ppPrf k (ImpRA f g p) = ("Imp-RA : " <> ppForm (f ==> g)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (Mrk s p) = ("Mark : " <> pack s) : L.map pad (ppPrf (k - 1) p)
ppPrf k (FunC _ _) = ["Fun-C?"]
ppPrf k (RelC _ _) = ["Rel-C?"]
ppPrf k (OrL fps) = "Or-L" : L.map pad (L.concatMap (\ (f_, p_) -> ": " <> ppForm f_ : ppPrf (k - 1) p_) fps)
ppPrf k (OrR fs fs' p) = ("Or-R : " <> ppForm (Or fs)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (AndL fs fs' p) = ("And-L : " <> ppForm (And fs)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (AndR _) = ["And-R?"]
ppPrf k (EqC x y) = ["Eq-C?"]
ppPrf k (EqS x y) = ["Eq-S?"]
ppPrf k (EqR x) = ["Eq-R?"]
ppPrf k (EqT x y z) = ["Eq-T?"]
ppPrf k (FaL vxs f p) =
  let (vs, xs) = unzip vxs in
  ("Fa-L : " <> ppForm (Fa vs f)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (ExR vxs f p) =
  let (vs, xs) = unzip vxs in
  ("Ex-R : " <> ppForm (Ex vs f)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (FaR vs m f p) = ["Fa-R?"]
ppPrf k (ExL vs m f p) = ["Ex-L?"]
ppPrf k Asm = ["Asm!"]

ppTake :: Int -> (a -> Text) -> a -> Text
ppTake k f x =
  let t = f x in
  let l = T.length t in
  if l <= k
  then t
  else T.take k t <> "..."

ppTakeEq :: Int -> (Form, Form) -> Text
ppTakeEq k (f, g) = ppTake k ppForm f <> " <-|-> " <> ppTake k ppForm g

ppSearchState :: VC -> [(Form, Form)] -> Text
ppSearchState vc = ppListNl (ppTakeEq 40)