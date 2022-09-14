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

ppMapping :: (Text, Term) -> Text
ppMapping (t, x) = t <> " |-> " <> ppTerm x

ppVmap :: (Text, Text) -> Text
ppVmap (v, w) = v <> " <-|-> " <> w

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
-- ppTerm (Par k) = "#" <> ppInt k
ppTerm (Var x) = x
ppTerm (Fun "" []) = "·"
ppTerm (Fun f xs) = f <> ppArgs (L.map ppTerm xs)

ppJct :: Bool -> String
ppJct True  = " /\\ "
ppJct False = " \\/ "

ppApp :: Text -> [Text] -> Text
ppApp f ts = f <> ppArgs ts

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
ppForm (And fs) = "(" <> T.intercalate " & " (L.map ppForm fs) <> ")"
ppForm (Or  fs) = "(" <> T.intercalate " | " (L.map ppForm fs) <> ")"
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
ppGterm (Gnum k) = ppInt k

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
ppPrf k (Mrk s p) = ("Mark : " <> s) : L.map pad (ppPrf (k - 1) p)
ppPrf k (FunC _ _) = ["Fun-C?"]
ppPrf k (RelC _ _) = ["Rel-C?"]
ppPrf k (OrL fps) = "Or-L" : L.map pad (L.concatMap (\ (f_, p_) -> ": " <> ppForm f_ : ppPrf (k - 1) p_) fps)
ppPrf k (OrR fs fs' p) = ("Or-R : " <> ppForm (Or fs)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (AndL fs fs' p) = ("And-L : " <> ppForm (And fs)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (AndR fps) = "And-R" : L.map pad (L.concatMap (\ (f_, p_) -> ": " <> ppForm f_ : ppPrf (k - 1) p_) fps)
-- ppPrf k (EqC w x y z) = ["Eq-C?"]
ppPrf k (EqS x y) = ["Eq-S?"]
ppPrf k (EqR x) = ["Eq-R : " <> ppTerm x]
ppPrf k (EqT x y z) = ["Eq-T?"]
ppPrf k (FaL vxs f p) =
  let (vs, xs) = unzip vxs in
  ("Fa-L : " <> ppForm (Fa vs f)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (ExR vxs f p) =
  let (vs, xs) = unzip vxs in
  ("Ex-R : " <> ppForm (Ex vs f)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (FaR vs m f p) = 
  "Fa-R : " <> ppForm (Fa vs f) : L.map pad (ppPrf (k - 1) p)
ppPrf k (ExL vs m f p) = 
  "Ex-L : " <> ppForm (Ex vs f) : L.map pad (ppPrf (k - 1) p)
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

writeTerm :: Term -> Text
writeTerm (Var x) = x
writeTerm (Fun f xs) = f <> ppArgs (L.map writeTerm xs)

writeForm :: Form -> Text
writeForm (Eq t s) = "(" <> writeTerm t <> " = " <> writeTerm s <> ")"
writeForm (Rel r xs) = r <> ppArgs (L.map writeTerm xs)
writeForm (Not f) = "~ " <> writeForm f
writeForm (And []) = "$true"
writeForm (Or  []) = "$false"
writeForm (And fs) = "(" <> T.intercalate " & " (L.map writeForm fs) <> ")"
writeForm (Or  fs) = "(" <> T.intercalate " | " (L.map writeForm fs) <> ")"
writeForm (Imp f g) = "(" <> writeForm f <> " => " <> writeForm g <> ")"
writeForm (Iff f g) = "(" <> writeForm f <> " <=> " <> writeForm g <> ")"
writeForm (Fa vs f) = "! " <> ppList id vs <> " : " <> writeForm f
writeForm (Ex vs f) = "? " <> ppList id vs <> " : " <> writeForm f

writeEF :: EF -> Text
writeEF (f, sd, ep, k, i) = ppApp "fof" [ppEP ep, "plain", writeForm f, ppApp "tab" [ppPol sd, ppInt k, ppInf i]] <> "."

ppEP :: EP -> Text
-- ppEP (k, []) = ppSQ $ ppInt k
--ppEP (k, l) = ppSQ $ T.intercalate ":" (L.map (\ (m_, n_) -> ppInt n_ <> "." <> ppInt m_) (L.reverse l)) <> ":" <> ppInt k
ppEP (k, l) = ppSQ $ T.intercalate ":" $ ppInt k : L.map (\ (m_, n_) -> ppInt m_ <> "." <> ppInt n_) l

ppSide :: Side -> Text
ppSide Lft = "lft"
ppSide Rgt = "rgt"

ppInf :: Inf -> Text
ppInf InfCut = "cut"
ppInf (InfEqR nm) = ppApp "eqr" [nm]
ppInf (InfEqS nm0 nm1) = ppApp "eqr" [nm0, nm1]
ppInf (InfEqT nm0 nm1 nm2) = ppApp "eqr" [nm0, nm1, nm2]
ppInf (InfEqC nm0 nm1 nm2) = ppApp "eqr" [nm0, nm1, nm2]
-- ppInf (InfFunC Text [Text]) = _
-- ppInf (InfRelC Text [Text]) = _
ppInf (InfNotL nm) = ppApp "notl" [nm]
ppInf (InfNotR nm) = ppApp "notr" [nm]
-- ppInf (InfOrR Text Int) = _
-- ppInf (InfAndL Text Int) = _
ppInf (InfImpR n sd) = ppApp "impr" [n, ppSide sd]
ppInf (InfIffL n dr) = ppApp "iffl" [n, ppDir dr]
ppInf (InfIffR n) = ppApp "iffr" [n]
ppInf (InfImpL n) = ppApp "impl" [n]
ppInf (InfOrR n k) = ppApp "orr" [n, ppInt k]
ppInf (InfAndL n k) = ppApp "andl" [n, ppInt k]
ppInf (InfOrL n) = ppApp "orl" [n]
ppInf (InfAndR n) = ppApp "andr" [n]
ppInf (InfFaL nm xs) = ppApp "fal" [nm, ppList writeTerm xs]
ppInf (InfFaR nm k) = ppApp "far" [nm, ppInt k]
ppInf (InfExL nm k) = ppApp "exl" [nm, ppInt k]
ppInf (InfExR nm xs) = ppApp "exr" [nm, ppList writeTerm xs]
ppInf (InfAx n m) = ppApp "ax" [n, m]
ppInf i = error $ "No printer for inf : " ++ show i

ppDir :: Dir -> Text
ppDir Obv = "obv"
ppDir Rev = "rev"

ppPol :: Pol -> Text
ppPol Pos = "pos"
ppPol Neg = "neg"

ppElab :: Elab -> Text
ppElab (Plab f p) = T.intercalate "\n" $ ["Plab", "f :", ppForm f, "prf :"] ++ ppPrf 20 p
ppElab (Rdef r _ _) = "rdef : " <> r
ppElab (AOC xs _ _) = "AOC :\nxs : " <> ppListNl ppTerm xs

ppSQ :: Text -> Text
ppSQ t = "'" <> t <> "'"

ppInt :: Int -> Text
ppInt 0 = "0"
ppInt 2 = "2"
ppInt 1 = "1"
ppInt 3 = "3"
ppInt 4 = "4"
ppInt 5 = "5"
ppInt 6 = "6"
ppInt 7 = "7"
ppInt 8 = "8"
ppInt 9 = "9"
ppInt k = ppInt (k `div` 10) <> ppInt (k `rem` 10)

ppPolForm :: (Form, Pol) -> Text
ppPolForm (f, Pos) = ppForm f <> " |-"
ppPolForm (f, Neg) = "|- " <> ppForm f
