{-# LANGUAGE OverloadedStrings #-}

module PP where

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
ft = B.fromLazyText
tlt = B.toLazyText 

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

ppAnt :: Ant -> Builder
ppAnt Nothing  = ""
ppAnt (Just t) = ppGterm t

ppAnForm :: AnForm -> String
ppAnForm (Af n f a) = printf "%s :: %s :: %s" n (tlt $ ppForm f) (tlt $ ppAnt a)

ppEq :: (Term, Term) -> Builder
ppEq (x, y) = ppForm (Eq x y)

pad :: Builder -> Builder
pad t = "  " <> t

ppProof :: Int -> Prf -> Builder
ppProof k p = ppInter "\n" $ ppPrf k p

ppPrf :: Int -> Prf -> [Builder]
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
ppPrf k (Mrk s p) = ("Mark : " <> ft s) : L.map pad (ppPrf (k - 1) p)
ppPrf k (FunC f xs ys) = ["Fun-C : ", "  f : " <> ft f, "  xs : " <> ppList ppTerm xs, "  ys : " <> ppList ppTerm ys]
ppPrf k (RelC r xs ys) = ["Rel-C : ", "  r : " <> ft r, "  xs : " <> ppList ppTerm xs, "  ys : " <> ppList ppTerm ys]
ppPrf k (OrL fps) = "Or-L" : L.map pad (L.concatMap (\ (f_, p_) -> ": " <> ppForm f_ : ppPrf (k - 1) p_) fps)
ppPrf k (OrR fs fs' p) = ("Or-R : " <> ppForm (Or fs)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (AndL fs fs' p) = ("And-L : " <> ppForm (And fs)) : L.map pad (ppPrf (k - 1) p)
ppPrf k (AndR fps) = "And-R" : L.map pad (L.concatMap (\ (f_, p_) -> ": " <> ppForm f_ : ppPrf (k - 1) p_) fps)
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
writeEF (f, sd, ep, k, i, cmt) = ppApp "fof" [ppEP ep, "plain", writeForm f, ppApp "tab" [ppPol sd, ppInt k, ppInf i, ft cmt]] <> "."

ppEP :: EP -> Builder
-- ppEP (k, []) = ppSQ $ ppInt k
--ppEP (k, l) = ppSQ $ ppInter ":" (L.map (\ (m_, n_) -> ppInt n_ <> "." <> ppInt m_) (L.reverse l)) <> ":" <> ppInt k
ppEP (k, l) = ppSQ $ ppInter ":" $ ppInt k : L.map (\ (m_, n_) -> ppInt m_ <> "." <> ppInt n_) l

ppSide :: Side -> Builder
ppSide Lft = "lft"
ppSide Rgt = "rgt"

ppInf :: Inf -> Builder
ppInf InfCut = "cut"
ppInf (InfEqR nm) = ppApp "eqr" [ft nm]
ppInf (InfEqS nm0 nm1) = ppApp "eqs" [ft nm0, ft nm1]
ppInf (InfEqT nm0 nm1 nm2) = ppApp "eqt" [ft nm0, ft nm1, ft nm2]
ppInf (InfNotL nm) = ppApp "notl" [ft nm]
ppInf (InfNotR nm) = ppApp "notr" [ft nm]
ppInf (InfImpR n sd) = ppApp "impr" [ft n, ppSide sd]
ppInf (InfIffL n dr) = ppApp "iffl" [ft n, ppDir dr]
ppInf (InfIffR n) = ppApp "iffr" [ft n]
ppInf (InfImpL n) = ppApp "impl" [ft n]
ppInf (InfOrR n) = ppApp "orr" [ft n]
ppInf (InfAndL n) = ppApp "andl" [ft n]
ppInf (InfOrL n) = ppApp "orl" [ft n]
ppInf (InfAndR n) = ppApp "andr" [ft n]
ppInf (InfFaL nm xs) = ppApp "fal" [ft nm, ppList writeTerm xs]
ppInf (InfFaR nm k) = ppApp "far" [ft nm, ppInt k]
ppInf (InfExL nm k) = ppApp "exl" [ft nm, ppInt k]
ppInf (InfExR nm xs) = ppApp "exr" [ft nm, ppList writeTerm xs]
ppInf (InfAx n m) = ppApp "ax" [ft n, ft m]
ppInf (InfFunC ns m) = ppApp "func" [ppList ft ns, ft m]
ppInf (InfRelC ns m n) = ppApp "relc" [ppList ft ns, ft m, ft n]
ppInf InfRdef = "rdef" 
ppInf (InfAoC xs) = ppApp "aoc" [ppList writeTerm xs]
ppInf Close = "close"

ppDir :: Dir -> Builder
ppDir Obv = "obv"
ppDir Rev = "rev"

ppPol :: Pol -> Builder
ppPol Pos = "pos"
ppPol Neg = "neg"

ppElab :: Elab -> Builder
ppElab (Plab f p t) = ppInter "\n" $ ["Plab", "f :" <> ppForm f, "prf :"] ++ ppPrf 20 p ++ ["Notes : " <> ft t]
ppElab (Rdef r f g _ t) = "rdef : " <> ft r <> " : " <> ppForm f <> " |- " <> ppForm g <> "\nNotes : " <> ft t
ppElab (AOC xs _ _ _ t) = "AOC :\nxs : " <> ppListNl ppTerm xs <> "\nNotes : " <> ft t

ppSQ :: Builder -> Builder
ppSQ t = "'" <> t <> "'"

ppNat :: Int -> Builder
ppNat 0 = "0"
ppNat 2 = "2"
ppNat 1 = "1"
ppNat 3 = "3"
ppNat 4 = "4"
ppNat 5 = "5"
ppNat 6 = "6"
ppNat 7 = "7"
ppNat 8 = "8"
ppNat 9 = "9"
ppNat k = ppNat (k `div` 10) <> ppNat (k `rem` 10)

ppInt :: Int -> Builder
ppInt k = if k < 0 then "-" <> ppNat (- k) else ppNat k

ppPolForm :: (Form, Pol) -> Builder
ppPolForm (f, Pos) = ppForm f <> " |-"
ppPolForm (f, Neg) = "|- " <> ppForm f
