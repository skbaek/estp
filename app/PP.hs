{-# LANGUAGE OverloadedStrings #-}

module PP where

import Basic
import Data.List as L
import Data.ByteString as BS 
import Data.ByteString.Builder as BD (Builder, byteString, writeFile) 
import Data.ByteString.Conversion (toByteString') 
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList )
import Data.Map as HM

import Types
import Data.Functor.Contravariant (Op(Op))
import Control.Applicative ( Alternative((<|>)) )
import Control.Monad as M ( guard, mzero )

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
ppTerm (Var x) = byteString x
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

ft :: BS -> Builder
ft = byteString

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

ppGent :: Gent -> Builder
ppGent (GenF f) = ppFormData f
ppGent (GenT f ts) = ft f <> ppArgs (L.map ppGent ts)
ppGent (Genl ts) = ppList id $ L.map ppGent ts
ppGent (Genn k) = ppInt k
ppGent (Genv v) = ft v

ppEq :: (Term, Term) -> Builder
ppEq (x, y) = ppForm (Eq x y)

pad :: Builder -> Builder
pad t = "  " <> t

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
ppElab ((nm, sgn, f), i) =  ppApp "fof" [ft nm, ppSign sgn, writeForm f, ppApp "inference" [ppInf i]] <> "."

fmtAF :: Anf -> Builder
fmtAF (nm, rl, f, Nothing) = ppApp "fof" [ft nm, ft rl, ppForm f]
fmtAF (nm, rl, f, Just (t, Nothing)) = ppApp "fof" [ft nm, ft rl, ppForm f, ppGent t]
fmtAF (nm, rl, f, Just (t, Just ts)) = ppApp "fof" [ft nm, ft rl, ppForm f, ppGent t, ppList ppGent ts]

ppFormData :: Form -> Builder
ppFormData f = "$fof(" <> ppForm f <> ")"

ppInf :: Inf -> Builder
ppInf (Id n m) = ppApp "id" [ft n, ft m]
ppInf (Cut f nf nt) = ppApp "cut" [ppFormData f, ft nf, ft nt]
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

ppSign :: Bool -> Builder
ppSign True = "true"
ppSign False = "false"

ppSignForm :: (Bool, Form) -> Builder
ppSignForm (True, f) = "[T] " <> ppForm f
ppSignForm (False, f) = "[F] " <> ppForm f

ppNL :: (a -> Builder) -> (a -> Builder)
ppNL p x = p x <> "\n"


-- Serialization

serInt :: Int -> Builder
serInt k = ppInt k <> "."

serBS :: BS -> Builder
serBS tx = ft tx <> "."

serSign :: Bool -> Builder
serSign True = "T"
serSign False = "F"

serList :: (a -> Builder) -> [a] -> Builder
serList _ [] = "."
serList s (x : xs) = "," <> s x <> serList s xs

serFunct :: Funct -> Builder
serFunct (Reg t) = serBS t
serFunct (Idx k) = "#" <> serInt k

serTerm :: Term -> Builder
serTerm (Var v) = "$" <> serBS v
serTerm (Fun f xs) = "@" <> serFunct f <> serList serTerm xs

serForm :: Form -> Builder
serForm (Eq x y) = "=" <> serTerm x <> serTerm y
serForm (Rel r xs) = "@" <> serFunct r <> serList serTerm xs
serForm (Not f) = "~" <> serForm f
serForm (And fs) = "&" <> serList serForm fs
serForm (Or fs)  = "|" <> serList serForm fs
serForm (Imp f g) = ">" <> serForm f <> serForm g
serForm (Iff f g) = "^" <> serForm f <> serForm g
serForm (Fa vs f) = "!" <> serList serBS vs <> serForm f
serForm (Ex vs f) = "?" <> serList serBS vs <> serForm f

serNodeName :: Node -> Builder
serNodeName (nm, _, _) = serBS nm

serProof :: Proof -> Builder
serProof p = serBS (proofRN p) <> serProof' p

serSignForm :: (Bool, Form) -> Builder
serSignForm (b, f) = serSign b <> serForm f

bconcat :: [Builder] -> Builder
bconcat = L.foldr (<>) "" 

serProof' :: Proof -> Builder
serProof' (Id_ _ nt nf) = "I" <> serBS nt <> serBS nf
serProof' (Cut_ _ f pf pt) = "C" <> serForm f <> serProof pf <> serProof pt
serProof' (RelD_ _ p) = 
 case proofRSF p of 
  (True, f) -> "D" <> serForm f <> serProof p
  _ -> error "F-signed definition"
serProof' (AoC_ _ x p) = 
 case proofRSF p of 
  (True, f) -> "A" <> serTerm x <> serForm f <> serProof p
  _ -> error "F-signed choice axiom instance"
serProof' (Open_ _) = "O"
serProof' (FunC_ _ nts nf) = "F" <> serList serBS nts <> serBS nf
serProof' (RelC_ _ nts nt nf) = "R" <> serList serBS nts <> serBS nt <> serBS nf
serProof' (EqR_ ni nf) = "=R" <>  serBS nf
serProof' (EqS_ ni nt nf) = "=S" <>  serBS nt <> serBS nf
serProof' (EqT_ ni nxy nyz nxz) = "=T" <>  serBS nxy <> serBS nyz <> serBS nxz
serProof' (NotT_ ni nm p) = "~T" <> serBS nm <> serProof p
serProof' (NotF_ ni nm p) = "~F" <> serBS nm <> serProof p
serProof' (OrT_ ni nm ps) = "|T" <>  serBS nm <> bconcat (L.map serProof ps)
serProof' (OrF_ ni nm k p) = "|F" <>  serBS nm <> serInt k <> serProof p
serProof' (AndT_ ni nm k p) = "&T" <>  serBS nm <> serInt k <> serProof p
serProof' (AndF_ ni nm ps) = "&F" <>  serBS nm <> bconcat (L.map serProof ps)
serProof' (ImpT_ ni nm pa pc) = ">T" <>  serBS nm <> serProof pa <> serProof pc
serProof' (ImpFA_ ni nm p) = ">FA" <> serBS nm <> serProof p
serProof' (ImpFC_ ni nm p) = ">FC" <> serBS nm <> serProof p
serProof' (IffTO_ ni nm p) = "^TO" <> serBS nm <> serProof p
serProof' (IffTR_ ni nm p) = "^TR" <> serBS nm <> serProof p
serProof' (IffF_ ni nm po pr) = "^F" <>  serBS nm <> serProof po <> serProof pr
serProof' (FaT_ ni nm xs p) = "!T" <>  serBS nm <> serList serTerm xs <> serProof p
serProof' (FaF_ ni nm k p) = "!F" <>  serBS nm <> serInt k <> serProof p
serProof' (ExT_ ni nm k p) = "?T" <>  serBS nm <> serInt k <> serProof p
serProof' (ExF_ ni nm xs p) = "?F" <>  serBS nm <> serList serTerm xs <> serProof p

-- Diff display

neqVars :: [BS] -> [BS] -> Builder
neqVars vs ws = ppList ft vs <> "\n!=\n" <> ppList ft ws

neqAppend :: Builder ->  Builder -> Builder
neqAppend b c = b <> "\n------------------------------------------\n" <> c

neqForm :: Form -> Form -> Builder -> Builder
neqForm f g = neqAppend (ppForm f <> "\n!=\n" <> ppForm g) 

neqTerm :: Term -> Term -> Builder -> Builder
neqTerm f g  = neqAppend (ppTerm f <> "\n!=\n" <> ppTerm g) 
diffFunct :: Funct -> Funct -> Maybe Builder
diffFunct (Reg t) (Idx k) = return $ "Reg " <> ft t <> " != Idx " <> ppInt k
diffFunct (Idx k) (Reg t) = return $ "Idx " <> ppInt k <> " != Reg " <> ft t 

diffFunct (Idx k) (Idx m) = do
  guard $ k /= m
  return $ ppInt k <> " != " <> ppInt m
diffFunct (Reg t) (Reg s) = do
  guard $ t /= s
  return $ ft t <> " != " <> ft s

diffTerms :: [Term] -> [Term] -> Maybe Builder
diffTerms [] [] = mzero
diffTerms (x : xs) (y : ys) = diffTerm x y <|> diffTerms xs ys
diffTerms _ _ = Just "unequal number of terms"

diffTerm :: Term -> Term -> Maybe Builder
diffTerm x@(Var _) y@(Fun _ _) = return $ ppTerm x <> " != " <> ppTerm y
diffTerm x@(Fun _ _) y@(Var _) = return $ ppTerm x <> " != " <> ppTerm y
diffTerm x@(Var v) y@(Var w) = do 
  guard $ v /= w 
  return $ ppTerm x <> " != " <> ppTerm y
diffTerm x@(Fun f xs) y@(Fun g ys) = 
  neqTerm x y <$> ( diffFunct f g <|> (zipM xs ys >>= first (uncurry diffTerm)) )

diffJunct :: [Form] -> [Form] -> Maybe Builder
diffJunct [] [] = mzero
diffJunct (f : fs) (g : gs) = diffForm f g <|> diffJunct  fs gs
diffJunct _ _ = Just "unequal number of conjuncts/disjuncts"

diffForm :: Form -> Form -> Maybe Builder
diffForm f@(Eq x y) g@(Eq a b) = neqForm f g <$> (diffTerm x a <|> diffTerm y b)
diffForm f@(Rel r xs) g@(Rel s ys) = neqForm f g <$> (diffFunct r s <|> diffTerms xs ys)
diffForm f@(Iff fl fr) g@(Iff gl gr) = neqForm f g <$> (diffForm fl gl <|> diffForm fr gr)
diffForm (Not f) (Not g) = neqForm (Not f) (Not g) <$> diffForm f g
diffForm (Ex vs f) (Ex ws g) = 
  neqForm (Ex vs f) (Ex ws g) <$> (if vs == ws then diffForm f g else return (neqVars vs ws))
diffForm (Fa vs f) (Fa ws g) = 
  neqForm (Fa vs f) (Fa ws g) <$> (if vs == ws then diffForm f g else return (neqVars vs ws))
diffForm (Imp fa fc) (Imp ga gc) = 
  neqForm (Imp fa fc) (Imp ga gc) <$> (diffForm fa ga <|> diffForm fc gc)
diffForm (And fs) (And gs) = neqForm (And fs) (And gs) <$> diffJunct fs gs
diffForm (Or fs) (Or gs) = neqForm (Or fs) (Or gs) <$> diffJunct fs gs
diffForm f g = Just $ "Default case!\n" <> ppForm f <> "\n!=\n" <> ppForm g 

diffSignForm :: (Bool, Form) -> (Bool, Form) -> Maybe Builder
diffSignForm (True, _) (False, _) = Just "diff sign"
diffSignForm (False, _) (True, _) = Just "diff sign"
diffSignForm (True, f) (True, g) = diffForm f g
diffSignForm (False, f) (False, g) = diffForm f g

diffTrail :: (Bool, Form) -> (Bool, Form) -> BS
diffTrail x y = 
  case diffSignForm x y of 
    Just b -> toByteString' b 
    _ -> error "cannot find diff"

-- writeProof :: String -> [BS] -> Proof -> IO ()
-- writeProof nm nms prf = BD.writeFile nm $ serList serBS nms <> serProof prf
writeProof :: String -> Proof -> IO ()
writeProof nm prf = BD.writeFile nm $ serProof prf