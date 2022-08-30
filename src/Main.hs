{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
-- {-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use second" #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use list comprehension" #-}

module Main where

import Types
import Basic
import PP
-- import App
import Parse
import Check
import BT
import Sat
import Lem
import Control.Monad as M (guard, MonadPlus, foldM, foldM_, (>=>), mzero, when )
import Control.Monad.Fail as MF (MonadFail, fail)
import Control.Applicative ( Alternative((<|>)) )
import System.Environment
import Data.List as L
    (filter, null, find, map, length, foldl, elem, partition, all, any, concat, (\\),
    elemIndex, insert, sortBy, concatMap, unzip, nub, splitAt, delete, reverse )
import Data.Text as T ( uncons, unpack, pack, Text, take, length )
import Data.Set as S ( empty, insert, member, singleton, toList, Set, fromList, delete,
  union, unions, difference, disjoint, (\\), null, lookupMin, isSubsetOf, intersection, size )
import qualified Data.Text.Lazy as TL (toStrict, intercalate)
-- import Data.Text.IO as TIO
import Data.Text.Read as TR ( decimal )
import Data.Functor ((<&>))
import Data.Map as HM ( Map, insert, lookup, empty, map, member, mapMaybe, mapKeys, toList,
  fromListWithKey, mapWithKey, delete, notMember, findWithDefault, partitionWithKey, isSubmapOf,
  filterWithKey, update, fromList, alter, foldrWithKey, foldr )
import Debug.Trace (trace)
import Data.Maybe as MB ( isNothing, fromMaybe, mapMaybe )
import qualified Data.Bifunctor as DBF
import qualified System.Posix.Internals as Prelude
import qualified Data.IntMap as IM
import Data.Tuple (swap)

putAnForm :: AnForm -> IO ()
putAnForm i = Prelude.putStr $ ppAnForm i ++ "\n"

addHyp :: Hyps -> AnForm -> Hyps
addHyp (nsq, sq) (Af n f _) = (HM.insert n f nsq, S.insert f sq)

sortAfs :: [AnForm] -> [AnForm]
sortAfs = sortBy compareAfs

compareAfs :: AnForm -> AnForm -> Ordering
compareAfs (Af (m :> ms) _ _) (Af (n :> ns) _ _) =
  case compare m n of
    EQ -> compare (TR.decimal ms) (TR.decimal ns)
    other -> other
compareAfs _ _ = LT

top :: Form
top = And []

bot :: Form
bot = Or []

tfsimp :: Form -> Form
tfsimp (Rel r xs) = Rel r xs
tfsimp (Eq x y) = Eq x y
tfsimp (Not f) =
  case tfsimp f of
    And [] -> Or []
    Or [] -> And []
    g -> Not g
tfsimp (Or fs) =
  let gs = L.map tfsimp fs in
  if top `elem` gs then top
    else
      case L.filter (/= bot) gs of
        [g] -> g
        hs -> Or hs
tfsimp (And fs) =
  let gs = L.map tfsimp fs in
  if bot `elem` gs then bot
    else
      case L.filter (/= top) gs of
        [g] -> g
        hs -> And hs
tfsimp (Fa vs f) =
  case tfsimp f of
    And [] -> And []
    Or [] -> Or []
    g -> Fa vs g
tfsimp (Ex vs f) =
  case tfsimp f of
    And [] -> And []
    Or [] -> Or []
    g -> Ex vs g
tfsimp (Imp f g) =
  case (tfsimp f, tfsimp g) of
    (Or [], _) -> top
    (_, And []) -> top
    (And [], g_) -> g_
    (f_, Or []) -> Not f_
    (f', g') -> Imp f' g'
tfsimp (Iff f g) =
  case (tfsimp f, tfsimp g) of
    (Or [], g') -> Not g'
    (f', Or []) -> Not f'
    (And [], g') -> g'
    (f', And []) -> f'
    (f', g') -> Iff f' g'

addIf :: Bool -> NSeq-> Text -> Form -> [NSeq]
addIf True c n f = return $ HM.insert n f c
addIf False _ n _ = error $ "Cannot add formula : " ++ unpack n

getHyp :: Text -> NSeq -> IO Form
getHyp n c =
  case HM.lookup n c of
    Just f -> return f
    _ -> MF.fail $ "Hypothesis does not exist : " ++ show n

isOr :: Form -> Bool
isOr (Or _) = True
isOr _ = False

isAnd :: Form -> Bool
isAnd (And _) = True
isAnd _ = False

-- updr :: PrvGoal -> Ctx -> BT Ctx
-- updr pg@(f, g, k) c =
--   cast (appFaRL Same pg c) !>=
--     uncurry updr $
--     cast (appAx Lax pg c) <|>
--     ( do ((f', k'), c') <- cast $ appIffLO (f, k) c
--          updr (f', g, k') c' ) <|>
--     ( do ((f', k'), c') <- cast $ appIffLR (f, k) c
--          updr (f', g, k') c' )

flatOr :: [Form] -> [Form]
flatOr [] = []
flatOr (Or [] : fs) = flatOr fs
flatOr (Or fs : gs) = flatOr fs ++ flatOr gs
flatOr (f : fs) = f : flatOr fs

flatAnd :: [Form] -> [Form]
flatAnd [] = []
flatAnd (And [] : fs) = flatAnd fs
flatAnd (And fs : gs) = flatAnd fs ++ flatAnd gs
flatAnd (f : fs) = f : flatAnd fs

nt :: Maybe a
nt = Nothing

conAuxAux :: Text -> Set Text -> Text -> Set Text -> Set Text
conAuxAux v wso w vs =
  if S.member w wso
  then S.delete v vs
  else vs

conAux :: Text -> Set Text -> VC -> Maybe VC
conAux v nws (vws, wvs) = do
  ws <- HM.lookup v vws
  let wsi = S.intersection ws nws
  let wso = ws S.\\ nws
  guard $ not $ S.null wsi
  let vws' = HM.insert v wsi vws
  let wvs' = HM.mapWithKey (conAuxAux v wso) wvs
  return (vws', wvs')

conVars :: [Text] -> [Text] -> VC -> Maybe VC
conVars vs ws vc = do
  (vws, wvs) <- foldM (\ vc_ v_ -> conAux v_ (S.fromList ws) vc_) vc vs
  (wvs', vws') <- foldM (\ vc_ w_ -> conAux w_ (S.fromList vs) vc_) (wvs, vws) ws
  return (vws', wvs')

conVar :: Text -> Text -> VC -> Maybe VC
conVar v w = conVars [v] [w]

data Path =
    NewRel Text Int
  | NewFun Text Int
  | NewEq
  | NewFa Bool
  | NewEx Bool
  | NewImpL
  | NewImpR
  | NewIffL
  | NewIffR
  | NewOr Int Int
  | NewAnd Int Int
  | NewNot
  deriving (Ord, Eq)

data PrePath =
    PreRel Text Int
  | PreFun Text Int
  | PreEq
  | PreFa [Text]
  | PreEx [Text]
  | PreImpL
  | PreImpR
  | PreIffL
  | PreIffR
  | PreOr Int Int
  | PreAnd Int Int
  | PreNot
  deriving (Ord, Eq)

type Sig = HM.Map [Path] Int
type Sigs = HM.Map Text Sig

varSigs :: Sigs -> Text -> Sigs
varSigs sg v =
  case HM.lookup v sg of
    Just _ -> et "var-new-sigs"
    _ -> HM.insert v HM.empty sg

extendPrePathsRel :: [PrePath] -> Text -> Int -> [Term] -> [([PrePath], Term)]
extendPrePathsRel pts r k [] = []
extendPrePathsRel pts r k (x : xs) = (PreRel r k : pts, x) : extendPrePathsRel pts r (k + 1) xs

extendPrePathsFun :: [PrePath] -> Text -> Int -> [Term] -> [([PrePath], Term)]
extendPrePathsFun pts f k [] = []
extendPrePathsFun pts f k (x : xs) = (PreFun f k : pts, x) : extendPrePathsFun pts f (k + 1) xs

posCount :: [Form] -> Int
posCount fs = L.length $ L.filter isPos fs

negCount :: [Form] -> Int
negCount fs = L.length $ L.filter isNeg fs

formSigs :: Sigs -> [PrePath] -> Form -> Sigs
formSigs sg pts (Eq x y) = do
  let sg' = termSigs sg (PreEq : pts) x
  termSigs sg' (PreEq : pts) y
formSigs sg pts (Rel r xs) =
  let ptxs = extendPrePathsRel pts r 0 xs in
  L.foldl (uncurry . termSigs) sg ptxs
formSigs sg pts (Not f) =  formSigs sg (PreNot : pts) f
formSigs sg pts (Imp f g) =
  let sg' = formSigs sg (PreImpL : pts) f in
  formSigs sg' (PreImpR : pts) g
formSigs sg pts (Iff f g) =
  let sg' = formSigs sg (PreIffL : pts) f in
  formSigs sg' (PreIffR : pts) g
formSigs sg pts (Fa vs f) =
  let sg' = L.foldl varSigs sg vs in
  formSigs sg' (PreFa vs : pts) f
formSigs sg pts (Ex vs f) =
  let sg' = L.foldl varSigs sg vs in
  formSigs sg' (PreEx vs : pts) f
formSigs sg pts (Or fs)  = L.foldl (\ sg_ (f_, fs_) -> formSigs sg_ (PreOr  (posCount fs_) (negCount fs_) : pts) f_) sg (plucks fs)
formSigs sg pts (And fs) = L.foldl (\ sg_ (f_, fs_) -> formSigs sg_ (PreAnd (posCount fs_) (negCount fs_) : pts) f_) sg (plucks fs)

markNonOcc :: PrePath -> Path
markNonOcc (PreFa _) = NewFa False
markNonOcc (PreEx _) = NewEx False
markNonOcc (PreRel r k) = NewRel r k
markNonOcc (PreFun f k) = NewFun f k
markNonOcc (PreOr k m) = NewOr k m
markNonOcc (PreAnd k m) = NewAnd k m
markNonOcc PreImpL = NewImpL
markNonOcc PreImpR = NewImpR
markNonOcc PreIffL = NewIffL
markNonOcc PreIffR = NewIffR
markNonOcc PreEq = NewEq
markNonOcc PreNot = NewNot

markFstOcc :: Text -> [PrePath] -> [Path]
markFstOcc _ [] = et "unbound var"
markFstOcc v (PreFa vs : pts) =
  if v `elem` vs
  then NewFa True  : L.map markNonOcc pts
  else NewFa False : markFstOcc v pts
markFstOcc v (PreEx vs : pts) =
  if v `elem` vs
  then NewEx True  : L.map markNonOcc pts
  else NewEx False : markFstOcc v pts
markFstOcc v (pt : pts) =
  markNonOcc pt : markFstOcc v pts

termSigs :: Sigs -> [PrePath] -> Term -> Sigs
termSigs sg pts (Var v) =
  let npts = markFstOcc v pts in
  case HM.lookup v sg of
    Just mp ->
      let mp' = HM.alter succMaybe npts mp in
      HM.insert v mp' sg
    _ -> et $ "Signature does not include var : " <> v
termSigs sg pts (Fun f xs) =
  let ptxs = extendPrePathsFun pts f 0 xs in
  L.foldl (uncurry . termSigs) sg ptxs
termSigs sg _ (Par _) = sg

insertBySig :: Text -> Maybe [Text] -> Maybe [Text]
insertBySig v (Just vs) = Just (v : vs)
insertBySig v _ = Just [v]

groupByTextKey :: (Ord a) => HM.Map Text a -> HM.Map a [Text]
groupByTextKey = HM.foldrWithKey (HM.alter . insertBySig) HM.empty

succMaybe :: Maybe Int -> Maybe Int
succMaybe (Just k) = Just (k + 1)
succMaybe _ = Just 1

mergeBySigs :: Map Sig [Text] -> Map Sig [Text] -> [([Text], [Text])]
mergeBySigs fm gm =
  HM.foldrWithKey
    (\ sg_ vs_ ->
      case HM.lookup sg_ gm of
        Just ws ->
          if L.length vs_ == L.length ws
          then ((vs_, ws) :)
          else et "Cannot merge sigs : 0"
        _ -> et "Cannot merge sigs : 1" )
    [] fm

dne :: Form -> Form
dne (Not (Not f)) = dne f
dne (Not f) = Not $ dne f
dne (Or fs) = Or $ L.map dne fs
dne (And fs) = And $ L.map dne fs
dne (Imp f g) = Imp (dne f) (dne g)
dne (Iff f g) = Iff (dne f) (dne g)
dne (Fa vs f) = Fa vs $ dne f
dne (Ex vs f) = Ex vs $ dne f
dne f = f

qmerge :: Form -> Form
qmerge (Not f) = Not $ qmerge f
qmerge (Or fs)  = Or $ L.map qmerge fs
qmerge (And fs) = And $ L.map qmerge fs
qmerge (Imp f g) = Imp (qmerge f) (qmerge g)
qmerge (Iff f g) = Iff (qmerge f) (qmerge g)
qmerge (Fa vs (Fa ws f)) = Fa (vs ++ ws) $ qmerge f
qmerge (Ex vs (Ex ws f)) = Ex (vs ++ ws) $ qmerge f
qmerge (Fa vs f) = Fa vs $ qmerge f
qmerge (Ex vs f) = Ex vs $ qmerge f
qmerge f = f

fltn :: Form -> Form
fltn (Not f) = Not $ fltn f
fltn (Or fs) = Or $ flatOr $ L.map fltn fs
fltn (And fs) = And $ flatAnd $ L.map fltn fs
fltn (Imp f g) = Imp (fltn f) (fltn g)
fltn (Iff f g) = Iff (fltn f) (fltn g)
fltn (Fa vs f) = Fa vs $ fltn f
fltn (Ex vs f) = Ex vs $ fltn f
fltn f = f

-- flatten :: Form -> Form
-- flatten (Not f) =
--   case flatten f of
--     Not f' -> f'
--     f' -> Not f'
-- flatten (And fs) = And $ flatAnd $ L.map flatten fs
-- flatten (Or fs)  = Or  $ flatOr  $ L.map flatten fs
-- flatten (Fa vs f)  =
--   case flatten f of
--     Fa ws f' -> Fa (vs ++ ws) f'
--     f' -> Fa vs f'
-- flatten (Ex vs f)  =
--   case flatten f of
--     Ex ws f' -> Ex (vs ++ ws) f'
--     f' -> Ex vs f'
-- flatten (Imp f g)  = Imp (flatten f) (flatten g)
-- flatten (Iff f g)  = Iff (flatten f) (flatten g)
-- flatten f = f

origPrep :: Form -> Form -> IO (Form, Form, Prf -> Prf)
origPrep f g = do
  ([f', g'], _) <- cast $ rnfs HM.empty HM.empty [f, g]
  let f'' = dne f'
  let f''' = fltn f''
  let g'' = dne g'
  let g''' = fltn g''

  pfrn <- prn 0 f f' -- pfrn : |- f <=> f'
  pfdn <- pdne 0 f' f'' -- pfrn : |- f <=> f'
  pffl <- pfl 0 f'' f'''
  let pf =
         cuts [ (f <=> f', pfrn), (f' <=> f'', pfdn), (f'' <=> f''', pffl),
                (f <=> f'', iffTrans f f' f''), (f <=> f''', iffTrans f f'' f''') ] (iffMP f f''')
  pgrn <- prn 0 g g' -- pfrn : |- f <=> f'
  pgdn <- pdne 0 g' g'' -- pfrn : |- f <=> f'
  pgfl <- pfl 0 g'' g'''
  let pg = 
         cuts [ (g <=> g', pgrn), (g' <=> g'', pgdn), (g'' <=> g''', pgfl),
                (g <=> g'', iffTrans g g' g''), (g <=> g''', iffTrans g g'' g''') ] (iffMPR g g''')
  let pfun = \ p_ -> Cut f''' pf $ Cut g''' p_ pg
  return (f''', g''', pfun)

orig :: Form -> Form -> IO Prf
orig _f _g = do
  (f, g, pf) <- origPrep _f _g
  -- pt $ "f : " <> ppForm f <> "\n"
  -- pt $ "g : " <> ppForm g <> "\n"
  cvc <- makeVC f g
  vc <- searchOrig 0 cvc [(f, g)]
  let vr = vcToVr vc
  pf <$> uvm 0 vr f g

makeVC :: Form -> Form -> IO VC
makeVC f g = do
  let vs = formVars f
  let ws = formVars g
  let fsgs = formSigs HM.empty [] f
  let gsgs = formSigs HM.empty [] g
  let fm = groupByTextKey fsgs
  let gm = groupByTextKey gsgs
  let vswss = mergeBySigs fm gm
  -- -- pt $ ppListNl (\ (vs_, ws_) -> ppList id vs_ <> " <=|=> " <> ppList id vs_) vswss
  foldM (\ vc_ (vs_, ws_) -> cast (conVars vs_ ws_ vc_)) (initVC vs ws) vswss

ppCla :: Form -> Text
ppCla (Or fs) = ppList ppForm fs
ppCla f = ppForm f

ppMat :: Form -> Text
ppMat (Fa _ f) = ppMat f
ppMat (Ex _ f) = ppMat f
ppMat (And fs) = ppListNl ppCla fs
ppMat f = ppCla f

-- skolemize :: [Form] -> PrvGoal -> Ctx -> IO Ctx
-- skolemize hs pg c =
--   case ( appAx Pars pg c,
--          appNotLR pg c <|> appFaRL Perm pg c <|> appExLR Perm pg c,
--          appImpLR pg c <|> appIffRL pg c,
--          appOrRL pg c <|> appAndLR pg c ) of
--     (Just c', _, _, _) -> return c'
--     (_, Just (pg, c'), _, _) -> skolemize hs pg c'
--     (_, _, Just (pg0, pg1, c'), _) -> skolemize hs pg0 c' >>= skolemize hs pg1
--     (_, _, _, Just (pgs, c')) -> foldM (flip (skolemize hs)) c' pgs
--     _ -> first (skolemizeAux pg c) (pluck hs)
-- 
-- skolemizeAux :: PrvGoal -> Ctx -> (Form, [Form]) -> IO Ctx
-- skolemizeAux (f, g, k) c (h, hs) = do
--   let ((h', k'), c0) = tryAppFaL (h, k) c
--   ((f', m), (g', n), c1) <- cast $ appImpL (h', k') c0
--   c2 <- cast $ appAx ParFvs (f, f', m) c1
--   skolemize hs (g', g, n) c2

-- blank :: Ctx
-- blank = Ctx {fresh = 1, binding = HM.empty, proofs = HM.empty }

isRdef :: Text -> Form -> Bool
isRdef r (Fa _ (Iff (Rel s _) _)) = r == s
isRdef r (Iff (Rel s _) _) = r == s
isRdef _ _ = False

putText :: Text -> IO ()
putText tx = Prelude.putStr (unpack tx)

checkElab :: Seq -> Form -> Elab -> IO ()
checkElab sq g (Plab p) =
  verify 0 sq (S.singleton g) p
checkElab sq g (Nnf ex f) = do
  guard $ S.member f sq
  guard $ nnf ex f == g
checkElab sq g (Rdef r f p) = do
  guard $ isRdef r f
  verify 0 (S.singleton f) (S.singleton g) p
checkElab sq g (Tfe f) = do
  guard $ S.member f sq
  guard $ tfsimp f == g
checkElab sq g (AOC xs f p) = isAOC xs f
checkElab sq g (Lrats fs lrs) = do
  print "Verifying LRAT..."
  guard (L.all (`S.member` sq) fs)
  print "All premises present..."
  checkLrats (lratCtx 1 fs) lrs

lratCtx :: Int -> [Form] -> Map Int Form
lratCtx _ [] = HM.empty
lratCtx k (f : fs) = HM.insert k f $ lratCtx (k + 1) fs

negLit :: Form -> Form
negLit (Not f) = f
negLit f = Not f

negated :: Set Form -> Form -> Bool
negated fs (Not f) = f `elem` fs
negated fs f = Not f `elem` fs

checkLrat :: Map Int Form -> Set Form -> [Int] -> IO ()
checkLrat _ _ [] = mzero
checkLrat fs fxs [k] = do
  ls <- cast $ HM.lookup k fs >>= formToLits
  guard $ L.all (negated fxs) ls
checkLrat fs fxs (k : ks) = do
  ls <- cast $ HM.lookup k fs >>= formToLits
  fx <- cast $ breakSingleton $ nub $ L.filter (not . negated fxs) ls
  checkLrat fs (S.insert fx fxs) ks

checkLrats :: Map Int Form -> [Lrat] -> IO ()
checkLrats _ [] = mzero
checkLrats fs [Add _ [] hs] = checkLrat fs S.empty hs
checkLrats _ [_] = mzero
checkLrats fs (Add k ls hs : lrs) = do
  let ns = S.fromList $ L.map negLit ls
  checkLrat fs ns hs
  checkLrats (HM.insert k (Or ls) fs) lrs
checkLrats fs (Del _ ks : lrs) =
  checkLrats (L.foldl (flip HM.delete) fs ks) lrs

breakBv :: Term -> Maybe Text
breakBv (Var v) = Just v
breakBv _ = Nothing

isSkolemTerm :: [Text] -> Term -> Bool
isSkolemTerm vs (Fun _ xs) =
  case mapM breakBv xs of
    Just ws -> sublist vs ws && sublist ws vs
    _ -> False
isSkolemTerm _ _ = False

isConstant :: Term -> Bool
isConstant (Fun _ []) = True
isConstant _ = False

isAOC :: [Term] -> Form -> IO ()
isAOC xs (Fa vs (Imp (Ex ws f) g)) = do
  guard $ L.all (isSkolemTerm vs) xs
  wxs <- zipM ws xs
  guard $ substForm wxs f == g
isAOC xs (Imp (Ex ws f) g) = do
  guard $ L.all isConstant xs
  wxs <- zipM ws xs
  guard $ substForm wxs f == g
isAOC _ _ = mzero

elabIO :: Hyps -> AnForm -> IO Hyps
elabIO (nsq, sq) (Af n f a) = do
  print $ "Elaborating step = " <> n
  e <- elab nsq (Af n f a)
  checkElab sq f e
  return (HM.insert n f nsq, S.insert f sq)

-- elabIO' :: HM.Map Text Form -> AnForm -> IO (HM.Map Text Form)
-- elabIO' nsq (Af n f a) = do
--   print $ "Elaborating step = " <> n
--   elab' nsq (Af n f a)
--   return $ HM.insert n f nsq

nnf :: Bool -> Form -> Form
nnf ex (Not (Rel r xs)) = Not $ Rel r xs
nnf ex (Not (Eq x y)) = Not $ Eq x y
nnf ex (Not (Not f)) = nnf ex f
nnf ex (Not (Or fs)) = And $ L.map (nnf ex . Not) fs
nnf ex (Not (And fs)) = Or $ L.map (nnf ex . Not) fs
nnf ex (Not (Imp f g)) = nnf ex (And [Not g, f])
nnf True  (Not (Iff f g)) = Not $ Iff (nnf True f) (nnf True g)
nnf False (Not (Iff f g)) =
  let nn = nnf False (Not g) in
  let nm = nnf False (Not f) in
  let pn = nnf False g in
  let pm = nnf False f in
  And [Or [nn, nm], Or [pn, pm]]
nnf ex (Not (Fa vs f)) = Ex vs (nnf ex $ Not f)
nnf ex (Not (Ex vs f)) = Fa vs (nnf ex $ Not f)
nnf ex (Or fs)  = Or  $ L.map (nnf ex) fs
nnf ex (And fs) = And $ L.map (nnf ex) fs
nnf ex (Imp f g) = Or [nnf ex g, nnf ex $ Not f]
nnf True  (Iff f g) = Iff (nnf True f) (nnf True g)
nnf False (Iff f g) =
  let nn = nnf False (Not g) in
  let nm = nnf False (Not f) in
  let pn = nnf False g in
  let pm = nnf False f in
  And [Or [pm, nn], Or [pn, nm]]
nnf ex (Fa vs f) = Fa vs $ nnf ex f
nnf ex (Ex vs f) = Ex vs $ nnf ex f
nnf ex (Rel r xs) = Rel r xs
nnf ex (Eq x y) = Eq x y

shuffle :: a -> a -> BT (a, a)
shuffle x y = return (x, y) <|> return (y, x)

gFunFunctor :: Gterm -> Maybe Text
gFunFunctor (Gfun t []) = return t
gFunFunctor _ = Nothing

-- infer' :: Text -> [Form] -> Form -> IO ()
-- infer' "superposition" [f, g] h         = superpose f g h
-- infer' "forward_demodulation" [f, g] h  = superpose f g h
-- infer' "backward_demodulation" [f, g] h = superpose f g h
-- infer' _ _ _ = return ()

infer :: Text -> [Form] -> Form -> IO Prf
infer "superposition" [f, g] h         = superpose f g h
infer "forward_demodulation" [f, g] h  = superpose f g h
infer "backward_demodulation" [f, g] h = superpose f g h
infer "negated_conjecture" [f] g = guard (f == g) >> return (Ax f)
infer "factoring" [f] g = efactor (Just True) f g
infer "duplicate_literal_removal" [f] g = efactor (Just True) f g
infer "equality_resolution" [f] g = efactor (Just False) f g
infer "trivial_inequality_removal" [f] g = efactor nt f g
infer "subsumption_resolution" [f, g] h = resolve f g h
infer "resolution" [f, g] h = resolve f g h
infer "definition_folding" (f : fs) g = definFold fs f g
infer "definition_unfolding" (f : fs) g = dunfold fs f g
infer "pure_predicate_removal" [f] g = ppr f g
infer "flattening" [f] g = flat f g
infer "equality_factoring" [f] g = eqfactor f g
infer "rectify" [f] g            = rectify f g
infer "avatar_component_clause" [f] g = avatarComp f g
infer "cnf_transformation" [f] g = cnfTrans f g
infer "avatar_split_clause" (f : fs) g    = avatarSplit fs f g
infer "unused_predicate_definition_removal" [f] g = return Asm -- et "todo : updr" -- cast $ updr (f, g, 0) blank
infer "skolemisation" (f : fs) g = return Asm -- et "todo : skolemisation" -- skolemize fs (f, g, 0) blank
infer "avatar_contradiction_clause" [f] g = efactor (Just True) f g -- et "todo : avatar contra" -- avatarContra f g
infer r fs g = et $ "No inference : " <> r

df1rw :: [Form] -> Form -> IO (Form, Form)
df1rw [] _ = mzero
df1rw (Fa vs (Iff l r) : es) g =
  if g == l
  then return (Fa vs (l <=> r), r)
  else df1rw es g
df1rw (Iff l r : es) g =
  if g == l
  then return (l <=> r, r)
  else df1rw es g
df1rw (e : es) f = et $ "non-definition : " <> ppForm e

dfj :: [Form] -> [Form] -> IO (Form, [Form])
dfj es (f : fs) = (DBF.second (: fs) <$> df1 es f) <|> (DBF.second (f :) <$> dfj es fs)
dfj _ [] = mzero

specs :: VM -> [(Term, Term)] -> Maybe VM
specs vm [] = return vm
specs vm ((x, y) : xys) = do
  vm' <- spec vm x y
  specs vm' xys

spec :: VM -> Term -> Term -> Maybe VM
spec vm (Var v) y = do
  case HM.lookup v vm of
    Just x -> do
      guard $ x == y
      return vm
    _ -> return $ HM.insert v y vm
spec vm x@(Fun f xs) y@(Fun g ys) = do
  guard $ f == g
  xys <- zipM xs ys
  foldM (\ vm_ (x_, y_) -> spec vm_ x_ y_) vm xys
spec vm x y = do
  guard (x == y)
  return vm


df1 :: [Form] -> Form -> IO (Form, Form)
df1 es (Ex vs f) = DBF.second (Ex vs) <$> df1 es f
df1 es (Fa vs f) = DBF.second (Fa vs) <$> df1 es f
df1 es (And fs)  = DBF.second And <$> dfj es fs
df1 es (Or fs)   = DBF.second Or <$> dfj es fs
df1 es (Not f)   = DBF.second Not <$> df1 es f
df1 es (Imp f g) = (DBF.second (`Imp` g) <$> df1 es f) <|> (DBF.second (Imp f) <$> df1 es g)
df1 es (Iff f g) = (DBF.second (`Iff` g) <$> df1 es f) <|> (DBF.second (Iff f) <$> df1 es g)
df1 _ (Eq _ _) = mzero
df1 es g@(Rel _ _) = df1rw es g

dff :: [Form] -> Form -> Form -> IO [(Form, Form)]
dff es f g =
  if f == g
  then return []
  else do (e, g') <- df1 es g
          ((e, g') :) <$> dff es f g'

type USOL = [Term]

duts :: [(Dir, Form)] -> [Term] -> Maybe [Term]
duts es (x : xs) = ((: xs) <$> dut es x) <|> ((x :) <$> duts es xs)
duts es [] = mzero

dut :: [(Dir, Form)] -> Term -> Maybe Term
dut es x@(Fun f xs) = (Fun f <$> duts es xs) <|> first (dutt x) es
dut es x = first (dutt x) es

dutt :: Term -> (Dir, Form) -> Maybe Term
dutt x (Obv, Fa vs (Eq a b)) = do
  vm <- cast $ spec HM.empty a x
  return $ subt vm b
dutt x (Rev, Fa vs (Eq a b)) = do
  vm <- cast $ spec HM.empty b x
  return $ subt vm a
dutt x (Obv, Eq a b)
  | x == a = return b
  | otherwise = nt
dutt x (Rev, Eq a b)
  | x == b = return a
  | otherwise = nt
dutt _ (dr, f) = et $ "non-equation : " <> ppForm f <> "\n"

du :: [(Dir, Form)] -> Term -> Term -> IO USOL
du es x y = do
  if x == y
  then return []
  else do x' <- cast $ dut es x
          (x' :) <$> du es x' y

dufAux :: [(Dir, Form)] -> Form -> (Form, [Form]) -> IO ([Form], (Form, [USOL]))
dufAux es g (f, fs) = (fs,) <$> duf es f g

dufs :: [(Dir, Form)] -> [Form] -> [Form] -> IO ([Form], [(Form, [USOL])])
dufs es = mapAccumM (\ fs_ g_ -> first (dufAux es g_) (plucks fs_))

duf :: [(Dir, Form)] -> Form -> Form -> IO (Form, [USOL])
duf es (Not f)   (Not g)   = DBF.first Not <$> duf es f g
duf es (Ex vs f) (Ex ws g) = DBF.first (Ex vs) <$> duf es f g
duf es (Fa vs f) (Fa ws g) = DBF.first (Fa vs) <$> duf es f g
duf es (And fs)  (And gs)  = do
  (fs', xss) <- unzip . snd <$> dufs es fs gs
  return (And fs', L.concat xss)
duf es (Or fs)   (Or gs)   = do
  (fs', xss) <- unzip . snd <$> dufs es fs gs
  return (Or fs', L.concat xss)
duf es (Imp e f) (Imp g h) = do
  (e', xsl) <- duf es e g
  (f', xsr) <- duf es f h
  return (Imp e' f', xsl ++ xsr)
duf es (Iff e f) (Iff g h) = do
  (e', xsl) <- duf es e g
  (f', xsr) <- duf es f h
  return (Iff e' f', xsl ++ xsr)
duf es (Eq w x)  (Eq y z)  =
  ( do ws <- du es w y
       xs <- du es x z
       return (w === x, [ws, xs]) ) <|>
  ( do xs <- du es x y
       ws <- du es w z
       return (x === w, [xs, ws]) )
duf es (Rel r xs) (Rel s ys) = (Rel r xs,) <$> mapM2 (du es) xs ys
duf _ _ _ = mzero

tsfuns :: [Term] -> Set Text
tsfuns = S.unions . L.map tfuns

tfuns :: Term -> Set Text
tfuns (Fun f xs) = S.insert f $ S.unions $ L.map tfuns xs
tfuns _ = S.empty

fsfuns :: [Form] -> Set Text
fsfuns = S.unions . L.map ffuns

ffuns :: Form -> Set Text
ffuns (Rel _ xs) = S.unions $ L.map tfuns xs
ffuns (Or fs) = S.unions $ L.map ffuns fs
ffuns (And fs) = S.unions $ L.map ffuns fs
ffuns (Eq x y) = S.unions $ L.map tfuns [x, y]
ffuns (Imp f g) = S.unions $ L.map ffuns [f, g]
ffuns (Iff f g) = S.unions $ L.map ffuns [f, g]
ffuns (Not f) = ffuns f
ffuns (Fa _ f) = ffuns f
ffuns (Ex _ f) = ffuns f

overlaps :: Ord a => Set a -> Set a -> Bool
overlaps r s = not $ disjoint r s

headFix :: Set Text -> Term -> Bool
headFix dfs (Fun f _) = S.member f dfs
headFix dfs _ = False

obvEq :: Set Text -> Form -> (Dir, Form)
obvEq dfs f@(Fa vs (Eq a b))
  | headFix dfs a && disjoint dfs (tfuns b) = (Obv, f)
  | headFix dfs b && disjoint dfs (tfuns a) = (Rev, f)
  | otherwise = et "obv-eq : cannot fix direction"
obvEq dfs f@(Eq a b)
  | headFix dfs a && disjoint dfs (tfuns b) = (Obv, f)
  | headFix dfs b && disjoint dfs (tfuns a) = (Rev, f)
  | otherwise = et "obv-eq : cannot fix direction"
obvEq dfs _ = et "obv-eq : non-equation"

dunfold :: [Form] -> Form -> Form -> IO Prf
dunfold es f h = do
  let dfs = ffuns f S.\\ ffuns h
  let des = L.map (obvEq dfs) es
  (f', uss) <- duf des f h
  -- pt "Unfold solution found :\n"
  -- pt $ ppListNl ppUsol uss
  (h', gs) <- dips f' uss
  guardMsg (h == h') "Simultaneous substitution result does not match target"
  -- pt "All interpolants :\n"
  -- pt $ ppListNl ppForm gs
  -- pt $ "f : " <> ppForm f <> "\n"
  -- pt $ "f' : " <> ppForm f' <> "\n"
  pl <- ppuf 0 f f'
  pr <- uips (puf 0 es) f' gs h
  return $ Cut f' (Cut (f <=> f') pl (iffMP f f')) pr

ppufAux :: Int -> [Form] -> [Form] -> IO [(Form, Form, Prf)]
ppufAux _ [] _ = return []
ppufAux k (f : fs) ggs = do
  ((g, p), gs) <- pluckFirst (\ g_ -> (g_,) <$> ppuf k f g_) ggs
  fgps <- ppufAux k fs gs
  return ((f, g, p) : fgps)

ppuf :: Int -> Form -> Form -> IO Prf
ppuf k (Not f) (Not g) = do
  p <- ppuf k f g
  return $ Cut (f <=> g) p $ iffToNotIffNot f g
ppuf k (Or fs) (Or gs) = do
  fgps <- ppufAux k fs gs
  let fgs = L.map (\ (f_, g_, _) -> (f_, g_)) fgps
  let iffps = L.map (\ (f_, g_, p_) -> (f_ <=> g_, p_)) fgps
  cuts iffps <$> cast (iffsToOrIffOr' fgs fs gs)
ppuf k (And fs) (And gs) = do
  fgps <- ppufAux k fs gs
  let fgs = L.map (\ (f_, g_, _) -> (f_, g_)) fgps
  let iffps = L.map (\ (f_, g_, p_) -> (f_ <=> g_, p_)) fgps
  cuts iffps <$> cast (iffsToAndIffAnd' fgs fs gs)
ppuf k (Imp e f) (Imp g h) = do
  pl <- ppuf k e g -- pl : |- fl <=> gl 
  pr <- ppuf k f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ impCong e f g h
ppuf k (Iff e f) (Iff g h) = do
  pl <- ppuf k e g -- pl : |- fl <=> gl 
  pr <- ppuf k f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ iffCong e f g h
ppuf k (Fa vs f) (Fa ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- ppuf k' f' g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToFaIffFa vs k f g
ppuf k (Ex vs f) (Ex ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- ppuf k' f' g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToExIffEx vs k f g
ppuf _ (Rel r xs) (Rel s ys) = do
  guard $ r == s && xs == ys
  return $ iffRefl (Rel r xs)
ppuf _ (Eq x y) (Eq a b)
  | x == a && y == b = return $ iffRefl (x === y)
  | x == b && y == a = return $ iffRFull (x === y) (a === b) (EqS x y) (EqS a b)
  | otherwise = mzero
ppuf _ f g = mzero

notNotIff :: Form -> Prf
notNotIff f =
  iffRFull (Not (Not f)) f
    (NotL (Not f) $ NotR f $ Ax f)
    (NotR (Not f) $ NotL f $ Ax f)

prn :: Int -> Form -> Form -> IO Prf
prn k (Not f) (Not g) = do
  p <- prn k f g
  return $ Cut (f <=> g) p $ iffToNotIffNot f g
prn k (Or fs) (Or gs) = do
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> prn k f_ g_) fs gs
  cuts fgps <$> cast (iffsToOrIffOr fs gs)
prn k (And fs) (And gs) = do
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> prn k f_ g_) fs gs
  cuts fgps <$> cast (iffsToAndIffAnd fs gs)
prn k (Imp e f) (Imp g h) = do
  pl <- prn k e g -- pl : |- fl <=> gl 
  pr <- prn k f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ impCong e f g h
prn k (Iff e f) (Iff g h) = do
  pl <- prn k e g -- pl : |- fl <=> gl 
  pr <- prn k f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ iffCong e f g h
prn k (Fa vs f) (Fa ws g) = do
  let (k', xs) = listPars k ws
  vxs <- zipM vs xs
  wxs <- zipM ws xs
  vws <- mapM2 (\ v_ w_ -> return (v_, Var w_)) vs ws
  let f' = substForm vxs f
  let f'' = substForm vws f
  let g' = substForm wxs g
  if substForm wxs f'' == f' 
  then ru 
  else et $ "\nf = " <> ppForm f <> "\nf-sub = " <> ppForm f' <> "\nf-rnm = " <> ppForm f'' <> "\nf-rnm-sub = " <> ppForm (substForm wxs f'') <> "\n"
  p <- prn k' f' g'
  Cut (Fa ws $ f'' <=> g) (FaR ws k (f'' <=> g) p) <$> faIffToFaIffFa'' k vs f ws g
prn k (Ex vs f) (Ex ws g) = do
  let (k', xs) = listPars k ws
  vxs <- zipM vs xs
  wxs <- zipM ws xs
  vws <- mapM2 (\ v_ w_ -> return (v_, Var w_)) vs ws
  let f' = substForm vxs f
  let f'' = substForm vws f
  let g' = substForm wxs g
  if substForm wxs f'' == f' 
  then ru 
  else et $ "\nf = " <> ppForm f <> "\nf-sub = " <> ppForm f' <> "\nf-rnm = " <> ppForm f'' <> "\nf-rnm-sub = " <> ppForm (substForm wxs f'') <> "\n"
  p <- prn k' f' g'
  Cut (Fa ws $ f'' <=> g) (FaR ws k (f'' <=> g) p) <$> faIffToExIffEx'' k vs f ws g
prn _ f g
  | f == g = return $ iffRefl f
  | otherwise = et "prove-DNE"

breakFa :: Form -> Maybe ([Text], Form)
breakFa (Fa vs f) = return (vs, f)
breakFa _ = mzero

breakEx :: Form -> Maybe ([Text], Form)
breakEx (Ex vs f) = return (vs, f)
breakEx _ = mzero

pnm :: Int -> Form -> Form -> IO Prf
pnm k (Not f) (Not g) = do
  p <- pnm k f g
  return $ Cut (f <=> g) p $ iffToNotIffNot f g
pnm k (Or fs) (Or gs) = do
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> pnm k f_ g_) fs gs
  cuts fgps <$> cast (iffsToOrIffOr fs gs)
pnm k (And fs) (And gs) = do
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> pnm k f_ g_) fs gs
  cuts fgps <$> cast (iffsToAndIffAnd fs gs)
pnm k (Imp e f) (Imp g h) = do
  pl <- pnm k e g -- pl : |- fl <=> gl 
  pr <- pnm k f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ impCong e f g h
pnm k (Iff e f) (Iff g h) = do
  pl <- pnm k e g -- pl : |- fl <=> gl 
  pr <- pnm k f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ iffCong e f g h
pnm k (Fa vs f) _g 
  | L.any (`varInf` f) vs = do
    pt $ "Variables " <> ppList id vs <> " occur in " <> ppForm _g <> "\n"
    (ws, g) <- cast $ breakFa _g
    guardMsg (ws == L.filter (`varInf` f) vs) "Normalized list mismatch"
    let (k', wxs) = varPars k ws
    let f' = substForm wxs f
    let g' = substForm wxs g
    p <- pnm k' f' g'
    return $ 
      cuts
        [ (Fa vs f <=> Fa ws f, degenFaIffFa k vs ws f),
          (Fa ws (f <=> g), FaR ws k (f <=> g) p),
          (Fa ws f <=> Fa ws g, faIffToFaIffFa ws k f g) ] $
        iffTrans (Fa vs f) (Fa ws f) (Fa ws g)
  | otherwise = do 
    pt $ "Variables " <> ppList id vs <> " do not occur in " <> ppForm _g <> "\n"
    p <- pnm k f _g
    return $ cuts [(Fa vs f <=> f, degenFaIff k vs f), (f <=> _g, p)] $ iffTrans (Fa vs f) f _g
pnm k (Ex vs f) _g 
  | L.any (`varInf` f) vs = do
    (ws, g) <- cast $ breakEx _g
    guardMsg (ws == L.filter (`varInf` f) vs) "Normalized list mismatch"
    let (k', wxs) = varPars k ws
    let f' = substForm wxs f
    let g' = substForm wxs g
    p <- pnm k' f' g'
    -- return $ cutIff (Ex vs f) (Ex ws f) (degenExIffEx k vs ws f) $ 
    --   Cut (Fa ws (f <=> g)) (FaR ws k (f <=> g) p) $ faIffToExIffEx ws k f g
    return $ 
      cuts
        [ (Ex vs f <=> Ex ws f, degenExIffEx k vs ws f),
          (Fa ws (f <=> g), FaR ws k (f <=> g) p),
          (Ex ws f <=> Ex ws g, faIffToExIffEx ws k f g) ] $
        iffTrans (Ex vs f) (Ex ws f) (Ex ws g)
  | otherwise = do 
    p <- pnm k f _g
    return $ cuts [(Ex vs f <=> f, degenExIff k vs f), (f <=> _g, p)] $ iffTrans (Ex vs f) f _g
pnm _ f g
  | f == g = return $ iffRefl f
  | otherwise = et $ "prove-normalized error\nf = " <> ppForm f <> "\ng = " <> ppForm g <> "\n"

orIffFlatOr :: [Form] -> [Form] -> IO Prf
orIffFlatOr fs gs = do 
  guardMsg (flatOr fs == gs) "flatten result mismatch"
  let gps = L.map (\ g_ -> (g_, Ax g_)) gs
  p <- orLs gps (Or fs) 
  return $ iffRFull (Or fs) (Or gs) (OrR gs gs p) (orRs (Or fs) $ OrL gps)

andIffFlatAnd :: [Form] -> [Form] -> IO Prf
andIffFlatAnd fs gs = do 
  guardMsg (flatAnd fs == gs) "flatten result mismatch"
  let gps = L.map (\ g_ -> (g_, Ax g_)) gs
  p <- andRs gps (And fs) 
  return $ iffRFull (And fs) (And gs) (andLs (And fs) $ AndR gps) (AndL gs gs p) --(OrR gs gs p0) (orRs (Or fs) $ OrL gps)

pfl :: Int -> Form -> Form -> IO Prf
pfl k (Not f) (Not g) = do
  p <- pfl k f g
  return $ Cut (f <=> g) p $ iffToNotIffNot f g
pfl k (Or fs) (Or hs) = do
  let gs = L.map fltn fs
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> pfl k f_ g_) fs gs
  p0 <- cuts fgps <$> cast (iffsToOrIffOr fs gs)
  p1 <- orIffFlatOr gs hs
  return $ cuts [(Or fs <=> Or gs, p0), (Or gs <=> Or hs, p1)] $ iffTrans (Or fs) (Or gs) (Or hs)
pfl k (And fs) (And hs) = do
  let gs = L.map fltn fs
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> pfl k f_ g_) fs gs
  p0 <- cuts fgps <$> cast (iffsToAndIffAnd fs gs)
  p1 <- andIffFlatAnd gs hs
  return $ cuts [(And fs <=> And gs, p0), (And gs <=> And hs, p1)] $ iffTrans (And fs) (And gs) (And hs)
pfl k (Imp e f) (Imp g h) = do
  pl <- pfl k e g -- pl : |- fl <=> gl 
  pr <- pfl k f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ impCong e f g h
pfl k (Iff e f) (Iff g h) = do
  pl <- pfl k e g -- pl : |- fl <=> gl 
  pr <- pfl k f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ iffCong e f g h
pfl k (Fa vs f) (Fa ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- pfl k' f' g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToFaIffFa vs k f g
pfl k (Ex vs f) (Ex ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- pfl k' f' g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToExIffEx vs k f g
pfl _ f g
  | f == g = return $ iffRefl f
  | otherwise = et "prove-flatten"

pdne :: Int -> Form -> Form -> IO Prf
pdne k (Not f) (Not g) = do
  p <- pdne k f g
  return $ Cut (f <=> g) p $ iffToNotIffNot f g
pdne k (Not (Not f)) g = do
  p <- pdne k f g -- p : |- f <=> g
  -- return $ Cut (Not (Not f) <=> g) (notNotIff f) p
  return $ cuts [(Not (Not f) <=> f, notNotIff f), (f <=> g, p)] $ iffTrans (Not (Not f)) f g
pdne k (Or fs) (Or gs) = do
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> pdne k f_ g_) fs gs
  cuts fgps <$> cast (iffsToOrIffOr fs gs)
pdne k (And fs) (And gs) = do
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> pdne k f_ g_) fs gs
  cuts fgps <$> cast (iffsToAndIffAnd fs gs)
pdne k (Imp e f) (Imp g h) = do
  pl <- pdne k e g -- pl : |- fl <=> gl 
  pr <- pdne k f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ impCong e f g h
pdne k (Iff e f) (Iff g h) = do
  pl <- pdne k e g -- pl : |- fl <=> gl 
  pr <- pdne k f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ iffCong e f g h
pdne k (Fa vs f) (Fa ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- pdne k' f' g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToFaIffFa vs k f g
pdne k (Ex vs f) (Ex ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- pdne k' f' g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToExIffEx vs k f g
pdne _ f g
  | f == g = return $ iffRefl f
  | otherwise = et "prove-DNE"

sliceUsols :: [USOL] -> ([Maybe Term], [USOL])
sliceUsols [] = ([], [])
sliceUsols ([] : uss) =
  let (mxs, uss') = sliceUsols uss in
  (nt : mxs, [] : uss')
sliceUsols ((x : xs) : uss) =
  let (mxs, uss') = sliceUsols uss in
  (Just x : mxs, xs : uss')

dips :: Form -> [USOL] -> IO (Form, [Form])
dips f uss =
  if L.all L.null uss
  then return (f, [])
  else do let (mxs, uss') = sliceUsols uss
          ([], g) <- ssubf mxs f
          (h, gs) <- dips g uss'
          return (h, g : gs)

ppMaybe :: (a -> Text) -> Maybe a -> Text
ppMaybe f (Just x) = "Just " <> f x
ppMaybe _ _  = "Nothing"

-- Like StateT but with return tuple swapped
newtype StateM s m a = StateM { runStateM :: s -> m (s, a) }

instance Functor m => Functor (StateM s m) where
    fmap f (StateM x) = StateM $ \s -> fmap (\(s', a) -> (s', f a)) (x s)

instance Monad m => Applicative (StateM s m) where
    pure x = StateM $ \s -> return (s, x)
    StateM f <*> StateM x = StateM $ \s -> do (s', f') <- f s
                                              (s'', x') <- x s'
                                              return (s'', f' x')

-- | Monadic variant of 'mapAccumL'.
mapAccumM :: (Monad m, Traversable t)
          => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumM f s t = runStateM (traverse (\x -> StateM (`f` x)) t) s

ssubts :: [Maybe Term] -> [Term] -> IO ([Maybe Term], [Term])
ssubts mxs [] = return (mxs, [])
ssubts (Just x : mxs) (_ : xs) = DBF.second (x :) <$> ssubts mxs xs
ssubts (_ : mxs) (x : xs) = DBF.second (x :) <$> ssubts mxs xs
ssubts [] (_ : _) = mzero

ssubf :: [Maybe Term] -> Form -> IO ([Maybe Term], Form)
ssubf mxs (Not f) = DBF.second Not <$> ssubf mxs f
ssubf mxs (Imp f g) = do
  (mxs', f') <- ssubf mxs f
  (mxs'', g') <- ssubf mxs' g
  return (mxs'', f' ==> g')
ssubf mxs (Iff f g) = do
  (mxs', f') <- ssubf mxs f
  (mxs'', g') <- ssubf mxs' g
  return (mxs'', f' <=> g')
ssubf mxs (Or fs) = DBF.second Or <$> mapAccumM ssubf mxs fs
ssubf mxs (And fs) = DBF.second And <$> mapAccumM ssubf mxs fs
ssubf mxs (Fa vs f) = DBF.second (Fa vs) <$> ssubf mxs f
ssubf mxs (Ex vs f) = DBF.second (Ex vs) <$> ssubf mxs f
ssubf mxs (Eq x y) = do
  (mxs', [x', y']) <- ssubts mxs [x, y]
  return (mxs', Eq x' y')
ssubf mxs (Rel r xs) = DBF.second (Rel r) <$> ssubts mxs xs

definFold :: [Form] -> Form -> Form -> IO Prf
definFold es f g = do
  -- pt $ "DFF Defins =\n" <> ppListNl ppForm es <> "\n"
  -- pt $ "DFF Source = " <> ppForm f <> "\n"
  -- pt $ "DFF Target = " <> ppForm g <> "\n"
  egs <- dff es f g
  -- pt $ ppListNl (\ (e_, g_) -> ppForm e_ <> "\n================\n" <> ppForm g_ <> "\n\n") egs
  -- pt "Begin use-GM...\n"
  uegs f egs g
  -- pt "use-GM success!\n"

uufs :: Form -> [(Form, Form)] -> Form -> IO Prf
uufs f [] h = do
  guardMsg (f == h) "f and g not equal after rewriting"
  return $ Ax f
uufs f ((e, g) : egs) h = do
  -- pt $ "f to be rewritten : " <> ppForm f <> "\n"
  -- pt $ "equation e : " <> ppForm e <> "\n"
  -- pt $ "interpolant g : " <> ppForm g <> "\n"
  pl <- uuf 0 f e g -- pgh : |- h <=> g
  pr <- uufs g egs h
  return $ Cut g (Cut (f <=> g) pl (IffLO f g $ mp f g)) pr

uips :: (Form -> Form -> IO Prf) -> Form -> [Form] -> Form -> IO Prf
uips pf f [] h = do
  guardMsg (f == h) "f and g not equal after rewriting"
  return $ Ax f
uips pf f (g : gs) h = do
  pl <- pf f g
  pr <- uips pf g gs h  -- pgh : |- h <=> g
  return $ Cut g (Cut (f <=> g) pl (IffLO f g $ mp f g)) pr

uegs :: Form -> [(Form, Form)] -> Form -> IO Prf
uegs f [] h = do
  guardMsg (f == h) "f and g not equal after rewriting"
  return $ Ax f
uegs f ((e, g) : egs) h = do
  pl <- uegs f egs g
  phg <- ueg 0 HM.empty h e g -- pgh : |- h <=> g
  return $ Cut g pl $ Cut (h <=> g) phg (IffLR h g $ mp g h)


ueg :: Int -> VM -> Form -> Form -> Form -> IO Prf
ueg k gm f e g =
  if f == g
  then return $ iffRefl f
  else uegt gm f e g <|> uegr k gm f e g

cuts :: [(Form, Prf)] -> Prf -> Prf
cuts [] = id
cuts ((f, p) : fps) = Cut f p . cuts fps

uut :: Term -> Form -> Term -> IO Prf
uut x e y =
  if x == y
  then return $ EqR x
  else uutt x e y <|> uutr x e y

uutr :: Term -> Form -> Term -> IO Prf
uutr (Fun f xs) e (Fun g ys) = do
  guard $ f == g
  xyps <- mapM2 (\ x_ y_ -> (x_, y_,) <$> uut x_ e y_) xs ys
  return $ FunC f xyps
uutr _ _ _ = et "uutr cannot recurse"

uutt :: Term -> Form -> Term -> IO Prf
uutt x (Eq a b) y =
  case (x == a, y == b, x == b, y == a) of
    (True, True, _, _) -> return $ Ax (x === y)
    (_, _, True, True) -> return $ EqS a b
    _ -> mzero
uutt x (Fa vs (Eq a b)) y =
  case (specs HM.empty [(a, x), (b, y)], specs HM.empty [(a, y), (b, x)] ) of
    (Just vm, _) -> do
      vxs <- mapM (\ v_ -> (v_ ,) <$> cast (HM.lookup v_ vm)) vs
      return $ FaL vxs (a === b) $ Ax (x === y)
    (_, Just vm) -> do
      vxs <- mapM (\ v_ -> (v_ ,) <$> cast (HM.lookup v_ vm)) vs
      return $ FaL vxs (a === b) $ EqS y x
    _ -> mzero
uutt _ f _ = et "not an equation"

uuf :: Int -> Form -> Form -> Form -> IO Prf
uuf k f@(Rel r xs) e g@(Rel s ys) = do
  guard $ r == s
  xyps <- mapM2 (\ x_ y_ -> (x_, y_,) <$> uut x_ e y_) xs ys
  return $ relCong r xyps
uuf k (Fa vs f) e (Fa ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- uuf k' f' e g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToFaIffFa vs k f g
uuf k (Not f) e (Not g) = do
  p <- uuf k f e g
  return $ Cut (f <=> g) p $ iffToNotIffNot f g
uuf _ f _ g = et $ "uuf unimplmented case,\nf : " <> ppForm f <> "\ng : " <> ppForm g <> "\n"

puf :: Int -> [Form] -> Form -> Form -> IO Prf
puf k es (Not f) (Not g) = do
  p <- puf k es f g
  return $ Cut (f <=> g) p $ iffToNotIffNot f g
puf k es (Or fs) (Or gs) = do
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> puf k es f_ g_) fs gs
  cuts fgps <$> cast (iffsToOrIffOr fs gs)
puf k es (And fs) (And gs) = do
  fgps <- mapM2 (\ f_ g_ -> (f_ <=> g_,) <$> puf k es f_ g_) fs gs
  cuts fgps <$> cast (iffsToAndIffAnd fs gs)
puf k es (Imp e f) (Imp g h) = do
  pl <- puf k es e g -- pl : |- fl <=> gl 
  pr <- puf k es f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ impCong e f g h
puf k es (Iff e f) (Iff g h) = do
  pl <- puf k es e g -- pl : |- fl <=> gl 
  pr <- puf k es f h -- pl : |- fr <=> gr
  return $ Cut (e <=> g) pl $ Cut (f <=> h) pr $ iffCong e f g h
puf k es (Fa vs f) (Fa ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- puf k' es f' g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToFaIffFa vs k f g
puf k es (Ex vs f) (Ex ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  p <- puf k' es f' g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToExIffEx vs k f g
puf _ es (Rel r xs) (Rel s ys) = do
  xyps <- mapM2 (\ x_ y_ -> (x_, y_,) <$> put es x_ y_) xs ys
  return $ relCong r xyps
puf _ es (Eq a b) (Eq x y) = do
  pax <- put es a x
  pby <- put es b y
  return $ eqCong (a, x, pax) (b, y, pby)
puf _ _ f g =
  et $ "PUF failed:\n f : " <> ppForm f <> "\ng : " <> ppForm g <> "\n"

vmToVxs :: VM -> [Text] -> Maybe [(Text, Term)]
vmToVxs vm = mapM (\ v_ -> (v_,) <$> HM.lookup v_ vm)

vxsToVm :: [(Text, Term)] -> VM
vxsToVm = L.foldl (\ vm_ (v_, x_) -> HM.insert v_ x_ vm_) HM.empty

putt :: Term -> Term -> Form -> IO Prf
putt x y f@(Fa vs (Eq a b)) =
  ( do vm <- cast $ specs HM.empty [(a, x), (b, y)]
       vxs <- cast $ vmToVxs vm vs
       return $ FaL vxs (a === b) $ Ax (x === y) ) <|>
  ( do vm <- cast $ specs HM.empty [(a, y), (b, x)]
       vxs <- cast $ vmToVxs vm vs
       return $ FaL vxs (a === b) $ EqS y x )
putt x y (Eq a b)
  | x == a && y == b = return $ Ax (x === y)
  | x == b && y == a = return $ EqS y x
  | otherwise = mzero
putt x y _ = et "putt : not an equation"

put :: [Form] -> Term -> Term -> IO Prf
put es x@(Fun f xs) y@(Fun g ys)
  | x == y = return $ EqR x
  | otherwise =
    first (putt x y) es <|> do
      guard $ f == g
      xyps <- mapM2 (\ x_ y_ -> (x_, y_,) <$> put es x_ y_) xs ys
      return $ FunC f xyps
put es x y
  | x == y = return $ EqR x
  | otherwise = first (putt x y) es

uegr :: Int -> VM -> Form -> Form -> Form -> IO Prf
uegr k gm (Not f) e (Not g) = do
  p <- ueg k gm f e g
  return $ Cut (f <=> g) p $ iffToNotIffNot f g
uegr k gm (Or fs) e (Or gs) = do
  ps <- mapM2 (\ f_ g_ -> ueg k gm f_ e g_) fs gs
  fgs <- mapM2 (\ f_ g_ -> return $ Iff f_ g_) fs gs
  fgps <- zipM fgs ps
  cuts fgps <$> cast (iffsToOrIffOr fs gs)
uegr k gm (And fs) e (And gs) = do
  ps <- mapM2 (\ f_ g_ -> ueg k gm f_ e g_) fs gs
  fgs <- mapM2 (\ f_ g_ -> return $ Iff f_ g_) fs gs
  fgps <- zipM fgs ps
  cuts fgps <$> cast (iffsToAndIffAnd fs gs)
uegr k gm (Imp fl fr) e (Imp gl gr) = do
  pl <- ueg k gm fl e gl -- pl : |- fl <=> gl 
  pr <- ueg k gm fr e gr -- pl : |- fr <=> gr
  return $ Cut (fl <=> gl) pl $ Cut (fr <=> gr) pr $ impCong fl fr gl gr
uegr k gm (Iff fl fr) e (Iff gl gr) = do
  pl <- ueg k gm fl e gl -- pl : |- fl <=> gl 
  pr <- ueg k gm fr e gr -- pl : |- fr <=> gr
  return $ Cut (fl <=> gl) pl $ Cut (fr <=> gr) pr $ iffCong fl fr gl gr
uegr k gm (Fa vs f) e (Fa ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  gm' <- foldM (\ gm_ (v_, x_) -> cast $ addVM v_ x_ gm_) gm vxs
  p <- ueg k' gm' f' e g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToFaIffFa vs k f g
uegr k gm (Ex vs f) e (Ex ws g) = do
  guard (vs == ws)
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  let g' = substForm vxs g
  gm' <- foldM (\ gm_ (v_, x_) -> cast $ addVM v_ x_ gm_) gm vxs
  p <- ueg k' gm' f' e g'
  return $ Cut (Fa vs $ f <=> g) (FaR vs k (f <=> g) p) $ faIffToExIffEx vs k f g
uegr _ _ f e g =
  et $ "UEGR failed:\n f : " <> ppForm f <> "\ne : " <> ppForm e <> "\ng : " <> ppForm g <> "\n"

uegt :: VM -> Form -> Form -> Form -> IO Prf
uegt gm f (Iff l r) g = do
  guard $ l == f
  guard $ r == g
  return $ Ax (f <=> g)
uegt gm f (Fa vs (Iff l r)) g = do
  vxs <- cast $ mapM (\ v_ -> (v_ ,) <$> HM.lookup v_ gm) vs
  let l' = substForm vxs l
  let r' = substForm vxs r
  guard $ l' == f
  guard $ r' == g
  return $ FaL vxs (l <=> r) $ Ax (f <=> g)
uegt _ _ e _ = et $ "Not a definition : " <> ppForm e

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

revDir :: Dir -> Dir
revDir Obv = Rev
revDir Rev = Obv

formPreds :: Form -> Set Text
formPreds (Eq _ _) = S.empty
formPreds (Rel r _) = S.singleton r
formPreds (Not f) = formPreds f
formPreds (Or fs) = L.foldl S.union S.empty $ L.map formPreds fs
formPreds (And fs) = L.foldl S.union S.empty $ L.map formPreds fs
formPreds (Imp f g) = S.union (formPreds f) (formPreds g)
formPreds (Iff f g) = S.union (formPreds f) (formPreds g)
formPreds (Fa _ f) = formPreds f
formPreds (Ex _ f) = formPreds f

diffPreds :: Form -> Form -> Set Text
diffPreds f g = S.difference (formPreds f) (formPreds g)

removeByPreds :: Set Text -> [Form] -> BT [Form]
removeByPreds rs [] = return []
removeByPreds rs (f : fs) = do
  fs' <- removeByPreds rs fs
  return (f : fs') <|> (guard (removeTarget rs f) >> return fs')

removeTarget :: Set Text -> Form -> Bool
removeTarget rs f = not $ S.disjoint rs $ formPreds f

flat :: Form -> Form -> IO Prf
flat f g = do
  let f' = qmerge $ dne $ fltn f
  if f' == g
  then return Asm
  else do (sg, sf) <- ndiff g f'
          pt $ "Flattened result does not match,\nsg = " <> ppForm sg <> "\nsf = " <> ppForm sf <> "\n"
          error "FLAT-FAIL"

ndiff :: Form -> Form -> IO (Form, Form)
ndiff (Fa vs f) (Fa ws g) =
  if vs == ws
  then ndiff f g
  else return (Fa vs f, Fa ws g)
ndiff (Ex vs f) (Ex ws g) =
  if vs == ws
  then ndiff f g
  else return (Fa vs f, Fa ws g)
ndiff f@(Or fs) g@(Or gs) =
  case zipM fs gs of
    Just fgs -> first (uncurry ndiff) fgs
    _ -> return (f, g)
ndiff f@(And fs) g@(And gs) =
  case zipM fs gs of
    Just fgs -> first (uncurry ndiff) fgs
    _ -> return (f, g)
ndiff f g = guard (f /= g) >> return (f, g)


ppr :: Form -> Form -> IO Prf
ppr f g = do
  -- pt "PPR step.\n"
  let rs = diffPreds f g
  -- pt $ "Removed predicates : " <> ppList id (S.toList rs) <> "\n"
  -- pt $ "f : " <> ppForm f <> "\n"
  -- pt $ "g : " <> ppForm g <> "\n"
  let f' = pprf rs True f
  if f' == g
  then return Asm
  else do pt $ "PPR result does not match, f' = " <> ppForm f' <> "\n"
          error "PPR-FAIL"

pp :: Set Text -> Form -> Bool
pp rs (Rel r _) = S.member r rs
pp _ _ = False

pprf :: Set Text -> Bool -> Form -> Form
pprf rs b (Not f) =
  let f' = pprf rs (not b) f in
  case f' of
    And [] -> Or []
    Or [] -> And []
    _ -> Not f'
pprf rs b (Or fs) =
  let fs' = flatOr (L.map (pprf rs b) fs) in
  if And [] `elem` fs'
  then And []
  else case L.filter (Or [] /=) fs' of
         [f] -> f
         fs'' -> Or fs''
pprf rs b (And fs) =
  let fs' = flatAnd (L.map (pprf rs b) fs) in
  if Or [] `elem` fs'
  then Or []
  else case L.filter (And [] /=) fs' of
         [f] -> f
         fs'' -> And fs''
pprf rs b (Imp f g) =
  let f' = pprf rs (not b) f in
  let g' = pprf rs b g in
  case (f', g') of
    (And [], _) -> g'
    (Or [], _) -> And []
    (_, And []) -> And []
    (_, Or []) -> Not f'
    _ -> Imp f' g'
pprf rs b f@(Iff _ _) = f
pprf rs b (Fa vs f) =
  case pprf rs b f of
    And [] -> And []
    Or [] -> Or []
    Fa ws f' -> Fa (vs ++ ws) f'
    f' -> Fa vs f'
pprf rs b (Ex vs f) =
  case pprf rs b f of
    And [] -> And []
    Or [] -> Or []
    Ex ws f' -> Ex (vs ++ ws) f'
    f' -> Ex vs f'
pprf rs b f@(Rel r _) =
  if S.member r rs
  then if b then And [] else Or []
  else f
pprf rs _ f@(Eq _ _) = f

guardMsg :: (Alternative m, Monad m) => Bool -> Text -> m ()
guardMsg True _ = return ()
guardMsg False s = error (unpack s)

andRs :: [(Form, Prf)] -> Form -> IO Prf
andRs fps (And fs) = AndR <$> mapM (\ f_ -> (f_,) <$> andRs fps f_) fs
andRs fps f = cast $ snd <$> L.find ((f ==) . fst) fps

orLs :: [(Form, Prf)] -> Form -> IO Prf
orLs fps (Or fs) = OrL <$> mapM (\ f_ -> (f_,) <$> orLs fps f_) fs
orLs fps f = cast $ snd <$> L.find ((f ==) . fst) fps

orRs :: Form -> Prf -> Prf
orRs (Or fs) p = OrR fs fs $ L.foldl (flip orRs) p fs
orRs _ p = p

andLs :: Form -> Prf -> Prf
andLs (And fs) p = AndL fs fs $ L.foldl (flip andLs) p fs
andLs _ p = p

uvm :: Int -> VR -> Form -> Form -> IO Prf
uvm k vr f g =
  if f == g
  then return $ Ax f
  else uvmr k vr f g

uvmr :: Int -> VR -> Form -> Form -> IO Prf
uvmr k vm (Or fs) (Or gs) = do
  let fs' = flatOr fs
  let gs' = flatOr gs
  fps <- useOrVR k vm fs' gs'
  -- orLs fps (Or fs) >>= orRs (Or gs)
  orRs (Or gs) <$> orLs fps (Or fs) 

uvmr k vm (And fs) (And gs) = do
  let fs' = flatAnd fs
  let gs' = flatAnd gs
  gps <- useAndVR k vm fs' gs'
  -- andRs gps (And gs) >>= andLs (And fs)
  andLs (And fs) <$> andRs gps (And gs) 

uvmr _ _ f@(Rel _ _) g@(Rel _ _) = do
  guard (f == g)
  return $ Ax f

uvmr _ _ f@(Eq x y) (Eq z w)
  | x == z && y == w = return $ Ax f
  | x == w && y == z = return $ EqS x y
  | otherwise = mzero

uvmr k vm (Not f) (Not g) = do
  p <- uvm k (swap vm) g f
  return $ notLR f g p
uvmr k vm (Not (Not f)) g = do
  p <- uvm k vm f g
  return $ NotL (Not f) $ NotR f p
uvmr k vm f (Not (Not g)) = do
  p <- uvm k vm f g
  return $ NotR (Not g) $ NotL g p

uvmr k vm (Imp fl fr) (Imp gl gr) = do
  pl <- uvm k (swap vm) gl fl
  pr <- uvm k vm fr gr
  return $ ImpL fl fr (ImpRA gl gr pl) (ImpRC gl gr pr)

uvmr k vm f@(Iff fl fr) g@(Iff gl gr) = do
  pol <- uvm k (swap vm) gl fl -- pol : gl |- fl
  por <- uvm k vm fr gr         -- por : fr |- gr
  prr <- uvm k (swap vm) gr fr -- prr : gr |- fr
  prl <- uvm k vm fl gl         -- prl : fl |- gl
  let po = IffLO fl fr $ ImpL fl fr pol por -- po : gl |- gr
  let pr = IffLR fl fr $ ImpL fr fl prr prl -- pr : gr |- gl
  return $ IffR gl gr (impRAC gl gr po) (impRAC gr gl pr)

uvmr k vm (Fa vs f) (Fa ws g) = do
  let (k', xs) = listPars k ws
  wxs <- zipM ws xs
  let ys = L.map (pairWithVR vm wxs) vs
  vys <- zipM vs ys
  let f' = substForm vys f
  let g' = substForm wxs g
  p <- uvm k' vm f' g'
  return $ FaR ws k g $ FaL vys f p

uvmr k vm (Ex vs f) (Ex ws g) = do
  let (k', xs) = listPars k vs
  vxs <- zipM vs xs
  let ys = L.map (pairWithVR (swap vm) vxs) ws
  wys <- zipM ws ys
  let f' = substForm vxs f
  let g' = substForm wys g
  p <- uvm k' vm f' g'
  return $ ExL vs k f $ ExR wys g p

uvmr _ _ f g = mzero -- error $ unpack $ "use-VR umimplemented :\n" <> "f = " <> ppForm f <> "\ng = " <> ppForm g <> "\n"

useOrVR :: Int -> VR -> [Form] -> [Form] -> IO  [(Form, Prf)]
useOrVR _ _ [] [] = return []
useOrVR k vm (f : fs) ggs = do
  (p, gs) <- first (\ (g_, gs_) -> (, gs_) <$> uvm k vm f g_) $ plucks ggs
  fps <- useOrVR k vm fs gs
  return $ (f, p) : fps
useOrVR _ _ [] (_ : _) = mzero

useAndVR :: Int -> VR -> [Form] -> [Form] -> IO  [(Form, Prf)]
useAndVR _ _ [] [] = return []
useAndVR k vm ffs (g : gs) = do
  (p, fs) <- first (\ (f_, fs_) -> (, fs_) <$> uvm k vm f_ g) $ plucks ffs
  gps <- useAndVR k vm fs gs
  return $ (g, p) : gps
useAndVR _ _ (_ : _) [] = mzero

pairWithVR :: VR -> [(Text, Term)] -> Text -> Term
pairWithVR (vw, _) wxs v =
  fromMaybe zt ( do w <- HM.lookup v vw
                    snd <$> L.find ((w ==) . fst) wxs )

-- avatarContra :: Form -> Form -> IO Prf
-- avatarContra f g = do 
--   _

-- avatarContra :: Form -> Form -> IO Ctx
-- avatarContra f g = do
--   (gs, k, c) <- cast $ appConc (g, 0) blank
--   (_, gls, c') <- cast $ appPrem Nothing (f, k) c
--   cast $ litsMap Exact gls gs c'

avatarComp :: Form -> Form -> IO Prf -- Ctx
avatarComp f@(Iff l r) g =
  avatarCompCore f g <|>
  ( do p <- avatarCompCore (Iff r l) g
       return $ Cut (r <=> l) (iffSym l r) p )
avatarComp _ _ = et "avatar-comp : not iff"

avatarCompCore :: Form -> Form -> IO Prf -- Ctx
avatarCompCore (Iff s f) g = do
  (_, ggs, pp) <- useConc 0 g
  -- (_, ggs) <- cast $ concLits 0 g
  gs <- cast $ deleteOnce (Not s) ggs
  fs <- cast $ premLits f
  vm <- mapSearch HM.empty fs gs
  pf <- usePrem (mapPremLit gs) vm f
  return $ pp $ NotR s $ Cut f (iffMP s f) pf
avatarCompCore _ _ = et "avatar-comp-core : not iff"

expandLit :: Int -> Form -> Form -> IO (Int, [Form], Prf -> Prf)
expandLit k (Not g) (Iff g' h)
  | g == g' = return (k, [Not h], \ p_ -> Cut (Not h) p_ (notLR h g $ iffMP g h))
expandLit k g (Iff g' h)
  | g == g' = do
    (k', hs, ph) <- useConc k h
    return (k', hs, \ p_ -> Cut h (ph p_) (iffMPR g h) )
expandLit _ _ _ = mzero

expandLits :: [Form] -> Int -> [Form] -> IO (Int, [Form], Prf -> Prf)
expandLits ds k [] = return (k, [], id)
expandLits ds k (g : gs) = do
  (k', hs, pg) <- first (expandLit k g) ds
  (k'', hs', pgs) <- expandLits ds k' gs
  return (k'', hs ++ hs', pg . pgs)

--   d <- pick ds
--   (f, c1) <- cast $ expandResult d g c0
--   (m, n0, c2) <- cast $ appCut nt (Not f, k) c1
--   ((_, _, n1), c3) <- cast $ appNotLR (Not f, Not g, n0) c2
--   ((gf, n2), c4) <- cast (appIffLO (d, n1) c3) <|> cast (appIffLR (d, n1) c3)
--   c5 <- cast $ appMP gf (g, f, n2) c4
--   return (Not f : hs, m, c5)
-- expandSplit ds (hs, k, c) g = do
--   d <- pick ds
--   (g', k', c') <- cast $ rwro d g k c <|> rwrr d g k c
--   (hs', k'', c'') <- cast $ appConc (g', k') c'
--   return (hs' ++ hs, k'', c'')

avatarSplit :: [Form] -> Form -> Form -> IO Prf
avatarSplit ds f g = do
  (k, gs, pg) <- useConc 0 g
  (_, hs, ph) <- expandLits ds k gs
  vm <- cast $ searchCnf (hs ++ gs) (HM.empty, [f])
  pf <- proveCnfTrans vm f (hs ++ gs)
  return $ pg $ ph pf

addVM :: Text -> Term -> VM -> Maybe VM
addVM v x gm =
  case HM.lookup v gm of
    Just y ->
      if x == y
      then return gm
      else nt
    _ -> return $ HM.insert v x gm

gndTerm :: Term -> Term
gndTerm (Var _) = zt
gndTerm (Fun f xs) = Fun f $ L.map gndTerm xs
gndTerm x = x

initVC :: Set Text -> Set Text -> VC
initVC vs ws =
  let lvs = S.toList vs in
  let lws = S.toList ws in
  let vws = HM.fromList (L.map (, ws) lvs) in
  let wvs = HM.fromList (L.map (, vs) lws) in
  (vws, wvs)

emptyVR :: VR
emptyVR = (HM.empty, HM.empty)

conTerms :: VC -> [Term] -> [Term] -> Maybe VC
conTerms vm [] [] = return vm
conTerms vm (x : xs) (y : ys) = do
  vm' <- conTerm vm x y
  conTerms vm' xs ys
conTerms _ _ _ = nt

conTerm :: VC -> Term -> Term -> Maybe VC
conTerm vc (Var v) (Var w) =
  case conVar v w vc of
    Just vc' -> Just vc'
    _ -> mzero
conTerm vm (Fun f xs) (Fun g ys) = do
  guard (f == g) -- "Cannot ground function"
  conTerms vm xs ys
conTerm vm x y = do
  guard (x == y) -- "Cannot ground pair\n" <> ppTerm x <> "\n" <> ppTerm y <> "\n"
  return vm

agvmt :: VM -> Term -> Term
agvmt gm (Var v) =
  case HM.lookup v gm of
    Just x -> gndTerm x
    _ -> zt
agvmt gm (Fun f xs) = Fun f $ L.map (agvmt gm) xs
agvmt _ x = x

avmt :: VM -> Term -> Term
avmt gm (Var v) =
  case HM.lookup v gm of
    Just x -> x
    _ -> Var v
avmt gm (Fun f xs) = Fun f $ L.map (avmt gm) xs
avmt _ x = x

tavmt :: VM -> Term -> Maybe Term
tavmt gm (Var v) =
  case HM.lookup v gm of
    Just x -> return x
    _ -> mzero -- return $ Var v
tavmt gm (Fun f xs) = Fun f <$> mapM (tavmt gm) xs
tavmt _ x = return x

tavmf :: VM -> Form -> Maybe Form
tavmf gm (Eq x y) = do
  x' <- tavmt gm x
  y' <- tavmt gm y
  return (Eq x' y')
tavmf gm (Rel r xs) = Rel r <$> mapM (tavmt gm) xs
tavmf gm (Or fs) = Or <$> mapM (tavmf gm) fs
tavmf gm (And fs) = And <$> mapM (tavmf gm) fs
tavmf gm (Not f) = do
  f' <- tavmf gm f
  return (Not f')
tavmf gm (Imp f g) = do
  f' <- tavmf gm f
  g' <- tavmf gm g
  return (Imp f' g')
tavmf gm (Iff f g) = do
  f' <- tavmf gm f
  g' <- tavmf gm g
  return (Iff f' g')
tavmf gm (Fa vs f) = Fa vs <$> tavmf gm f
tavmf gm (Ex vs f) = Ex vs <$> tavmf gm f

aoct :: [Term] -> [Text] -> VM -> Term -> Term -> IO VM
aoct zs ws vm (Var v) y@(Fun f xs) =
  case HM.lookup v vm of
    Just x -> guard (x == y) >> return vm
    _ -> guard (isPerm zs xs) >> return (HM.insert v y vm)
aoct zs ws vm x@(Fun f xs) y@(Fun g ys)
  | x == y = return vm
  | f == g = foldM2 (aoct zs ws) vm xs ys
  | otherwise = mzero
aoct zs ws vm x y
  | x == y = return vm
  | otherwise = mzero

aocf :: [Term] -> [Text] -> VM -> Form -> Form -> IO VM
aocf zs ws vm (Eq x y) (Eq z w) =
  foldM2 (aoct zs ws) vm [x, y] [z, w] <|> foldM2 (aoct zs ws) vm [x, y] [w, z]
aocf zs ws vm (Rel r xs) (Rel s ys) = do
  guard $ r == s
  foldM2 (aoct zs ws) vm xs ys
aocf zs us vm (Not f) (Not g) = aocf zs us vm f g
aocf zs us vm (Or fs) (Or gs) = foldM2 (aocf zs us) vm fs gs
aocf zs us vm (And fs) (And gs) = foldM2 (aocf zs us) vm fs gs
aocf zs us vm (Fa vs f) (Fa ws g) = do
  guard $ isPerm vs ws
  aocf zs (ws L.\\ vs) vm f g
aocf zs us vm (Ex vs f) (Ex ws g) = do
  guard $ isPerm vs ws
  aocf zs (ws L.\\ vs) vm f g
aocf zs ws vm f g = et "aocf : todo"

normalizeAOC :: Form -> IO ([Term], Form)
normalizeAOC (Fa vs (Imp (Ex ws f) g)) = do
  vm <- aocf (L.map Var vs) ws HM.empty  f g
  -- pt $ "VM =\n" <> ppVM vm 
  xs <- cast $ mapM (`HM.lookup` vm) ws
  return (xs, Fa vs (Imp (Ex ws f) (subf vm f)))
normalizeAOC (Imp (Ex ws f) g) = do
  vm <- aocf [] ws HM.empty f g
  xs <- cast $ mapM (`HM.lookup` vm) ws
  return (xs, Imp (Ex ws f) (subf vm f))
normalizeAOC _ = mzero


mkRdef :: Text -> Form -> Maybe Form
mkRdef r (Fa vs g) = do
  f <- mkRdef r g
  return (Fa vs f)
mkRdef r (Iff (Rel s xs) f) = guard (r == s) >> return (Iff (Rel s xs) f)
mkRdef r (Iff f (Rel s xs)) = guard (r == s) >> return (Iff (Rel s xs) f)
mkRdef r (Or [f, Not (Rel s xs)]) = guard (r == s) >> return (Iff (Rel s xs) f)
mkRdef r (Or fs) = do
  (fs', Not (Rel s xs)) <- desnoc fs
  guard (r == s)
  return (Iff (Rel s xs) (Or fs'))
mkRdef _ f = error $ unpack $ "Cannot make rdef :\n" <> ppFormNl f

desnoc :: [a] -> Maybe ([a], a)
desnoc [] = nt
desnoc [x] = return ([], x)
desnoc (x : xs) = DBF.first (x :) <$> desnoc xs

proveRdef :: Form -> Form -> IO Prf
proveRdef (Fa vs f) (Fa ws g) = do
  guard (vs == ws)
  let (_, xs) = listPars 0 vs
  vxs <- zipM vs xs
  p <- proveRdef' (substForm vxs f) (substForm vxs g)
  return $ FaR ws 0 g $ FaL vxs f p
proveRdef f g = proveRdef' f g

rDefLemma0 :: Form -> Form -> Prf
rDefLemma0 f g =
  let p = IffLO f g (mp f g) in -- f, f <=> g |- g
  OrR [g, Not f] [g, Not f] $ NotR f p -- f <=> g |- g \/ ~f

rDefLemma1 :: Form -> [Form] -> [Form] -> Prf
rDefLemma1 r fs fsnr =
  let pl = rDefLemma0 r (Or fs) in -- (r <=> \/ fs) |- (\/ fs) \/ ~r
  let ps = L.map (\ f_ -> (f_, Ax f_)) fs in
  let pfsnr = OrL [(Or fs, OrL ps), (Not r, Ax (Not r))] in -- pfsnr : (\/ fs) \/ ~r |- fs, ~r
  let pr = OrR fsnr fsnr pfsnr in                           -- ps    : (\/ fs) \/ ~r |- \/ fsnr
  Cut (Or [Or fs, Not r]) pl pr -- (r <=> \/ fs) |- \/ fsnr

proveRdef' :: Form -> Form -> IO Prf
proveRdef' f@(Iff fl fr) g@(Iff gl gr) =
  (guard (f == g) >> return (Ax f)) <|>
  (guard (fl == gr && fr == gl) >> return (iffSym fl fr))
proveRdef' (Iff r b) (Or [b', Not r'])  = do
  guard (r == r' && b == b')
  return $ rDefLemma0 r b
proveRdef' (Iff r (Or fs)) (Or fsnr)  = do
  (fs', Not r') <- cast $ desnoc fsnr
  guard (r == r' && fs == fs')
  return $ rDefLemma1 r fs fsnr
proveRdef' f g = et $ "Anomaly! : " <> ppForm f <> " |- " <> ppForm g <> "\n"

relDef :: Text -> Form -> IO Elab
relDef r g = do
  f <- cast $ mkRdef r g
  -- c <- cast $ proveRdef (f, g, 0) blank
  -- p <- cast $ extractPrf c
  p <- proveRdef f g
  return $ Rdef r f p

addTicks :: Int -> Text -> Text
addTicks 0 t = t
addTicks k t = addTicks (k - 1) t <> "'"

type MTT = HM.Map Text Text
type MTI = HM.Map Text Int

rnt :: MTT -> Term -> Maybe Term
rnt m (Var v) = do
  w <- HM.lookup v m
  return $ Var w
rnt m (Fun f xs) = Fun f <$> mapM (rnt m) xs
rnt m x = return x

rnfs :: MTT -> MTI -> [Form] -> Maybe ([Form], MTI)
rnfs tt ti [] = return ([], ti)
rnfs tt ti (f : fs) = do
  (f', ti') <- rnf tt ti f
  DBF.first (f' :) <$> rnfs tt ti' fs

rnf :: MTT -> MTI -> Form -> Maybe (Form, MTI)
rnf tt ti (Rel r xs) = do
  xs' <- mapM (rnt tt) xs
  return (Rel r xs', ti)
rnf tt ti (Eq x y) = do
  [x', y'] <- mapM (rnt tt) [x, y]
  return (Eq x' y', ti)
rnf tt ti (Not f) = DBF.first Not <$> rnf tt ti f
rnf tt ti (Or fs) = DBF.first Or <$> rnfs tt ti fs
rnf tt ti (And fs) = DBF.first And <$> rnfs tt ti fs
rnf tt ti (Imp f g) = do
  ([f', g'], ti') <- rnfs tt ti [f, g]
  return (Imp f' g', ti')
rnf tt ti (Iff f g) = do
  ([f', g'], ti') <- rnfs tt ti [f, g]
  return (Iff f' g', ti')
rnf tt ti (Fa vs f) = do
  let (vs', tt', ti') = rnvs tt ti vs
  (f', ti'') <- rnf tt' ti' f
  return (Fa vs' f', ti'')
rnf tt ti (Ex vs f) = do
  let (vs', tt', ti') = rnvs tt ti vs
  (f', ti'') <- rnf tt' ti' f
  return (Ex vs' f', ti'')

rnvs :: MTT -> MTI -> [Text] -> ([Text], MTT, MTI)
rnvs tt ti [] = ([], tt, ti)
rnvs tt ti (v : vs) =
  let k = 1 + HM.findWithDefault (-1) v ti in
  let v' = addTicks k v in
  let tt' = HM.insert v v' tt in
  let ti' = HM.insert v k ti in
  let (vs', tt'', ti'') = rnvs tt' ti' vs in
  (v' : vs', tt'', ti'')

-- elab' :: NSeq -> AnForm -> IO ()
-- elab' s (Af _ g (Just (Gfun "inference" [Gfun r [], _, Glist l]))) = do
--   fs <- cast (mapM gFunFunctor l) >>= mapM (`lookupM` s)
--   infer' r fs g
-- elab' _ _ = return ()

elab :: NSeq -> AnForm -> IO Elab
elab s (Af n h (Just (Gfun "file" [_, Gfun m []]))) = do
  f <- getHyp m s
  Plab <$> orig f h
elab _ (Af n g (Just (Gfun "introduced" [Gfun "predicate_definition_introduction" [],Glist [Gfun "new_symbols" [Gfun "naming" [],Glist [Gfun r []]]]]))) = relDef r g
elab _ (Af n g (Just (Gfun "introduced" [Gfun "avatar_definition" [], Glist [Gfun "new_symbols" [Gfun "naming" [], Glist [Gfun r []]]]]))) = relDef r g
elab s (Af n g (Just (Gfun "introduced" [Gfun "choice_axiom" [], Glist []]))) = do
  (xs, f) <- normalizeAOC g
  p <- orig f g
  return $ AOC xs f p
elab c (Af m g (Just (Gfun "inference" [Gfun "true_and_false_elimination" [], _, Glist [Gfun n []]]))) =
  do f <- getHyp n c
     if tfsimp f == g
     then return $ Tfe f
     else error (unpack $ "TF-fail:\n" <> ppForm (tfsimp f) <> "\n" <> ppForm g)
elab c (Af m g (Just (Gfun "inference" [Gfun "ennf_transformation" [], _, Glist [Gfun n []]]))) =
  do f <- getHyp n c
     return $ Nnf True f
elab c (Af m g (Just (Gfun "inference" [Gfun "nnf_transformation" [], _, Glist [Gfun n []]]))) =
  do f <- getHyp n c
     return $ Nnf False f
elab s (Af _ g (Just (Gfun "inference" [Gfun "avatar_sat_refutation" [], _, Glist l]))) = do
  fs <- cast (mapM gFunFunctor l) >>= mapM (`lookupM` s)
  sat fs <&> Lrats fs
elab s (Af _ g (Just (Gfun "inference" [Gfun r [], _, Glist l]))) = do
  fs <- cast (mapM gFunFunctor l) >>= mapM (`lookupM` s)
  p <- infer r fs g
  return $ Plab p

elab _ (Af _ _ a) = error $ "Unimplemented case : " ++ show a

cnfTrans :: Form -> Form -> IO Prf
cnfTrans f g = do
  -- pt "CNF-Trans :\n"
  -- pt $ "f : " <> ppForm f <> "\n"
  -- pt $ "g : " <> ppForm g <> "\n"
  (_, gs, pp) <- useConc 0 g
  -- pt $ "gs : " <> ppList ppForm gs <> "\n"
  vm <- cast $ searchCnf gs (HM.empty, [f])
  -- pt $ "vm :\n" <> ppVM vm <> "\n"
  pf <- proveCnfTrans vm f gs
  return $ pp pf

useConcs :: Int -> [Form] -> IO (Int, [Form], Prf -> Prf)
useConcs k [] = return (k, [], id)
useConcs k (f : fs) = do
  (k', fs', pf) <- useConc k f
  (k'',fs'', pf') <- useConcs k' fs
  return (k'', fs' ++ fs'', pf . pf')

useConc :: Int -> Form -> IO (Int, [Form], Prf -> Prf)
useConc k (Fa vs f) = do
  let (k', vxs) = varPars k vs
  let f' = substForm vxs f
  (k'', fs, pf) <- useConc k' f'
  return (k'', fs, FaR vs k f . pf)
useConc k (Or fs) = do
  (k', fs', pp) <- useConcs k fs
  return (k', fs', OrR fs fs . pp)
useConc k g =
  if isLit g
  then return (k, [g], id)
  else et $ "use conc : not lit : " <> ppForm g

varParsVM :: Int -> [Text] -> (Int, [(Text, Term)], VM)
varParsVM k vs =
  let (k', vxs) = varPars k vs in
  let vm = L.foldl (\ vm_ (v_, x_) -> HM.insert v_ x_ vm_) HM.empty vxs in
  (k', vxs, vm)

concsLits :: Int -> [Form] -> Maybe (Int, [Form])
concsLits k [] = return (k, [])
concsLits k (f : fs) = do
  (k', fs') <- concLits k f
  (k'', fs'') <- concsLits k' fs
  return (k'', fs' ++ fs'')

concLits :: Int -> Form -> Maybe (Int, [Form])
concLits k (Fa vs f) = do
  let (k', vxs) = varPars k vs
  let vm = L.foldl (\ vm_ (v_, x_) -> HM.insert v_ x_ vm_) HM.empty vxs
  let f' = subf vm f
  concLits k' f'
concLits k (Or fs) = concsLits k fs
concLits k f =
  if isLit f
  then return (k, [f])
  else error "conc lits : not lit"

premsLits :: [Form] -> Maybe [Form]
premsLits [] = return []
premsLits (f : fs) = do
  fs' <- premLits f
  fs'' <- premsLits fs
  return $ fs' ++ fs''

premLits :: Form -> Maybe [Form]
premLits (Fa vs f) = premLits f
premLits (Or fs) = premsLits fs
premLits f = guard (isLit f) >> return [f]

usVar :: Text -> Text
usVar v = "_" <> v

usTerm :: Term -> Term
usTerm (Var v) = Var $ usVar v
usTerm (Fun f xs) = Fun f $ L.map usTerm xs
usTerm x = x

usCla :: Form -> Maybe Form
usCla (Fa vs f) = Fa (L.map usVar vs) <$> usCla f
usCla (Or fs) = Or <$> mapM usCla fs
usCla (Not f) = Not <$> usAtom f
usCla f = usAtom f

usAtom :: Form -> Maybe Form
usAtom (Eq x y)   = return $ Eq (usTerm x) (usTerm y)
usAtom (Rel r xs) = return $ Rel r $ L.map usTerm xs
usAtom _ = nt

isUnderscored :: Text -> Bool
isUnderscored t =
  case T.uncons t of
    Just (c, _) -> c == '_'
    _ -> False

gndt :: Term -> Term
gndt (Var _) = zt
gndt (Fun f xs) = Fun f $ L.map gndt xs
gndt x = x

claVars :: Form -> IO (Set Text)
claVars (Fa vs f) = S.union (S.fromList vs) <$> claVars f
claVars (Or fs) = S.unions <$> mapM claVars fs
claVars f = guard (isLit f) >> return S.empty

formVars :: Form -> Set Text
formVars (Fa vs f) = S.union (S.fromList vs) $ formVars f
formVars (Ex vs f) = S.union (S.fromList vs) $ formVars f
formVars (Imp f g) = S.union (formVars f) (formVars g)
formVars (Iff f g) = S.union (formVars f) (formVars g)
formVars (Not f) = formVars f
formVars (Or fs) = S.unions $ L.map formVars fs
formVars (And fs) = S.unions $ L.map formVars fs
formVars _ = S.empty

gndVar :: VM -> Text -> VM
gndVar vm v =
  case HM.lookup v vm of
    Just _ -> vm
    _ -> HM.insert v zt vm

efactor :: Maybe Bool -> Form -> Form -> IO Prf
efactor mb f g = do
  -- pt "Eq-Factor :\n"
  -- pt $ "f : " <> ppForm f <> "\n"
  -- pt $ "g : " <> ppForm g <> "\n"
  fls <- cast $ premLits f
  (_, gls, pp) <- useConc 0 g
  -- (_, gls) <- cast $ concLits 0 g
  -- pt $ "f-lits : " <> ppList ppForm fls <> "\n"
  -- pt $ "g-lits : " <> ppList ppForm gls <> "\n"
  avs <- claVars f
  _vm <- efctr gls (HM.empty, mb, fls)
  let vm = L.foldl gndVar (HM.map gndt _vm) avs
  -- pt "Eq-Factor success, results :\n"
  -- pt $ "VM =\n" <> ppVM vm <> "\n"
  pf <- usePrem (mapPremLit gls) vm f
  return $ pp pf

type EFS = (VM, Maybe Bool, [Form])
type FSTA = (VM, [Form])

fContained :: [Form] -> (Term, Term, Dir, SST) -> Bool
fContained hs (_, y, _, _) =
  let hfs = fsfuns hs in
  let yfs = tfuns y in
  S.isSubsetOf yfs hfs

genSST :: Dir -> [Form] -> [Form] -> (Form, [Form]) -> [(Term, Term, Dir, SST)]
genSST dr hs gs (Eq x y, fs) =
  let fs' = L.filter (\ l_ -> not (litOccursIn l_ hs)) fs in
  let gs' = L.filter (\ l_ -> not (litOccursIn l_ hs)) gs in
  [(x, y, dr, (HM.empty, fs', gs', [])), (y, x, dr, (HM.empty, fs', gs', []))]
genSST dr hs gs _ = []

genZs :: Dir -> [Form] -> [Form] -> [Form] -> [(Term, Term, Dir, SST)]
genZs dr fs gs hs =
  case unspecs fs hs of
    (_ : _ : _) -> []
    [ffs] -> L.filter (fContained hs) $ genSST dr hs gs ffs
    [] -> L.concatMap (genSST dr hs gs) (plucks fs)

lspec :: Form -> Form -> Bool
lspec (Not f) (Not g) = aspec f g
lspec f g = aspec f g

aspec :: Form -> Form -> Bool
aspec (Rel r xs) (Rel s ys) = r == s && isJust (zipM xs ys >>= specs HM.empty)
aspec (Eq x y) (Eq a b) = isJust $ specs HM.empty [(x, a), (y, b)] <|> specs HM.empty [(x, b), (y, a)]
aspec _ _ = False

unspecs :: [Form] -> [Form] -> [(Form, [Form])]
unspecs fls hls = L.filter (\ (f_, _) -> not (L.any (lspec f_) hls)) (plucks fls)

cutIff :: Form -> Form -> Prf -> Prf -> Prf
cutIff f g pfg p = Cut (f <=> g) pfg $ Cut g (iffMP f g) p

resolve :: Form -> Form -> Form -> IO Prf
resolve f g h = do
  ([f', g'], _) <- cast $ rnfs HM.empty HM.empty [f, g]
  fls <- cast $ premLits f'
  gls <- cast $ premLits g'
  -- (_, hls) <- cast $ concLits 0 h
  (_, hls, ph) <- useConc 0 h
  (_vm, _p, dr) <- rslv (HM.empty, nt, fls, gls, hls)
  fvs <- claVars f'
  gvs <- claVars g'
  let avs = S.toList (S.union fvs gvs)
  let vm = L.foldl gndVar (HM.map gndt _vm) avs
  let pvt =  subf vm _p
  prff' <- prn 0 f f'
  prfg' <- prn 0 g g'
  case dr of
    Obv -> do
      pf <- usePrem (mapPremLit (pvt : hls)) vm f'
      pg <- usePrem (mapPremLit (Not pvt : hls)) vm g'
      return $ cutIff f f' prff' $ cutIff g g' prfg' $ ph $ Cut pvt pf $ Cut (Not pvt) pg (NotL pvt $ Ax pvt)
    Rev -> do
      pf <- usePrem (mapPremLit (Not pvt : hls)) vm f'
      pg <- usePrem (mapPremLit (pvt : hls)) vm g'
      return $ cutIff f f' prff' $ cutIff g g' prfg' $ ph $ Cut pvt pg $ Cut (Not pvt) pf (NotL pvt $ Ax pvt)

eqfInit :: (Form, [Form]) -> [(Term, Term, [Form])]
eqfInit (Not (Eq x y), fs) = [(x, y, fs), (y, x, fs)]
eqfInit _ = []

eqfactor :: Form -> Form -> IO Prf
eqfactor f g = do
  fs <- cast $ premLits f
  -- (_, ggs) <- cast $ concLits 0 g
  (_, ggs, pg) <- useConc 0 g
  let xygs = L.concatMap eqfInit $ plucks ggs
  (vm, x, y, gs) <- first (\ (x_, y_, gs_) -> eqf x_ y_ gs_ (HM.empty, fs, [])) xygs

  pf <- usePrem (\f_ -> first (rwf x y f_) gs) vm f
  let eqpf
        | Not (x === y) `elem` ggs = NotR (x === y) pf
        | Not (y === x) `elem` ggs = NotR (y === x) $ Cut (x === y) (EqS y x) pf
        | otherwise = error "eqf : nonexistent eqn"
  return $ pg eqpf

superpose :: Form -> Form -> Form -> IO Prf
superpose f g h = do
  -- pt $ "f : " <> ppForm f <> "\n"
  -- pt $ "g : " <> ppForm g <> "\n"
  -- pt $ "h : " <> ppForm h <> "\n"
  ([f', g'], _) <- cast $ rnfs HM.empty HM.empty [f, g]
  -- pt $ "f' : " <> ppForm f' <> "\n"
  -- pt $ "g' : " <> ppForm g' <> "\n"
  fls <- cast $ premLits f'
  gls <- cast $ premLits g'
  (_, hls, ph) <- useConc 0 h
  -- pt $ "f-lits :\n" <> ppListNl ppForm fls <> "\n"
  -- pt $ "g-lits :\n" <> ppListNl ppForm gls <> "\n"
  -- pt $ "h-lits :\n" <> ppListNl ppForm hls <> "\n"
  let zos = genZs Obv fls gls hls
  let zrs = genZs Rev gls fls hls
  let zs = zos ++ zrs
  (x, y, dr, vm) <- first (\ (x_, y_, dr_, z_) -> super x_ y_ dr_ hls z_) zs
  prff' <- prn 0 f f'
  prfg' <- prn 0 g g'
  pfg <- case dr of
           Obv -> suprf vm x y f' g' hls
           Rev -> suprf vm x y g' f' hls
  return $ cutIff f f' prff' $ cutIff g g' prfg' $ ph pfg

rwt :: Term -> Term -> Term -> Term -> IO Prf
rwt x y a@(Fun f xs) b@(Fun g ys)
  | a == b = return $ EqR a
  | x == a && y == b = return $ Ax (x === y)
  | otherwise = do
    guard $ f == g
    xyps <- mapM2 (\ x_ y_ -> (x_, y_,) <$> rwt x y x_ y_) xs ys
    return $ FunC f xyps
rwt x y a b
  | a == b = return $ EqR a
  | x == a && y == b = return $ Ax (x === y)
  | otherwise = mzero
rwf :: Term -> Term -> Form -> Form -> IO Prf
rwf x y (Not f) (Not g) = do
   p <- rwf y x g f
   return $ Cut (y === x) (EqS x y) $ notLR f g p
rwf x y (Rel r xs) (Rel s ys) = do
  guard $ r == s
  xyps <- mapM2 (\ x_ y_ -> (x_, y_,) <$> rwt x y x_ y_) xs ys
  return $ RelC r xyps
rwf x y (Eq a b) (Eq c d) =
  ( do pac <- rwt x y a c
       pbd <- rwt x y b d
       return $ EqC (a, c, pac) (b, d, pbd) ) <|>
  ( do pad <- rwt x y a d
       pbc <- rwt x y b c
       return $ Cut (d === c) (EqC (a, d, pad) (b, c, pbc)) (EqS d c) )
rwf _ _ _ _ = mzero

useEqPremLit :: Term -> Term -> Prf -> Form -> [Form] -> IO Prf
useEqPremLit x y p f@(Eq a b) hs
  | (x == a) && (y == b) = return p
  | (x == b) && (y == a) = return $ Cut (x === y) (EqS a b) p
  | otherwise = mapPremLit hs f
useEqPremLit x y p f hs = mapPremLit hs f

suprf :: VM -> Term -> Term -> Form -> Form -> [Form] -> IO Prf
suprf vm x y f g hs = do
  -- pt "VM =\n"
  -- pt $ ppVM vm
  -- pt $ "x = " <> ppTerm x <> "\n"
  -- pt $ "y = " <> ppTerm y <> "\n"
  -- pt $ "g = " <> ppForm g <> "\n"
  -- pt $ "hs =\n" <> ppListNl ppForm hs <> "\n"
  pp <- usePrem (\g_ -> first (rwf x y g_) hs) vm g
  usePrem (flip (useEqPremLit x y pp) hs) vm f

proveCnfTrans :: VM -> Form -> [Form] -> IO Prf
proveCnfTrans vm (Fa vs f) gs = do
  let vxs = L.map (\ v_ -> (v_, agvmt vm (Var v_))) vs
  let f' = substForm vxs f
  FaL vxs f <$> proveCnfTrans vm f' gs
proveCnfTrans vm (And fs) gs =
  first (\ f_ -> AndL fs [f_] <$> proveCnfTrans vm f_ gs) fs
proveCnfTrans vm (Or fs) gs = do
  fps <- mapM (\ f_ -> (f_,) <$> proveCnfTrans vm f_ gs) fs
  return $ OrL fps
proveCnfTrans _ (Not (Eq x y)) gs
  | Not (Eq y x) `elem` gs = return $ notLR (x === y) (y === x) $ EqS y x
proveCnfTrans _ (Eq x y) gs
  | Eq y x `elem` gs = return $ EqS x y
proveCnfTrans _ f gs
  | f `elem` gs = return $ Ax f
  | otherwise = mzero

usePrem :: (Form -> IO Prf) -> VM -> Form -> IO Prf
usePrem  pf vm (Fa vs f) = do
  let vxs = L.map (\ v_ -> (v_, agvmt vm (Var v_))) vs
  let f' = substForm vxs f
  FaL vxs f <$> usePrem pf vm f'
usePrem pf vm (Or fs) = do
  fps <- mapM (\ f_ -> (f_ ,) <$> usePrem pf vm f_) fs
  return $ OrL fps
usePrem  pf _ f = pf f

mapPremLit :: [Form] -> Form -> IO Prf
mapPremLit hs f@(Not (Rel _ _)) = guard (f `elem` hs) >> return (Ax f)
mapPremLit hs f@(Rel _ _) = guard (f `elem` hs) >> return (Ax f)
mapPremLit hs (Eq x y)
  | Eq x y `elem` hs = return $ Ax (Eq x y)
  | Eq y x `elem` hs = return $ EqS x y
  | otherwise = error "use-prem : eq"
mapPremLit hs f@(Not (Eq x y))
  | f `elem` hs = return $ Ax f
  | x == y = return $ NotL (x === x) $ EqR x
  | Not (Eq y x) `elem` hs = return $ NotL (x === y) $ NotR (y === x) $ EqS y x
  | otherwise = et $ "use-prem-neq : " <> ppForm f
mapPremLit _ f = et $ "map-prem-lit-exception : " <> ppForm f <> "\n"

subt :: VM -> Term -> Term
subt vm (Var v) =
  case HM.lookup v vm of
    Just x -> x
    _ -> Var v
subt vm (Fun f xs) = Fun f $ L.map (subt vm) xs
subt vm x = x

subf ::  VM -> Form -> Form
subf vm (Eq x y) =
  let x' = subt vm x in
  let y' = subt vm y in
  Eq x' y'
subf vm (Rel r xs) = Rel r $ L.map (subt vm) xs
subf vm (Not f) = Not $ subf vm f
subf vm (Imp f g) =
  let f' = subf vm f in
  let g' = subf vm g in
  Imp f' g'
subf vm (Iff f g) =
  let f' = subf vm f in
  let g' = subf vm g in
  Iff f' g'
subf vm (Or fs)  = Or  $ L.map (subf vm) fs
subf vm (And fs) = And $ L.map (subf vm) fs
subf vm (Fa vs f) =
  let vm' = HM.filterWithKey (\ v_ _ -> v_ `notElem` vs) vm in
  Fa vs $ subf vm' f
subf vm (Ex vs f) =
  let vm' = HM.filterWithKey (\ v_ _ -> v_ `notElem` vs) vm in
  Ex vs $ subf vm' f

substVar :: Text -> Term -> Term -> Term
substVar v x (Var w) = if v == w then x else Var w
substVar v x (Par m) = Par m
substVar v x (Fun f xs) = Fun f $ L.map (substVar v x) xs

ubv :: VM -> Text -> Term -> Maybe VM
-- scheme  : ubv vm v x
-- assumes : v is unbound in vm
-- assumes : if x is a variable, it is also unbound in vm
ubv vm v x =
  let x' = avmt vm x in
  let vm' = HM.map (substVar v x') vm in
  do guard $ not $ hasVar v x'
     return $ HM.insert v x' vm'

hasVar :: Text -> Term -> Bool
hasVar v (Var w) = v == w
hasVar v (Fun _ xs) = L.any (hasVar v) xs
hasVar _ _ = False

lookUpTerm :: VM -> Term -> Maybe Term
lookUpTerm vm (Var v) = HM.lookup v vm
lookUpTerm vm _ = nt

utrw :: VM -> Term -> Term -> Term -> Term -> Maybe VM
utrw vm x y a@(Fun f xs) b@(Fun g ys) =
  ut vm a b <|>
  ( do vm' <- ut vm x a
       ut vm' y b ) <|>
  ( do xys <- zipM xs ys
       foldM (\ vm_ (a_, b_) -> utrw vm_ x y a_ b_) vm xys )
utrw vm x y a b =
  ut vm a b <|>
  ( do vm' <- ut vm x a
       ut vm' y b )

ut :: VM -> Term -> Term -> Maybe VM
ut vm (Par k) (Par m) = guard (k == m) >> return vm
ut vm (Fun f xs) (Fun g ys) = do
  guard $ f == g
  xys <- zipM xs ys
  foldM (\ vm_ (x_, y_) -> ut vm_ x_ y_) vm xys
ut vm x y =
  if x == y
  then return vm
  else
    case (lookUpTerm vm x, lookUpTerm vm y, x, y) of
       (Just x', _, _, _) -> ut vm x' y
       (_, Just y', _, _) -> ut vm x y'
       (_, _, Var v, _) -> ubv vm v y
       (_, _, _, Var w) -> ubv vm w x
       (_, _, _, _) -> nt

utrws :: VM -> Term -> Term -> [(Term, Term)] -> Maybe VM
utrws vm x y [] = return vm
utrws vm x y ((a, b) : abs) = do
  vm' <- utrw vm x y a b
  utrws vm' x y abs

uts :: VM -> [(Term, Term)] -> Maybe VM
uts vm [] = return vm
uts vm ((x, y) : xys) = do
  vm' <- ut vm x y
  uts vm' xys

ul :: VM -> Form -> Form -> [VM]
ul vm (Not f) (Not g) = ua vm f g
ul vm f g = ua vm f g

ua :: VM -> Form -> Form -> [VM]
ua vm (Eq s t) (Eq u v) =
  case (uts vm [(s, u), (t, v)], uts vm [(s, v), (t, u)]) of
    (Just vm', Just vm'') ->
      case (HM.isSubmapOf vm' vm'', HM.isSubmapOf vm'' vm') of
        (True, _) -> [vm']
        (_, True) -> [vm'']
        _ -> [vm', vm'']
    (Just vm', _) -> [vm']
    (_, Just vm') -> [vm']
    _ -> []
ua vm (Rel r xs) (Rel s ys) =
  if r == s
  then cast $ zipM xs ys >>= uts vm
  else []
ua vm _ _ = []

type RSTA = (VM, Maybe (Form, Dir), [Form], [Form], [Form])

type SST = (VM, [Form], [Form], [(Term, Term)])

shorter :: [a] -> [b] -> Bool
shorter _ [] = False
shorter [] _ = True
shorter (_ : xs) (_ : ys) = shorter xs ys

getShortList :: [[a]] -> Maybe [a]
getShortList [] = nt
getShortList [xs] = return xs
getShortList ([] : xss) = return []
getShortList ([x] : xss) = return [x]
getShortList (xs : xss) = do
  ys <- getShortList xss
  if shorter xs ys
  then return xs
  else return ys

genPvtAux :: VM -> Form -> Form -> [(VM, Form, Dir)]
genPvtAux vm (Not _) (Not _) = []
genPvtAux vm f (Not g) = L.map (, f, Obv) $ ua vm f g
genPvtAux vm (Not f) g = L.map (, f, Rev) $ ua vm f g
genPvtAux _ _ _ = []

genPvt :: VM -> (Form, [Form]) -> (Form, [Form]) -> [Form] -> [RSTA]
genPvt vm (f, fs) (g, gs) hs =
  L.map (\ (vm_, pvt_, dr_) -> (vm_, Just (pvt_, dr_), fs, gs, hs)) $ genPvtAux vm f g

genEF :: VM -> Maybe Bool -> Form -> [Form] -> [EFS]
genEF vm mb (Not (Eq x y)) fs =
  case ut vm x y of
    Just vm' -> [(vm', if isJust mb then Just True else nt, fs)]
    _ -> []
genEF _ _ _ _ = []

genFcs :: VM -> [Form] -> (Form, [Form]) -> [FSTA]
genFcs vm gs (f, fs) = L.map (, fs) (L.concatMap (ul vm f) gs)

genEFSs :: VM -> Maybe Bool -> [Form] -> (Form, [Form]) -> [EFS]
genEFSs vm (Just True) gs (f, fs) = L.map (, Just True, fs) (L.concatMap (ul vm f) gs)
genEFSs vm mb gs (f, fs) =
  genEF vm mb f fs ++ L.map (, mb, fs) (L.concatMap (ul vm f) gs)

genLftRss :: VM -> Maybe (Form, Dir) -> [Form] -> [Form] -> (Form, [Form]) -> [RSTA]
genLftRss vm Nothing gs hs (f, fs) =
  L.concatMap (\ (g_, gs_) -> genPvt vm (f, fs) (g_, gs_) hs) (plucks gs) ++ L.map (, nt, fs, gs, hs) (L.concatMap (ul vm f) hs)
genLftRss vm pd@(Just (pvt, Obv)) gs hs (f, fs) = L.map (, pd, fs, gs, hs) $ L.concatMap (ul vm f) (pvt : hs)
genLftRss vm pd@(Just (pvt, Rev)) gs hs (f, fs) = L.map (, pd, fs, gs, hs) $ L.concatMap (ul vm f) (Not pvt : hs)

genRgtRss :: VM -> Maybe (Form, Dir) -> [Form] -> [Form] -> (Form, [Form]) -> [RSTA]
genRgtRss vm Nothing fs hs (g, gs) =
  L.concatMap (\ (f_, fs_) -> genPvt vm (f_, fs_) (g, gs) hs) (plucks fs) ++ L.map (, nt, fs, gs, hs) (L.concatMap (ul vm g) hs)
genRgtRss vm pd@(Just (pvt, Obv)) fs hs (g, gs) = L.map (, pd, fs, gs, hs) $ L.concatMap (ul vm g) (Not pvt : hs)
genRgtRss vm pd@(Just (pvt, Rev)) fs hs (g, gs) = L.map (, pd, fs, gs, hs) $ L.concatMap (ul vm g) (pvt : hs)

litOccursIn :: Form -> [Form] -> Bool
litOccursIn (Not (Eq x y)) hs = Not (Eq x y) `elem` hs || Not (Eq y x) `elem` hs
litOccursIn (Eq x y) hs = Eq x y `elem` hs || Eq y x `elem` hs
litOccursIn f@(Not (Rel _ _)) hs = f `elem` hs
litOccursIn f@(Rel _ _) hs = f `elem` hs
litOccursIn _ _ = False

-- atomOccursIn :: Form -> [Form] -> Bool
-- atomOccursIn (Eq x y) hs = (x === y) `elem` hs || (y === x) `elem` hs
-- atomOccursIn f@(Rel _ _) hs = f `elem` hs
-- atomOccursIn _ _ = False

tryGndLit :: VM -> Form -> Maybe Form
tryGndLit vm f = do
  f' <- tavmf vm f
  guard $ isGndLit f'
  return f'

superatv :: VM -> [Form] -> [Form] -> [(Term, Term)] -> (Form, [Form]) -> [SST]
superatv vm hs gs xys (f, fs) =
  case tryGndLit vm f of
    Just f' ->
      if litOccursIn f' hs
      then [(vm, fs, gs, xys)]
      else []
    _ -> L.map (, fs, gs, xys) $ L.concatMap (ul vm f) hs

superpsv :: VM -> Term -> Term -> [Form] -> [Form] -> [(Term, Term)] -> (Form, [Form]) -> [SST]
superpsv vm x y hs fs xys (g, gs) = L.map (\ xys_ ->(vm, fs, gs, xys_ ++ xys)) $ L.concatMap (genRWEqs g) hs

supereq :: VM -> Term -> Term -> [Form] -> [Form] -> [Form] -> ((Term, Term), [(Term, Term)]) -> [SST]
supereq vm x y hs fs gs ((Fun f xs, Fun g ys), xys) =
  let lft = case ut vm (Fun f xs) (Fun g ys) of
              Just vm' -> [(vm', fs, gs, xys)]
              _ -> [] in
  let mid = case uts vm [(x, Fun f xs), (y, Fun g ys)] of
              Just vm' -> [(vm', fs, gs, xys)]
              _ -> [] in
  let rgt = case (f == g, zipM xs ys) of
              (True, Just abs) -> [(vm, fs, gs, abs ++ xys)]
              _ -> [] in
  lft ++ mid ++ rgt
supereq vm x y hs fs gs ((a, b), xys) =
  let lft = case ut vm a b of
              Just vm' -> [(vm', fs, gs, xys)]
              _ -> [] in
  let rgt = case uts vm [(x, a), (y, b)] of
              Just vm' -> [(vm', fs, gs, xys)]
              _ -> [] in
  lft ++ rgt

genRWEqs :: Form -> Form -> [[(Term, Term)]]
genRWEqs (Not g) (Not h) = genRWEqs g h
genRWEqs (Rel r xs) (Rel s ys) =
  case (r == s, zipM xs ys) of
    (True, Just xys) -> [xys]
    _ -> []
genRWEqs (Eq x y) (Eq a b) = [[(x, a), (y, b)], [(x, b), (y, a)]]
genRWEqs _ _ = []

type EQFS = (VM, [Form], [(Term, Term)])
eqfbranch :: Term -> Term -> [Form] -> EQFS -> [[EQFS]]
eqfbranch x y gs (vm, fs, xys) =
  let fbs = L.map (eqfLit vm gs xys) (plucks fs) in
  let ebs = L.map (eqfEq vm x y fs) (plucks xys) in
  fbs ++ ebs

eqfLit :: VM -> [Form] -> [(Term, Term)] -> (Form, [Form]) -> [EQFS]
eqfLit vm gs xys (f, fs) = L.map (\ xys_ ->(vm, fs, xys_ ++ xys)) $ L.concatMap (genRWEqs f) gs

eqfEq :: VM -> Term -> Term -> [Form] -> ((Term, Term), [(Term, Term)]) -> [EQFS]
eqfEq vm x y fs ((Fun f xs, Fun g ys), xys) =
  let lft = case ut vm (Fun f xs) (Fun g ys) of
              Just vm' -> [(vm', fs, xys)]
              _ -> [] in
  let mid = case uts vm [(x, Fun f xs), (y, Fun g ys)] of
              Just vm' -> [(vm', fs, xys)]
              _ -> [] in
  let rgt = case (f == g, zipM xs ys) of
              (True, Just abs) -> [(vm, fs, abs ++ xys)]
              _ -> [] in
  lft ++ mid ++ rgt
eqfEq vm x y fs ((a, b), xys) =
  let lft = case ut vm a b of
              Just vm' -> [(vm', fs, xys)]
              _ -> [] in
  let rgt = case uts vm [(x, a), (y, b)] of
              Just vm' -> [(vm', fs, xys)]
              _ -> [] in
  lft ++ rgt

superbranch :: Term -> Term -> [Form] -> SST -> [[SST]]
superbranch x y hs (vm, fs, gs, xys) =
  let abs = L.map (superatv vm hs gs xys) (plucks fs) in
  let pbs = L.map (superpsv vm x y hs fs xys) (plucks gs) in
  let ebs = L.map (supereq vm x y hs fs gs) (plucks xys) in
  abs ++ pbs ++ ebs

genRss :: RSTA -> [[RSTA]]
genRss (vm, pd, fs, gs, hs) =
  let lft = L.map (genLftRss vm pd gs hs) (plucks fs) in
  let rgt = L.map (genRgtRss vm pd fs hs) (plucks gs) in
  lft ++ rgt

type Pivot = Maybe (Form, Dir)

ppDir :: Dir -> Text
ppDir Obv = "====>"
ppDir Rev = "<===="

ppPivot :: Pivot -> Text
ppPivot (Just (p, d)) = ppForm p <> " | " <> ppDir d
ppPivot _ = "unknown"

ppRsta :: RSTA -> Text
ppRsta (vm, pvt, fs, gs, hs) =
  "VM =\n" <> ppVM vm <>
  "\nPivot = " <> ppPivot pvt <>
  "\nFs = " <> ppForms fs <>
  "\nGs = " <> ppForms gs <>
  "\nHs = " <> ppForms hs

fctr :: [Form] -> FSTA -> IO VM
fctr _ (vm, []) = return vm
fctr gs (vm, fs) = do
  let fcss = L.map (genFcs vm gs) (plucks fs)
  fcs <- cast $ getShortList fcss
  first (fctr gs) fcs

efctr :: [Form] -> EFS -> IO VM
efctr _ (vm, _, []) = return vm
efctr gs (vm, mb, fs) = do
  let fcss = L.map (genEFSs vm mb gs) (plucks fs)
  fcs <- cast $ getShortList fcss
  first (efctr gs) fcs

mapSearchBranch :: VM -> [Form] -> [Form] -> [[(VM, [Form])]]
mapSearchBranch vm fs gs =
  L.map (\ (f_, fs_) -> L.concatMap (L.map (, fs_) . ul vm f_) gs) (plucks fs)

mapSearch :: VM -> [Form] -> [Form] -> IO VM
mapSearch vm [] _ = return vm
mapSearch vm fs gs = do
  let zss = mapSearchBranch vm fs gs
  zs <- cast $ getShortList zss
  first (\ (vm_, fs_) -> mapSearch vm_ fs_ gs) zs

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

searchOrigMB :: VC -> [(Form, Form)] -> Maybe VC
searchOrigMB vc [] = return vc
searchOrigMB vc fgs = do
  let zss = L.map (forkOrigAux vc) (plucks fgs)
  zs <- cast $ getShortList zss
  first (uncurry searchOrigMB) zs

searchOrig :: Int -> VC -> [(Form, Form)] -> IO VC
searchOrig dt vc [] = return vc
searchOrig dt vc fgs = do
  -- M.when (dt `rem` 1000 == 0) $ pt $ "Search depth = " <> ppInt dt <> "\n"
  -- pt "\n==============================================================\n" 
  -- pt $ "Search depth = " <> ppInt dt <> "\n"
  -- pt $ ppSearchState vc fgs
  let zss = L.map (forkOrigAux vc) (plucks fgs)
  zs <- cast $ getShortList zss
  first (uncurry $ searchOrig (dt + 1)) zs

-- firstWithMsg :: (a -> IO b) -> [a] -> IO b
-- firstWithMsg f [] = mzero
-- firstWithMsg f (x : xs) =
--   f x <|> do
--     pt "Trial failed, backtracking...\n"
--     firstWithMsg f xs

search :: (Eq a, Show a) => (a -> Bool) -> (a -> [[a]]) -> a -> Maybe a
search tm br x =
  if tm x
  then return x
  else do let zss = br x
          -- guard $ [] `notElem` zss
          zs <- cast $ getShortList zss
          first (search tm br) zs

super :: Term -> Term -> Dir -> [Form] -> SST -> IO (Term, Term, Dir, VM)
super x y dr hs (vm , [], [], []) = do
  let x' = agvmt vm x
  let y' = agvmt vm y
  return (x', y', dr, vm)
super x y dr hs z = do
  let zss = superbranch x y hs z
  zs <- cast $ getShortList zss
  first (super x y dr hs) zs

eqf :: Term -> Term -> [Form] -> (VM, [Form], [(Term, Term)]) -> IO (VM, Term, Term, [Form])
eqf x y gs (vm, [], []) = return (vm, x, y, gs)
eqf x y gs z = do
  let zss = eqfbranch x y gs z
  zs <- cast $ getShortList zss
  first (eqf x y gs) zs

rslv :: RSTA -> IO (VM, Form, Dir)
rslv (vm, Just (pvt, dr), [], [], _) = return (vm, pvt, dr)
rslv r = do
  let rss = genRss r
  rs <- cast $ getShortList rss
  first rslv rs

bvsOccurTerm :: [Text] -> Term -> Bool
bvsOccurTerm vs (Var v) = v `elem` vs
bvsOccurTerm vs (Fun f xs) = L.any (bvsOccurTerm vs) xs
bvsOccurTerm vs _ = False

bvsOccurForm :: [Text] -> Form -> Bool
bvsOccurForm vs (Eq x y) = bvsOccurTerm vs x || bvsOccurTerm vs y
bvsOccurForm vs (Rel _ xs) = L.any (bvsOccurTerm vs) xs
bvsOccurForm vs (Not f) = bvsOccurForm vs f
bvsOccurForm vs (And fs) = L.any (bvsOccurForm vs) fs
bvsOccurForm vs (Or fs)  = L.any (bvsOccurForm vs) fs
bvsOccurForm vs (Imp f g) = bvsOccurForm vs f || bvsOccurForm vs g
bvsOccurForm vs (Iff f g) = bvsOccurForm vs f || bvsOccurForm vs g
bvsOccurForm vs (Fa ws f) = bvsOccurForm (vs L.\\ ws) f
bvsOccurForm vs (Ex ws f) = bvsOccurForm (vs L.\\ ws) f

varInt :: Text -> Term -> Bool
varInt v (Var w) = v == w
varInt v (Fun f xs) = L.any (varInt v) xs
varInt v _ = False

varInf :: Text -> Form -> Bool
varInf v (Eq x y) = varInt v x || varInt v y
varInf v (Rel _ xs) = L.any (varInt v) xs
varInf v (Not f) = varInf v f
varInf v (And fs) = L.any (varInf v) fs
varInf v (Or fs)  = L.any (varInf v) fs
varInf v (Imp f g) = varInf v f || varInf v g
varInf v (Iff f g) = varInf v f || varInf v g
varInf v (Fa ws f) = v `notElem` ws && varInf v f
varInf v (Ex ws f) = v `notElem` ws && varInf v f

pointLessQuant :: Form -> Bool
pointLessQuant (Fa vs f) = not $ bvsOccurForm vs f
pointLessQuant (Ex vs f) = not $ bvsOccurForm vs f
pointLessQuant _ = False

type PSS = (VC, [(Form, Form)])

isSingleton :: Set a -> Bool
isSingleton s =
  case S.toList s of
    [_] -> True
    _ -> False

stepOrigFsGs :: Maybe [(Form, [Form], Form, [Form])] -> VC -> [Form] -> [Form] -> [Form] -> [(Form, [Form], Form, [Form])]
stepOrigFsGs Nothing   vc _ [] _ = []
stepOrigFsGs (Just tp) vc _ [] _ = tp
stepOrigFsGs mtps      vc sf (f : fs) ggs =
  case stepOrigFGs vc f ggs of
    [] -> []
    [(g, gs)] -> [(f, sf ++ fs, g, gs)]
    ggss ->
      let ntps = Just $ L.map (\ (g_, gs_) -> (f, sf ++ fs, g_, gs_)) ggss in
      case mtps of
        Nothing -> stepOrigFsGs ntps vc (f : sf) fs ggs
        Just hhss ->
          if shorter ggss hhss
          then stepOrigFsGs ntps vc (f : sf) fs ggs
          else stepOrigFsGs mtps vc (f : sf) fs ggs

ttx :: Text -> a -> a
ttx t = trace (unpack t)

stepOrigFGs :: VC -> Form -> [Form] -> [(Form, [Form])]
stepOrigFGs vc f ggs =
  let tuples = MB.mapMaybe (\ (g_, gs_) -> (, (g_, gs_)) <$> searchOrigMB vc [(f, g_)]) (plucks ggs) in
  let results = representatives (\ (vc0, _) (vc1, _) -> vc0 == vc1) tuples in
  L.map snd results

representatives :: (a -> a -> Bool) -> [a] -> [a]
representatives _ [] = []
representatives eqc (x : xs) =
  let xs' = L.filter (not . eqc x) xs in
  x : representatives eqc xs'

forkOrigAux :: VC -> ((Form, Form), [(Form, Form)]) -> [PSS]
forkOrigAux vc (fg, fgs) = L.map (DBF.second (++ fgs)) $ forkOrig vc fg

forkOrig :: VC -> (Form, Form) -> [PSS]
forkOrig vc (Rel r xs, Rel s ys)
  | r == s = do
    vc' <- cast $ conTerms vc xs ys
    return (vc', [])
forkOrig vc (Eq x y, Eq a b) =
  case (conTerms vc [x, y] [a, b], conTerms vc [x, y] [b, a]) of
    (Just vc', Nothing) -> [(vc', [])]
    (Nothing, Just vc') -> [(vc', [])]
    (Just vc', Just vc'') ->
      if vc' == vc''
      then [(vc', [])]
      else [(vc', []), (vc'', [])]
    _ -> []
forkOrig vc (Not f, Not g) = [(vc, [(f, g)])]
forkOrig vc (Imp e f, Imp g h) = [(vc, [(e, g), (f, h)])]
forkOrig vc (Iff e f, Iff g h) = [(vc, [(e, g), (f, h)])]
forkOrig vc (Fa vs f, Fa ws g) = do
  vc' <- cast $ conVars vs ws vc
  return (vc', [(f, g)])
forkOrig vc (Ex vs f, Ex ws g) = do
  vc' <- cast $ conVars vs ws vc
  return (vc', [(f, g)])
forkOrig vr (Or [], Or []) = [(vr, [])]
forkOrig vr (And [], And []) = [(vr, [])]
forkOrig vc (Or fs, Or gs) =
  L.map (\ (f_, fs_, g_, gs_) -> (vc, [(f_, g_), (Or fs_, Or gs_)])) $ stepOrigFsGs nt vc [] fs gs
forkOrig vc (And fs, And gs) =
  L.map (\ (f_, fs_, g_, gs_) -> (vc, [(f_, g_), (And fs_, And gs_)])) $ stepOrigFsGs nt vc [] fs gs
forkOrig vc _ = []

searchCnf :: [Form] -> (VM, [Form]) -> Maybe VM
searchCnf gls z = fst <$> search (L.null . snd) (\ (vm_, fs_) -> L.map (forkCnf vm_ gls) (plucks fs_)) z

forkCnf :: VM -> [Form] -> (Form, [Form]) -> [(VM, [Form])]
forkCnf vm gls (Or fs, hs) = [(vm, fs ++ hs)]
forkCnf vm gls (Fa _ f, hs) = [(vm, f : hs)]
forkCnf vm gls (And fs, hs) = L.map (\ f_ -> (vm, f_ : hs)) fs
forkCnf vm gs (f, hs) = L.map (, hs) $ L.concatMap (ul vm f) gs

appVrTerm :: VR -> Term -> Term
appVrTerm vr (Var v) =
  case HM.lookup v (fst vr) of
    Just x -> Var x
    _ -> et "appVrTerm : no mapping"
appVrTerm vw (Fun f xs) = Fun f $ L.map (appVrTerm vw) xs
appVrTerm vw x = x

appVrForm :: VR -> Form -> Form
appVrForm vr (Not f) = Not $ appVrForm vr f
appVrForm vr (Fa vs f) = Fa (L.map (\ v_ -> HM.findWithDefault "_" v_ (fst vr)) vs) $ appVrForm vr f
appVrForm vr (Ex vs f) = Ex (L.map (\ v_ -> HM.findWithDefault "_" v_ (fst vr)) vs) $ appVrForm vr f
appVrForm vr (Imp f g) = Imp (appVrForm vr f) (appVrForm vr g)
appVrForm vr (Iff f g) = Iff (appVrForm vr f) (appVrForm vr g)
appVrForm vr (Or fs) = Or $ L.map (appVrForm vr) fs
appVrForm vr (And fs) = And $ L.map (appVrForm vr) fs
appVrForm vr (Rel r xs) = Rel r $ L.map (appVrTerm vr) xs
appVrForm vr (Eq x y) = Eq (appVrTerm vr x) (appVrTerm vr y)

delVacVars :: Form -> Form
delVacVars (Not f) = Not $ delVacVars f
delVacVars (Fa vs f) =
  case L.filter (`varInf` f) vs of
    [] -> delVacVars f
    vs' -> Fa vs' $ delVacVars f
delVacVars (Ex vs f) =
  case L.filter (`varInf` f) vs of
    [] -> delVacVars f
    vs' -> Ex vs' $ delVacVars f
delVacVars (Imp f g) = Imp (delVacVars f) (delVacVars g)
delVacVars (Iff f g) = Iff (delVacVars f) (delVacVars g)
delVacVars (Or fs) = Or $ L.map delVacVars fs
delVacVars (And fs) = And $ L.map delVacVars fs
delVacVars f@(Rel _ _) = f
delVacVars f@(Eq _ _) = f

vcToVrAux :: HM.Map Text (Set Text) -> HM.Map Text Text
vcToVrAux vws = do
  let vwss = HM.toList vws
  HM.fromList $ MB.mapMaybe (\ (v_, ws_) -> (v_,) <$> breakSingleton (S.toList ws_)) vwss

vcToVr :: VC -> VR
vcToVr (vws, wvs) = (vcToVrAux vws, vcToVrAux wvs)

rectify :: Form -> Form -> IO Prf
rectify f g = do
  let nf = delVacVars f
  p0 <- pnm 0 f nf
  p1 <- orig nf g 
  return $ cutIff f nf p0 p1

isAtom :: Form -> Bool
isAtom (Rel _ _) = True
isAtom (Eq _ _) = True
isAtom _ = False

isLit :: Form -> Bool
isLit (Not f) = isAtom f
isLit f = isAtom f

ru :: (Monad m) => m ()
ru = return ()

snipDotP :: String -> Maybe String
snipDotP [] = nt
snipDotP [_] = nt
snipDotP ".p" = return ""
snipDotP [_, _] = nt
snipDotP (c : cs) = (c :) <$> snipDotP cs

snipName :: String -> Maybe String
snipName [] = nt
snipName ('/' : cs) =
  case snipName cs of
    Just s -> Just s
    _ -> snipDotP cs
snipName (_ : cs) =
  case snipName cs of
    Just s -> Just s
    _ -> nt

main :: IO ()
main = do
  (tptp : tstp : flags) <- getArgs
  tptp_afs <- parseName tptp
  tstp_afs <- sortAfs <$> parseName tstp
  let hs = L.foldl addHyp (HM.empty, S.empty) tptp_afs
  Prelude.putStr $ tptp ++ "\n"
  if "silent" `elem` flags
    then return ()
    else mapM_ putAnForm tptp_afs
  Prelude.putStr $ tstp ++ "\n"
  if "silent" `elem` flags
    then return ()
    else mapM_ putAnForm tstp_afs
  name <- cast $ snipName tptp
  if tooHard name
  then return ()
  else foldM_ elabIO hs tstp_afs
  Prelude.putStr "Elab complete.\n\n"

tooHard :: String -> Bool
tooHard n = False -- n `elem` ["ALG038+1", "ALG016+1", "HAL004+1", "ALG114+1", "ALG111+1", "ALG121+1"]

  -- Prelude.putStr $ tstp ++ "\n"
  -- tstp_afs <- sortAfs <$> parseName tstp
  -- foldM_ elabIO' HM.empty tstp_afs
  -- Prelude.putStr "Elab complete.\n\n"