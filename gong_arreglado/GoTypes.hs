{-# LANGUAGE MultiParamTypeClasses
           , TemplateHaskell
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , DeriveGeneric, DeriveAnyClass
  #-}

module GoTypes where

import Unbound.LocallyNameless hiding (Generic)

import Control.Applicative
import Control.Arrow ((+++))
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.List as L
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe

import GHC.Generics (Generic)
import Control.DeepSeq


-- DEBUG
import System.IO.Unsafe
import Debug.Trace

data Channel

type ChName = Name Channel


type EqnName = Name GoType


data GoType = Send String ChName GoType
            | Recv String ChName GoType
            | Tau String GoType
            | IChoice String GoType GoType -- Just two things?
            | OChoice String [GoType]
            | Par String [GoType]
            | New String Int (Bind ChName GoType)
            | Null
            | Close String ChName GoType
            | TVar String EqnName
            | ChanInst GoType [ChName] -- P(c)
            | ChanAbst (Bind [ChName] GoType) -- \c.P
            | Seq String [GoType]
            | Buffer ChName (Bool, Int, Int) -- True when Open, Bound, Current
            | ClosedBuffer ChName -- Only used for guard/label
    deriving (Show)


eqGT :: GoType -> GoType -> Bool
eqGT Null Null = True
eqGT (Send _ n1 t1) (Send _ n2 t2) = n1 `aeq` n2 && (eqGT t1 t2)
eqGT (Recv _ n1 t1) (Recv _ n2 t2) = n1 `aeq` n2 && (eqGT t1 t2)
eqGT (Tau _ t1) (Tau _ t2) =  eqGT t1 t2
eqGT (IChoice _ t1 t2) (IChoice _ ta tb) = (eqGT t1 ta) && (eqGT t2 tb)
eqGT (OChoice _ xs) (OChoice _ ys) = L.foldr (\p ac -> ac && (eqGT (fst p) (snd p))) True (L.zip xs ys)
eqGT (Par _ xs) (Par _ ys) = L.foldr (\p ac -> ac && (eqGT (fst p) (snd p))) True (L.zip xs ys)
eqGT (New _ _ t1) (New _ _ t2) = let (def1, main1) = runFreshM (unbind t1) in let (def2, main2) = runFreshM (unbind t2) in main1 `eqGT` main2
eqGT (Close _ name t) (Close _ name2 t2) = (name `aeq` name2) && (eqGT t t2)
eqGT (TVar _ name1) (TVar _ name2) = name1 `aeq` name2
eqGT (ChanInst t1 names1) (ChanInst t2 names2) = (eqGT t1 t2) && (names1 `aeq` names2)
eqGT (ChanAbst t1) (ChanAbst t2) = let (def1, main1) = runFreshM (unbind t1) in let (def2, main2) = runFreshM (unbind t2) in main1 `eqGT` main2 && def1 `aeq` def2
eqGT (Seq _ xs) (Seq _ ys) = L.foldr (\p ac -> ac && (eqGT (fst p) (snd p))) True (L.zip xs ys)
eqGT t1 t2 = t1 `aeq` t2

addToLine :: GoType -> String -> GoType
addToLine t "" = t
addToLine (Send line n t) newLine = Send (line++"\n"++ (removeFirstNewLine newLine)) n t
addToLine (Recv line n t) newLine = Recv (line++"\n"++(removeFirstNewLine newLine)) n t
addToLine (Tau line t) newLine = Tau (line++"\n"++(removeFirstNewLine newLine)) t
addToLine (IChoice line t1 t2) newLine = IChoice (line++"\n"++(removeFirstNewLine newLine)) t1 t2
addToLine (OChoice line gts) newLine = OChoice (line++"\n"++(removeFirstNewLine newLine)) gts
addToLine (Par line gts) newLine = Par (line++"\n"++(removeFirstNewLine newLine)) gts
--addToLine (New line i b) newLine = New (line++":"++newLine) i b
addToLine (New line i b) newLine = let (def1, main1) = runFreshM (unbind b) in New (line++"\n"++(removeFirstNewLine newLine)) i (bind def1 (addToLine main1 newLine))
addToLine (Close line n t) newLine = Close (line++"\n"++(removeFirstNewLine newLine)) n t
addToLine (TVar line eqn) newLine = TVar (line++"\n"++(removeFirstNewLine newLine)) eqn
addToLine (ChanInst t names) newLine = ChanInst (addToLine t newLine) names
addToLine (ChanAbst b) newLine = let (def1, main1) = runFreshM (unbind b) in ChanAbst (bind def1 (addToLine main1 newLine))
addToLine (Seq line gts) newLine = Seq (line++"\n"++(removeFirstNewLine newLine)) gts
addToLine t newLine = t

getLineFromGT :: GoType -> String
getLineFromGT (Send line _ _) = (checkIfBuffer "SEND on line " line)
getLineFromGT (Recv line _ _) = (checkIfBuffer "RECV on line " line)
getLineFromGT (Tau line _) = if ((getTopOfLineNumberStack line)  /= "") then "TIMED event on line " ++ line else (getRestOfLineNumberStack line)
getLineFromGT (IChoice line _ _) = line
getLineFromGT (OChoice line _) = line
--getLineFromGT (Par line _) = (getTopOfLineNumberStack line) ++"SPWN" ++ getRestOfLineNumberStack line
getLineFromGT (Close line _ _) = "Close operation on line " ++ line
--getLineFromGT (TVar line _) = (getTopOfLineNumberStack line) ++ "CALL" ++ getRestOfLineNumberStack line
--getLineFromGT (Seq line _) = (getTopOfLineNumberStack line) ++ "SEQ" ++ getRestOfLineNumberStack line
getLineFromGT t = ""

checkIfBuffer :: String -> String -> String
checkIfBuffer op h1@('B':'U':'F':'F':'E':'R':restOfHistory) = h1
checkIfBuffer op h2 = op ++ h2

getLineFromSynched :: GoType -> GoType -> String
getLineFromSynched (Send line _ _) t = "SEND on line " ++ (getTopOfLineNumberStack line) ++ "\n\t" ++ replaceNewLines (getLineFromGT t) ++ (getRestOfLineNumberStack line)
getLineFromSynched (Recv line _ _) t = "RECV on line " ++ (getTopOfLineNumberStack line) ++ "\n\t" ++ replaceNewLines (getLineFromGT t) ++ (getRestOfLineNumberStack line)
getLineFromSynched (Tau line _) t = if ((getTopOfLineNumberStack line) /= "") then "TIMED event on line " ++ line else (getRestOfLineNumberStack line)
--getLineFromSynched (Tau line _) _ = "TIMED event on line " ++ (getTopOfLineNumberStack line) ++ "\n\t" ++ replaceNewLines (getLineFromGT t) ++ (getRestOfLineNumberStack line)
getLineFromSynched t1 t2 = getLineFromGT t1

replaceNewLines :: String -> String
replaceNewLines s = foldr (\x rec -> if x == '\n' then "\n\t" ++ rec else [x] ++ rec) "" s
--replaceNewLines "" = ""
--replaceNewLines (s:ss) = if s == '\n' then "\n\t" ++ ss else [s] ++ replaceNewLines ss

getTopOfLineNumberStack :: String -> String
getTopOfLineNumberStack "" = ""
getTopOfLineNumberStack (s:ss) = if s == '\n' then "" else [s] ++ (getTopOfLineNumberStack ss)

getRestOfLineNumberStack :: String -> String
getRestOfLineNumberStack "" = ""
getRestOfLineNumberStack "\n" = ""
getRestOfLineNumberStack (s:ss) = if s == '\n' then "\n" ++ (removeFirstNewLine ss) else getRestOfLineNumberStack ss

removeFirstNewLine :: String -> String
removeFirstNewLine "" = ""
removeFirstNewLine (s:ss) = if s == '\n' then ss else (s:ss)
--
--initTuned :: String -> String
--initTuned "" = ""
--initTuned ss = init ss

isBuffer :: GoType -> Bool
isBuffer (Buffer _ _) = True
isBuffer _ = False

data Eqn = EqnSys (Bind (Rec [(EqnName , Embed GoType)]) GoType)
    deriving (Show)
-- inner Proc will always be ChanAbst
             
$(derive [''Channel,''GoType,''Eqn])

--instance Alpha Channel
instance Alpha GoType
instance Alpha Eqn


-- -- PARALLEL STUFF
-- instance NFData GoType where rnf x = seq x ()
-- instance NFData Eqn where rnf x = seq x ()
-- instance NFData (Name a) where rnf x = seq x ()

instance Subst GoType Eqn
--instance Subst String GoType
--instance Subst String Eqn

instance Subst GoType GoType where
  isvar (TVar _ x) = Just (SubstName x)
  isvar _ = Nothing

type M a = FreshM a


-- Free name/var wrappers --
fnTyp :: GoType -> [ChName]
fnTyp t = fv t

fvTyp :: GoType -> [EqnName]
fvTyp t = fv t

fnEqn :: Eqn -> [ChName]
fnEqn e = fv e

fvEqn :: Eqn -> [EqnName]
fvEqn e = fv e


-- GoType Combinators (TVars, New, Chan Abs and Inst) --
tvar :: String -> GoType
tvar = (TVar "t") . s2n

new :: Int -> String -> GoType -> GoType
new i s t = New "" i $ bind (s2n s) t

chanAbst :: String -> GoType -> GoType
chanAbst s t = ChanAbst $ bind ([s2n s]) t

chanAbstL :: [String] -> GoType -> GoType
chanAbstL l t = ChanAbst $ bind (L.map s2n l) t

chanInst :: String -> String -> GoType
chanInst s c = ChanInst (tvar s) ([s2n c])

chanInstL :: String -> [String] -> GoType
chanInstL s l = ChanInst (tvar s) (L.map s2n l)

------------------------------

-- Equation System Combinators --

eqn' :: String -> GoType -> GoType -> Eqn
eqn' s t1 t2 = EqnSys (bind (rec [(s2n s , Embed(t1) )]) t2)

eqn :: String -> String -> GoType -> GoType -> Eqn
eqn s c t1 t2 = eqn' s (chanAbst c t1) t2



eqnl :: [(String,[String],GoType)] -> GoType -> Eqn
eqnl l t = EqnSys (bind (rec (L.map (\(var,plist,def) -> 
                     (s2n var , Embed(chanAbstL plist def))
                     ) l)) t) 

----------------------------------------

-- Structural Congruence --

-- Flatten out Pars in Par (i.e. T | (S | R) == (T | S) | R)--
flattenPar :: GoType -> GoType
flattenPar (Par line l) = Par line (flattenPar' l)
    where flattenPar' (x:xs) = 
                      case x of
                        Par line l -> case (flattenPar x) of
                                    Par line l' -> l'++(flattenPar' xs)
                                    t -> t:(flattenPar' xs)
                        _ -> x:(flattenPar' xs)  
          flattenPar' [] = []
flattenPar t = t

-- Remove Nulls from Par (i.e.  T | 0 == T)--
gcNull :: GoType -> GoType
gcNull (Par line l) = let res = gcNull' l in
       if (L.null res) then Null else Par line res
  where gcNull' (x:xs) =
                case x of 
                     Null -> gcNull' xs
                     _ -> x:(gcNull' xs)
        gcNull' [] = []
gcNull t = t

-- GC unused bound names --
gcBNames' :: GoType -> M GoType
gcBNames' (Send l c t) = do
  t' <- gcBNames' t
  return $ Send l c t'
gcBNames' (Recv l c t) = do
  t' <- gcBNames' t
  return $ Recv l c t'
gcBNames' (Tau l t) = do
  t' <- gcBNames' t
  return $ Tau l t'
gcBNames' (IChoice line t1 t2) = do
  t1' <- gcBNames' t1
  t2' <- gcBNames' t2
  return $ IChoice line t1' t2'
gcBNames' (OChoice line l) = do
 lm' <- mapM gcBNames' l
 return $ OChoice line lm'
gcBNames' (Par line l) = do
 lm' <- mapM gcBNames' l
 return $ Par line lm'
gcBNames' (New line i bnd) = do
  (c,t) <- unbind bnd
  t' <- gcBNames' t
  -- GC if c not used
  if c `S.notMember` fv t' 
    then return t'
    else return (New line i (bind c t'))
gcBNames' (Null) = return Null
gcBNames' buf@(Buffer c _) = return buf  
gcBNames' (Close l c t) = do
  t' <- gcBNames' t
  return $ Close l c t'
gcBNames' (TVar line x) = return $ TVar line x
gcBNames' (ChanInst t lc) = do  -- P(~c)
  t' <- gcBNames' t
  return $ ChanInst t' lc
gcBNames' (ChanAbst bnd) = do
  (c,t) <- unbind bnd
  t' <- gcBNames' t
  return $ ChanAbst (bind c t')
gcBNames' (Seq line l) = do
  l' <- mapM gcBNames' l
  return $ Seq line l'
  

gcBNames :: GoType -> GoType
gcBNames = runFreshM . gcBNames'







-- Open top-level bound names in a list of parallel types --
-- return is a list of (mc,t) where mc is Nothing if t is
-- closed and Just(c) otherwise.
openBNames :: [GoType] -> M [([Maybe (Int, ChName)],(GoType,String))]
openBNames (x:xs) = do
     (l,t) <- openBNamesT x
     rest <- openBNames xs
     return $ (l,t):rest
openBNames [] = return $ [([Nothing],(Null,""))]

openBNamesT :: GoType -> M ([Maybe (Int, ChName)], (GoType, String))
openBNamesT (New line i bnd) = do
            (c,t) <- unbind bnd
            (l,t') <- openBNamesT t
            return $ ( Just(i,c):l ,(fst t',line))
openBNamesT t = return $ ([Nothing],(t,""))
    

-- Reconstructs the appropriate GoType from calls
-- to openBNames
closeBNames ::  M [([Maybe (Int, ChName)],(GoType, String))] -> M GoType
closeBNames m  = do
  l <- m
  let (names,ts) = unzip l
  let names' = concat names
  return $ L.foldr (\mc end ->
                    case mc of
                      Just(i,c) -> New (snd (head ts)) i (bind c end)
                      Nothing -> end) (Par "" (getFirsts ts)) names' --Check THIS

getFirsts :: [(GoType, String)] -> [GoType]
getFirsts [] = []
getFirsts (x:xs) = (fst x) : (getFirsts xs)

-- Composes open/close and escapes the freshness monad --
pullBNamesPar :: GoType -> GoType
pullBNamesPar (Par _ l) =
  runFreshM (closeBNames . openBNames $ l)
pullBNamesPar t = t 

  
nf :: M GoType -> M GoType
nf t = do t1 <- t
          (nf' (gcBNames t1))
   where nf' Null = return Null
         nf' (Send l c t) = do
             t' <- nf' t
             return $ (Send l c t')
         nf' (Recv l c t) = do
             t' <- nf' t
             return $ (Recv l c t')
         nf' (Tau l t) = do
             t' <- nf' t
             return $ (Tau l t')
         nf' (IChoice line t1 t2) = do
             t1' <- nf' t1
             t2' <- nf' t2
             return $ IChoice line t1' t2'
         nf' (OChoice line l) = do
             l' <- mapM nf' l
             return $ OChoice line l'
         nf' t@(Par line l) = do
             let t' = (gcNull . pullBNamesPar  . flattenPar $ t)
             case t' of
              Par line l' -> do
                          l'' <- mapM nf' l'
                          return $ Par line l''
              _ -> nf' t'
         nf' (New line i bnd) = do
             (c,t) <- unbind bnd
             t' <- nf' t
             return $ (New line i (bind c t'))
         nf' (Close l c t) = do
             t' <- nf' t
             return $ (Close l c t')
         nf' (TVar line x) = return $ TVar line x
         nf' t@(ChanInst t0 l) = return $ t
         nf' (ChanAbst bnd) = do
             (l,t) <- unbind bnd
             t' <- nf' t
             return $ (ChanAbst (bind l t'))
         nf' (Seq line l) = do
             l' <- mapM nf' l
             return $ Seq line l'
         nf' buf@(Buffer c _) = return buf
             

-- structCong :: GoType -> GoType -> Bool
-- structCong t1 t2 = (nf t1) `aeq` (nf t2)


-----------


gcBufferList :: [ChName] -> [GoType] -> [GoType] -> [GoType]
gcBufferList names prev [] = prev
gcBufferList names prev (x:xs) = case x of
  Null -> gcBufferList names prev xs
  Buffer c (o,i,j) ->
    if (j > 0) || o  || (c `L.elem` names)
    then gcBufferList names (prev++[x]) xs
    else let fna ys = L.foldr (++) [] $ L.map fv ys
             left = fna prev
             right = fna xs
         in if (c `L.elem` (right++left)) -- || ((L.null prev) && (L.null xs))
            then gcBufferList names (prev++[x]) xs
            else gcBufferList names prev xs
  otherwise -> gcBufferList names (prev++[x]) xs
                              
gcBuffer :: [ChName] -> M GoType -> M GoType
gcBuffer names t = do t' <- t
                      gcBuffer' names t'

gcBuffer' :: [ChName] -> GoType -> M GoType
gcBuffer' names (Par line list) = return $ Par line $ gcBufferList names [] list
gcBuffer' names (New line i bnd) = do
  (c,t) <- unbind bnd
  t' <- gcBuffer' names t--(c:names) t
  return $ New line i (bind c t')
gcBuffer' names t = return t



-- Once unfoldings of GoTypes and EquationSys --

unfoldType :: GoType -> M GoType
unfoldType (Send l c t) = do
  t' <- unfoldType t
  return $ Send l c t'
unfoldType (Recv l c t) = do
  t' <- unfoldType t
  return $ Recv l c t'
  
unfoldType (Tau l t) = do
  t' <- unfoldType t
  return $ Tau l t'
unfoldType (IChoice line t1 t2) = do
  t1' <- unfoldType t1
  t2' <- unfoldType t2
  return $ IChoice line t1' t2'
unfoldType (OChoice line l) = do
 lm' <- mapM unfoldType l
 return $ OChoice line lm'
unfoldType (Par line l) = do
 lm' <- mapM unfoldType l
 return $ Par line lm'
unfoldType (New line i bnd) = do
  (c,t) <- unbind bnd
  t' <- unfoldType t
  -- GC if c not used
  if c `S.notMember` fv t'
    then return t'
    else return (New line i (bind c t'))
unfoldType (Null) = return Null
unfoldType (Close l c t) = do
  t' <- unfoldType t
  return $ Close l c t'
unfoldType (TVar line x) = return $ TVar line x
unfoldType (ChanInst t lc) = do  -- P(~c)
  t' <- unfoldType t
  case t' of
   ChanAbst bnd -> do -- P == (\~d.P)(~c)
     (ld,t0) <- unbind bnd
     let perm = L.foldr (\(d,c) acc -> compose acc (single (AnyName d) (AnyName c))  )
                (Unbound.LocallyNameless.empty) (zip ld lc)
     return $ swaps perm t0
   otherwise -> return $ ChanInst t' lc
unfoldType (ChanAbst bnd) = do
  (c,t) <- unbind bnd
  t' <- unfoldType t
  return $ ChanAbst (bind c t')
unfoldType (Seq line l) = do
  l' <- mapM unfoldType l
  return $ Seq line l'


unfoldEqn :: Eqn -> M Eqn
unfoldEqn (EqnSys bnd) = do
    (r,body) <- unbind bnd
    let vars = unrec r
    let newbody = L.foldr (\(x,Embed rhs) body -> subst x rhs body) body vars
    return $ EqnSys (bind (rec vars) newbody)

unfoldTop :: Eqn -> M Eqn
unfoldTop (EqnSys bnd) = do
    (r,body) <- unbind bnd
    let vars = unrec r
    let newbody = L.foldr (\(x,Embed rhs) body -> subst x rhs body) body vars
    bla <- unfoldType newbody
    return $ EqnSys (bind (rec vars) bla)


---- Fencing predicate for types ----

-- G ; ~y ; ~z |-t T  
-- G records previously encountered recursive calls
-- ~y represents names that t can use if T is single-threaded
-- ~z represents names that a sub-process of T can use if T is multi-threaded


-- EqnSys (Bind (Rec [(EqnName , Embed GoType)]) GoType)

finMem :: (Eq a) => [a] -> [a] -> Bool
finMem l1 l2 = not (null l1 || null l2 || (length l1 /= length l2)) &&
                 let sl1 = tail (inits l1) in 
                    aux sl1 l2 l1                    
   where aux (x:y:xs) l l1 = if (L.isSuffixOf x l) then
                             null ((drop (length x) l1) `intersect` l)
                          else
                             aux (y:xs) l l1
         aux [x] l l1 = null (x `intersect` l)
         aux [] l l1 = False

-- abd `finMem` abc = True
-- abc `finMem` abc = False
-- bcda  `finMem` abcd = False
-- cdab `finMem` abcd = False
-- cdaa `finMem` abcd = False               

checkFinite :: Bool -> [(EqnName , Embed GoType)] -> (Set EqnName) ->
                 [ChName] -> [ChName] ->  EqnName -> GoType -> M Bool
checkFinite debug defEnv pRecs ys zs cDef (Send l c t) = checkFinite debug defEnv pRecs ys zs cDef t
checkFinite debug defEnv pRecs ys zs cDef (Recv l c t) = checkFinite debug defEnv pRecs ys zs cDef t
checkFinite debug defEnv pRecs ys zs cDef (Tau l t)    = checkFinite debug defEnv pRecs ys zs cDef t
checkFinite debug defEnv pRecs ys zs cDef (IChoice _ t1 t2) = do
                             b1 <- checkFinite debug defEnv pRecs ys zs cDef t1
                             b2 <- checkFinite debug defEnv pRecs ys zs cDef t2
                             return $ b1 && b2
checkFinite debug defEnv pRecs ys zs cDef (OChoice _ l) = do
                             foldM (\acc t -> do
                                               b <- checkFinite debug defEnv pRecs ys zs cDef t
                                               return $ b && acc) True l
checkFinite debug defEnv pRecs ys zs cDef (Par _ [t]) = checkFinite debug defEnv pRecs ys zs cDef t
checkFinite debug defEnv pRecs ys zs cDef (Par _ l) = do
                             foldM (\acc t -> do
                                               b <- checkFinite debug defEnv pRecs [] (zs++ys) cDef t
                                               return $ b && acc) True l
--                             recCase <- foldM (\acc t -> do
--                                               b <- checkFinite debug defEnv pRecs ys zs cDef t
--                                               return $ b && acc) True (tail l)
--                             parCase <- checkFinite debug defEnv pRecs [] (zs++ys) cDef (head l)
--                             return $ (recCase && parCase)
checkFinite debug defEnv pRecs ys zs cDef (New _ i bnd) = do
                              (c,t) <- unbind bnd
                              checkFinite debug defEnv pRecs ys zs cDef t
checkFinite debug defEnv pRecs ys zs cDef (Null) = return $ True
checkFinite debug defEnv pRecs ys zs cDef (Close l c t) = checkFinite debug defEnv pRecs ys zs cDef t
checkFinite debug defEnv pRecs ys zs cDef t@(TVar line x) = error $ "[checkFinite] Oops: "++(show t)
                                                 -- Should be handled in ChanInst
checkFinite debug defEnv pRecs ys zs cDef (ChanInst (TVar line x) l) =
               if (x == cDef) then
                 -- return $ ((not . null $ ys) || (l `finMem` zs))
                 if ((not . null $ ys) || (l `finMem` zs))
                 then return True
                 else if debug
                      then error $ "Not fenced: "++(show ((l,zs),ys))
                      else return False
               else
                  if (x `S.member` pRecs) then
                   return $ True 
                  else
                    do
                      let tdef = (case (L.lookup x defEnv) of
                                   Just(Embed(t)) -> t
                                   _ -> error $ "Something went wrong, can't find: "++(show x)) 
                      let tabs = (case tdef of
                                   ChanAbst bnd -> bnd
                                   _ -> error "boom! wtf")
                      (params,abs) <- unbind tabs
                      let perm = L.foldr (\(d,c) acc -> compose acc (single (AnyName d) (AnyName c))  )
                                              (Unbound.LocallyNameless.empty) (zip params l)
                      checkFinite debug defEnv (S.insert x pRecs) ys zs cDef (swaps perm abs)
checkFinite debug defEnv pRecs ys zs cDef (ChanAbst bnd) = return $ True -- this shouldn't come up here I think
checkFinite debug defEnv pRecs ys zs cDef (Seq _ l) = do
                               foldM (\acc t -> do
                                               b <- checkFinite debug defEnv pRecs ys zs cDef t
                                               return $ b && acc) True l


-- maybe just check main?
checkFiniteP debug (EqnSys bnd) = do
             (defs,main) <- unbind bnd
             let defEnv = unrec defs
             b <- foldM (\acc (x,Embed(ChanAbst bnd))  -> do
                              (l,t) <- unbind bnd
                              b <- if null l then return True else checkFinite debug defEnv (S.empty) l [] x t
                              return $ (acc && b)) True defEnv
             b' <- checkFinite debug defEnv (S.empty) [] [] (s2n "main") main
             return $ (b && b')

runCheck :: Bool -> Eqn -> Bool
runCheck debug p = runFreshM $ checkFiniteP debug p

