module SymbolicSem where

import GoTypes
import PrettyGoTypes (pprintEqn, pprintType)
import Utils
import TypeSize

import Unbound.LocallyNameless
import Unbound.LocallyNameless.Ops

import Data.List as L
import Data.Set as S (intersection, null, fromList)

import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

-- DEBUG
import System.IO.Unsafe
import Debug.Trace






inList :: GoType -> [GoType] -> Bool
inList t [] = False
inList t (x:xs) = (t `aeq` x) ||  (inList t xs)


  


symCondition :: [ChName] -> [ChName] -> Bool
symCondition m [] = False
symCondition m b = S.null $ intersection (fromList m) (fromList b)



normalise :: Int -> [ChName] -> Environment -> GoType -> GoType
normalise k names defEnv ty =
  let t1 = nfUnfold k names [] defEnv ty
  in runFreshM $ nf (gcBuffer . initiate $ evalState t1 "") -- Estoy tirando el estado. Dps usarlo para algo


nfUnfold :: Int -> [ChName] -> [EqnName] -> Environment -> GoType -> State String (M GoType)
nfUnfold k m seen defEnv t =
  unfoldTillGuard k m seen defEnv t

-- PILA
unfoldTillGuard :: Int -> [ChName] -> [EqnName] -> Environment -> GoType -> State String (M GoType)
unfoldTillGuard k m seen defEnv (Par xs) =
  do ys <- sequence (map (unfoldTillGuard k m seen defEnv) xs)
     return $ return $ (Par (concat (sequence ys)))
unfoldTillGuard k m  seen defEnv ori@(ChanInst (TVar t) lc)
  | (symCondition m lc) || (t `L.elem` seen) = return $ return ori
  | otherwise =
--  Acá es donde reemplaza la llamada a ChanInst TVar t por lo que hay en la lista de definiciones
    case L.lookup t defEnv of
      Just (Embed ty) ->
           case ty of
             ChanAbst bnd ->
               do (ld,t0) <- unbind bnd
                  let perm = L.foldr
                             (\(d,c) acc -> compose acc (single (AnyName d) (AnyName c)))
                             (Unbound.LocallyNameless.empty) (zip ld lc)
                  unfoldTillGuard k m (t:seen) defEnv $ swaps perm t0
             _ -> return $ return ty
      _ -> error $ "[unfoldTillGuard]Definition "++(show t)++" not found."++(show defEnv)
unfoldTillGuard k m  seen defEnv (New i bnd) =
  do (c,ty) <- unbind bnd
     nty <- let nm = if (length m) < k then c:m
                     else m
            in unfoldTillGuard k nm seen defEnv ty
     return $ return (New i (bind c (runFreshM nty))) -- Estoy perdiendo las variables frescas, esto creo que está mal!
unfoldTillGuard  k m seen defEnv (ChanAbst bnd) =
  do (c,ty) <- unbind bnd
     nty <- unfoldTillGuard k m seen defEnv ty
     return $ return (ChanAbst (bind c (runFreshM nty))) -- ACA ESTOY TIRANDO EL EFECTO
unfoldTillGuard  k m seen defEnv (Seq xs) = case xs of
  [x] -> unfoldTillGuard k m seen defEnv x
  [x,Null] -> unfoldTillGuard k m seen defEnv x
  otherwise -> error $ "[unfoldTillGuard] We don't deal with Seq yet: \n"++(pprintType $ Seq xs)
unfoldTillGuard  k m seen defEnv t = return $ return t

isTau :: GoType -> Bool
isTau (Tau _ t) = True
isTau t = False

getFreePars :: GoType -> M [GoType]
getFreePars (New i bnd) = do (c,ty) <- unbind bnd
                             getFreePars ty
getFreePars (Par xs) = return $ xs
getFreePars t = return $ [t]


getGuardsCont :: GoType -> [(GoType, GoType)]
getGuardsCont (Send l n t) = [(Send l n Null, t)]
getGuardsCont (Recv l n t) = [(Recv l n Null, t)]
getGuardsCont (Tau l t) = [(Tau l Null, t)]
getGuardsCont (IChoice t1 t2) = [(Tau 0 Null, t1), (Tau 0 Null, t2)]
getGuardsCont (OChoice xs) = L.foldr (++) [] $ map getGuardsCont xs
getGuardsCont (Close l c ty) = [(Close l c Null, ty)]
getGuardsCont (Buffer c (open,b,k))
    | (b==0 && k==0)= [(ClosedBuffer c, Buffer c (False,b,k))]
    | (k < b) && (k > 0) = [ (Send 0 c Null, Buffer c (open,b,k-1))
                           , (Recv 0 c Null, Buffer c (open,b,k+1))
                           , (ClosedBuffer c, Buffer c (False,b,k))
                           ]
    | k > 0 = [(Send 0 c Null, Buffer c (open,b,k-1))
              , (ClosedBuffer c, Buffer c (False,b,k))
              ]
    | k < b = [(Recv 0 c Null, Buffer c (open,b,k+1))
              , (ClosedBuffer c, Buffer c (False,b,k))
              ]
    | not open = [(Send 0 c Null, Buffer c (open,b,k-1))
                 , (ClosedBuffer c, Buffer c (False,b,k))
                 ]
    | otherwise = [] 
getGuardsCont _ = []





compatibleConts :: [(GoType, GoType)] -> [(GoType, GoType)] -> [(GoType, GoType)]
compatibleConts xs ys =
  let prod = cartProd xs ys
      compa ((g1,t1), (g2,t2)) = match g1 g2
  in L.map (\((g1,t1),(g2,t2)) -> (t1,t2)) $
     L.filter compa prod



match :: GoType -> GoType -> Bool
match ((Send _ c1 _)) ((Recv _ c2 _)) =  c1 == c2
match ((Recv _ c2 _)) ((Send _ c1 _)) =  c1 == c2
match ((Close _ c _)) ((ClosedBuffer c')) = c == c'
match _ _ = False


tauGuards :: [(GoType, GoType)] -> [(GoType, GoType)]
tauGuards xs =  L.filter (\(x,y) -> isTau x) xs
                    
blockingGuards :: [(GoType, GoType)] -> [(GoType, GoType)]
blockingGuards xs = L.filter (\(x,y) -> not $ isTau x) xs


succsOf :: [(GoType, GoType)] -> [GoType] -> [GoType] -> [[GoType]]
succsOf guards prevPar [] = []
succsOf guards prevPar (x:xs) = let coguards = blockingGuards $ getGuardsCont x
                                    next = compatibleConts guards coguards
                                in
                                 (
                                   L.map
                                   (\(t1,t2) -> [t1]++prevPar++[t2]++xs)
                                   next
                                 )
                                 ++
                                 (succsOf guards (prevPar++[x]) xs)

genParSuccs :: [GoType] -> [GoType] -> [[GoType]]
genParSuccs _ [] = []
genParSuccs prev (x:xs) =
  let guards = getGuardsCont x
      bguards = blockingGuards guards
      tauguards = tauGuards guards
      tausuccs =
        L.map (\x -> prev++[x]++xs) (L.map (\(g,t) -> t) tauguards)
  in (succsOf bguards prev xs)
     ++
     tausuccs
     ++
     (genParSuccs (prev++[x]) xs)





genSuccs :: Environment -> GoType -> M [GoType]
genSuccs defEnv (New i bnd) = do (c,ty) <- unbind bnd
                                 ret <- (genSuccs defEnv ty)
                                 return $ L.map (\t -> New i $ bind c t) ret
genSuccs defEnv (Par xs) = return $ L.map (\x -> Par x) $ genParSuccs [] xs
genSuccs defEnvt t = return $ L.map (\x -> Par x) $ genParSuccs [] [t]


genStates :: Int -> [ChName] -> Environment -> [GoType] -> [GoType] -> M [GoType]
genStates k names env seen [] = return seen
genStates k names env seen (x:xs)
  | x `inList` seen = genStates k names env seen xs
  | otherwise = do
    next <- genSuccs env x
    genStates k names env (x:seen) (xs++(L.map (normalise k names env) next))


succs :: Int -> Eqn -> M [Eqn]
succs bound sys = succsNode bound [] sys

succsNode :: Int -> [ChName] -> Eqn -> M [Eqn]
succsNode bound names (EqnSys bnd) =
  let k = if L.null names then bound else length names
  in do (defs,main) <- unbind bnd
--        traceM ("\n bnd ------------------------------------------------------ " ++ (show bnd))
--        traceM ("\n defs ------------------------------------------------------ " ++ (show defs))
--        traceM ("\n main ------------------------------------------------------ " ++ (show main))
--        traceM ("\n NORMALIZE ------------------------------------------------------" ++ (show (normalise k names (unrec defs) main)))
        states <- genStates k names (unrec defs) []
                  [(normalise k names (unrec defs) main)]
--        traceM ("\n LLEGO ACAAAAAAAA???")
--        traceM ("\n states ------------------------------------------------------ " ++ (show states))
        return $ L.map (\x -> EqnSys $ bind defs x) (states :: [GoType])


                  

extractType :: M GoType -> M [GoType]
extractType ty =
  do ty' <- ty
     case ty' of
       (New i bnd) -> if (i==(-1))
                      then do (c,t) <- unbind bnd
                              extractType (return t)
                      else error $ "[extractType]Channels not initiated: "++(pprintType ty')
       (Par xs) -> return xs
       otherwise -> return [ty']

initiate :: M GoType -> M GoType
initiate t = do t' <- t
                initiateChannels t'

initiateChannels :: GoType -> M GoType
initiateChannels (New i bnd) =
  do (c,t) <- unbind bnd
     ty <- initiateChannels t
     return $ if (i == -1)
              then  New i $ bind c ty -- no buffer if already created
              else  New (-1) $ bind c (Par [ty, Buffer c (True,i,0)])
initiateChannels (Send l c t) =  do t2 <- initiateChannels t; return $ Send l c t2
initiateChannels (Recv l c t) =  do t2 <- initiateChannels t; return $ Recv l c t2
initiateChannels (Tau l t) = do t2 <- initiateChannels t; return $ Tau l t2
initiateChannels (IChoice t1 t2) =
  do  t1' <- initiateChannels t1
      t2' <- initiateChannels t2
      return $ IChoice t1' t2'
initiateChannels (OChoice xs) =
  do ts <- mapM initiateChannels xs
     return $ OChoice ts
initiateChannels (Par xs) = 
  do ts <- mapM initiateChannels xs
     return $ Par ts
initiateChannels Null = return Null
initiateChannels (Close l c t) = do t2 <- initiateChannels t; return $ Close l c t2
initiateChannels (TVar x) = return $ TVar x
initiateChannels (Buffer c s) = return $ Buffer c s
initiateChannels (ChanInst t lc) = do t' <- initiateChannels t
                                      return $ ChanInst t' lc
initiateChannels (ChanAbst bnd) =
  do (c,t) <- unbind bnd
     t' <- initiateChannels t
     return $ ChanAbst $ bind c t'
initiateChannels (Seq [t]) = initiateChannels t
initiateChannels (Seq [t,Null]) = initiateChannels t
initiateChannels (Seq xs) = case last xs of
  Null -> initiateChannels (Seq $ init xs)
  otherwise ->  error $ "[initiateChannels] We don't deal with full Seq yet: "
                        ++(show $ L.map pprintType xs) 

