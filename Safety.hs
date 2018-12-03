module Safety where

import Liveness
import GoTypes
import SymbolicSem
import Utils
import PrettyGoTypes

import Unbound.LocallyNameless
import Unbound.LocallyNameless.Ops

import Data.List as L
import Data.Set as S (intersection, null, fromList)

-- DEBUG
import System.IO.Unsafe
import Debug.Trace




getContinuation :: GoType -> Maybe [GoType]
getContinuation (Close line c ty) = Just [addToLine ty ("CLOSE operation on line " ++ line), Buffer c (False, 0,0)] -- DEAL WITH BUFFER
getContinuation _ = Nothing

closebarbs :: GoType -> [GoType]
closebarbs (Close l c ty) = [Close l c Null]
closebarbs t = []

forbiddenAction :: GoType -> GoType -> [GoType]
forbiddenAction (Send l n t) _ = [Send l n Null]
forbiddenAction (Close l c ty) _ = [Close l c Null]
forbiddenAction (New _ i bnd) t2 = let (c,ty) = unsafeUnbind bnd
                              in forbiddenAction ty t2
forbiddenAction (Par _ xs) c = L.foldr (++) [] $ L.map (flip forbiddenAction c) xs
forbiddenAction t _ = []


badmatch :: GoType -> GoType -> Bool
badmatch (Close _ c ty) (Send _ n t) = c == n
badmatch (Close _ c ty) (Close _ n t) = c == n
badmatch _ _ = False


noclose :: [GoType] -> [GoType] -> [(GoType, GoType)]
noclose [] [] = []
noclose list@(x:xs) ys =
  let prod = cartProd list ys
  in L.filter (\(x,y) -> badmatch x y) prod


checkPair :: GoType -> GoType -> [(GoType, GoType)]
checkPair t1 t2 = noclose (closebarbs t1) (forbiddenAction t2 t1)


checkList :: GoType -> [GoType] -> [(GoType, GoType)]
checkList _ [] = []
checkList current (x:xs) = (checkPair current x) ++ (checkList current xs)
                           
                           


-- Given a parallel composition of type, check whether each
-- one can make a move
--
checkAllSuccs :: [ChName] -> Int -> Rec [(EqnName, Embed GoType)] -> [GoType] -> [GoType] -> M [(GoType, GoType)]
checkAllSuccs names k sys prev [] = return []
checkAllSuccs names k sys prev (x:next) =
  case getContinuation x of
    Just ty -> 
      do  let temp = succsNode k names (EqnSys $ bind sys (Par "s" (prev++ty++next))) :: M [Eqn]
          nexts <- temp
          gotypes <- eqnToTypes temp
          rest <- (checkAllSuccs names k sys (prev++[x]) next)
          return $ ((checkList x gotypes) ++ rest)
    Nothing ->  checkAllSuccs names k sys (prev++[x]) next





safety :: Bool -> Int -> M [Eqn] -> M Bool
safety debug k eqs =
  do list <- eqs
     case list of
       (sys@(EqnSys bnd):xs) ->
         do (defs, main) <- unbind bnd
            ty <- extractType (return main)
            let names = L.nub $ fv ty
            out <- checkAllSuccs names k defs [] ty
            if L.null out
              then safety debug k $ return xs
              else if debug
                   then error $ "Term not safe: " ++ "\n" ++ findCollidingOperations (foldr (\x rec -> if (foldr (\y recBool -> x `aeq` y || recBool) False rec) then rec else (x:rec)) [] out)
                   else return False
       [] -> return True


--filterEquivalentBadMatch :: [(GoType,GoType)] -> [(GoType,GoType)]
--filterEquivalentBadMatch [] = []
--filterEquivalentBadMatch (x:xs) = (itsUnique x xs)++(filterEquivalentBadMatch xs)
--
--itsUnique :: (GoType, GoType) -> [(GoType, GoType)] ->[(GoType, GoType)]
--itsUnique g gs = if (foldr (\x rec -> g `aeq` x || rec) False gs) then [] else [g]
--
--sameForbiddenAction :: (GoType, GoType) -> (GoType, GoType) -> Bool
--sameForbiddenAction (g1, g2) (t1, t2) = ((g1 `aeq` t1) && (g2 `aeq` t2)) || ((g1 `aeq` t2) && (g2 `aeq` t1))
--
--thereIsASend :: [GoType] -> Bool
--thereIsASend xs = length (filter (\x -> case x of (Send l ch _) -> True; otherwise -> False) xs) > 0


findCollidingOperations :: [(GoType, GoType)] -> String
findCollidingOperations gts = intercalate ";\n" (map printColliding gts)

printColliding :: (GoType, GoType) -> String
printColliding (t1, t2) = "There is a " ++ operation t1 ++ "\n Colliding with:\nA " ++ operation t2 ++ "."

operation :: GoType -> String
operation (Close line ch t) = "close operation on line " ++ line
operation (Send line ch t) = "send operation on line " ++ line

-- we still need to check that the operations are done on the same channel
--collidingOperation :: [GoType] -> ChName -> String
--collidingOperation xs ch = let sends = filter (\x -> case x of (Send l ch _)-> True; otherwise -> False) xs in
--                            if (length sends) > 0
--                               then
--                                  case (head sends) of (Send line ch _) -> "send operation on line " ++ show line
--                               else
--                                  nextCloseStatementLine xs ch
--
--
--nextCloseStatementLine :: [GoType] -> ChName -> String
--nextCloseStatementLine xs ch = let closes = filter (\x -> case x of (Close l ch _)-> True; otherwise -> False) xs in
--                                    if (length closes) > 0
--                                        then
--                                            case (head closes) of (Close line _ _) -> "close operation on line " ++ show line
--                                        else
--                                            "unknown operation"