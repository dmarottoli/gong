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
getContinuation (Close _ c ty) = Just [ty, Buffer c (False, 0,0)] -- DEAL WITH BUFFER
getContinuation _ = Nothing

closebarbs :: GoType -> [GoType]
closebarbs (Close l c ty) = [Close l c Null]
closebarbs t = []

forbiddenAction :: GoType -> [GoType]
forbiddenAction (Send l n t) = [Send l n Null]
forbiddenAction (Close l c ty) = [Close l c Null]
forbiddenAction (New _ i bnd) = let (c,ty) = unsafeUnbind bnd
                              in forbiddenAction ty
forbiddenAction (Par _ xs) = L.foldr (++) [] $ L.map forbiddenAction xs
forbiddenAction t = []


badmatch :: GoType -> GoType -> Bool
badmatch (Close _ c ty) (Send _ n t) = c == n
badmatch (Close _ c ty) (Close _ n t) = c == n
badmatch _ _ = False


noclose :: [GoType] -> [GoType] -> Bool
noclose [] [] = True
noclose list@(x:xs) ys =
  let prod = cartProd list ys
  in L.null $ L.filter (\(x,y) -> badmatch x y) prod


checkPair :: GoType -> GoType -> Bool
checkPair t1 t2 = noclose (closebarbs t1) (forbiddenAction t2)


checkList :: GoType -> [GoType] -> Bool
checkList _ [] = True
checkList current (x:xs) = if (checkPair current x)
                           then (checkList current xs)
                           else False -- error $ "Term no safe: "++(pprintType x)
                           
                           


-- Given a parallel composition of type, check whether each
-- one can make a move
--
checkAllSuccs :: [ChName] -> Int -> Rec [(EqnName, Embed GoType)] -> [GoType] -> [GoType] -> M Bool
checkAllSuccs names k sys prev [] = return True
checkAllSuccs names k sys prev (x:next) =
  case getContinuation x of
    Just ty -> 
      do  let temp = succsNode k names (EqnSys $ bind sys (Par "s" (prev++ty++next))) :: M [Eqn]
          nexts <- temp
          gotypes <- eqnToTypes temp
          rest <- (checkAllSuccs names k sys (prev++[x]) next)
          return $ (checkList x gotypes) && rest
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
            if out
              then safety debug k $ return xs
              else if debug
                   then error $ "Term not safe: " ++ show ty ++ "\n" ++ show (findCollidingOperations ty)
                   else return False
       [] -> return True


thereIsASend :: [GoType] -> Bool
thereIsASend xs = length (filter (\x -> case x of (Send l ch _) -> True; otherwise -> False) xs) > 0


findCollidingOperations :: [GoType] -> String
findCollidingOperations (x:xs) = case x of
                            (Close line ch _) -> "There is a closing operation on line " ++ show line ++ " colliding with a " ++ collidingOperation xs ch
                            (Send line ch _) -> "There is a send operation on line " ++ show line ++ " colliding with a " ++ nextCloseStatementLine xs ch
                            otherwise -> findCollidingOperations xs

-- we still need to check that the operations are done on the same channel
collidingOperation :: [GoType] -> ChName -> String
collidingOperation xs ch = let sends = filter (\x -> case x of (Send l ch _)-> True; otherwise -> False) xs in
                            if (length sends) > 0
                               then
                                  case (head sends) of (Send line ch _) -> "send operation on line " ++ show line
                               else
                                  nextCloseStatementLine xs ch


nextCloseStatementLine :: [GoType] -> ChName -> String
nextCloseStatementLine xs ch = let closes = filter (\x -> case x of (Close l ch _)-> True; otherwise -> False) xs in
                                    if (length closes) > 0
                                        then
                                            case (head closes) of (Close line _ _) -> "close operation on line " ++ show line
                                        else
                                            "unknown operation"