{-# LANGUAGE BangPatterns #-}
module Liveness where

import GoTypes
import SymbolicSem
import Utils
import PrettyGoTypes
import Data.List


import Unbound.LocallyNameless
import Unbound.LocallyNameless.Ops

import Control.Parallel.Strategies
import Data.List as L
import Data.Set as S (intersection, null, fromList, toList)

-- import Control.Monad.Trans.State.Lazy
-- import Control.Monad
-- import Control.Concurrent.Async

-- DEBUG
import System.IO.Unsafe
import Debug.Trace


-- Barbs of a "sequential" type
barbs :: GoType -> [GoType]
barbs (Send l n t) = [Send l n Null]
barbs (Recv l n t) = [Recv l n Null]
barbs (OChoice _ xs) = L.foldr (++) [] $ L.map barbs xs
barbs (New _ i bnd) = let (c,ty) = unsafeUnbind bnd
                      in barbs ty
barbs (Par _ xs) = L.foldr (++) [] $ L.map barbs xs
barbs (Buffer c (open,b,k))
  | (k < b) && (k > 0) = [Send "BUFFER" c Null, Recv "BUFFER" c Null]
  | k > 0 = [Send "BUFFER" c Null]
  | k < b && open = [Recv "BUFFER" c Null]
  | k < b && not open = [Recv "BUFFER" c Null, Send "BUFFER" c Null]
  | not open = [Send "BUFFER" c Null]
  | otherwise = []
barbs t = []


synchronise :: [GoType] -> [GoType] -> Bool
synchronise [] _ = True -- No barbs: always good
synchronise [g] xs = not $ L.null $ L.filter (\x -> match x g) xs
synchronise list@(x:y:xs) ys =
  let prod = cartProd list ys
  in not $ L.null $ L.filter (\(x,y) -> match x y) prod


matchTypes :: GoType -> GoType -> Bool
matchTypes current candidate =
  synchronise (barbs current) (barbs candidate)

findMatch :: GoType -> [GoType] -> Bool
findMatch _ [] = False
findMatch t@(OChoice _ ys) (x:xs) = if any isTau ys
                                  then True
                                  else (matchTypes t x)
                                       ||
                                       (findMatch t xs)
findMatch current (x:xs) = (matchTypes current x)
                           ||
                           (findMatch current xs)
                           

eqnToTypes :: M [Eqn] -> M [GoType]
eqnToTypes mlist = do list <- mlist
                      helper list
  where helper :: [Eqn] -> M [GoType]
        helper ((EqnSys x):xs) = do (d,t) <- unbind x
                                    rest <- helper xs
                                    return $ (t:rest)
        helper [] = return []

-- Given a parallel composition of type, check whether each
-- one can make a move
--
checkStates :: [ChName] -> Int -> Rec [(EqnName, Embed GoType)] -> [GoType] -> [GoType] -> M [GoType]
checkStates names k sys prev [] = return []
checkStates names k sys prev (x:next) =
  if isBuffer x
  then checkStates names k sys (prev++[x]) next
  else
    do  let temp = succsNode k names (EqnSys $ bind sys (Par "" (prev++next))) :: M [Eqn] in
            do
                nexts <- temp
                gotypes <- eqnToTypes temp
                rest <- --trace ("\n tempGo = " ++ show gotypes ++ "\n" ++ "x = " ++ show x ++ "\n" ++ "prevNext = " ++ show (prev++next) ++ "\n" ++ "\n") $ 
                   (checkStates names k sys (prev++[x]) next)
                return $ if (not (findMatch x gotypes)) then (x:rest) else rest
--                  (
--                   -- if
--                    (findMatch x gotypes)
--                    -- then True
--                    -- else error $ show (pprintType x ,L.map pprintType gotypes)
--                  )
--                  && rest


liveness :: Bool -> Int -> M [Eqn] -> M String
liveness debug k eqs =
  do list <- eqs
     case list of
       (sys@(EqnSys bnd):xs) ->
         do (defs, main) <- unbind bnd
            ty <-  --trace ("main = " ++ show main ++ "\n") $
                  extractType (return main)
            let names = L.nub $ fv ty :: [ChName]
            out <- --trace ("names = " ++ show names ++ "\n" ++ "ty = " ++ show ty) $
               checkStates names k defs [] ty
            --traceM("out ------- " ++ (show out))
            if L.null out
              then liveness debug k $ return xs
              else if debug
                   then error $ "Term not live: " ++(show $ L.map pprintType ty) ++ "\n" ++ (analyze out)
                   else return ("Term not live: " ++ "\n" ++ (analyze out))
       [] -> return ("Term is live")

analyze :: [GoType] -> String
analyze gt = intercalate ";\n" (map notSynch gt)
--analyze [] = ""
--analyze (x:xs) = notSynch x ++ analyze xs

notSynch :: GoType -> String
notSynch gt = case gt of
                (Send l c _) -> "There is a Send operation without synch on line " ++ l
                (Recv l c _) -> "There is a Recv operation without synch on line " ++ l
                otherwise -> ""

-- ATTEMPT AT PARALLELISATION OF LIVENESS
--
atomLiveness :: Int -> Eqn -> Bool
atomLiveness k eq = L.null (runFreshM $ helper k eq)
  where helper k eq =
          case eq of
            sys@(EqnSys bnd) ->
              do (defs, main) <-unbind bnd
                 ty <- -- trace (show (defs,main)) $
                       extractType (return main)
                 let names = L.nub $ fv ty
                 checkStates names k defs [] ty



mapLiveness :: Int -> [Eqn] -> Bool
mapLiveness k eqs = helper
  where helper =
            let list = map (atomLiveness k) eqs `using` parListChunk ((length eqs) `div` 8) rpar
            in L.and list
       
-- atomLiveness :: Int -> Eqn -> M Bool
-- atomLiveness k eq = helper k eq
--   where helper k eq =
--           case eq of
--             sys@(EqnSys bnd) ->
--               do (!defs, main) <- unbind bnd
--                  ty <- trace (show main) $ extractType (return main)
--                  let names = L.nub $ fv ty :: [ChName]
--                  checkStates names k defs [] ty



-- -- mkStrat :: Strategy a -> Strategy m [a]

-- mapLiveness :: Int -> M [Eqn] -> M Bool
-- mapLiveness k eqs' = helper
--   where helper = do eqs <- eqs'
--                     let list = (mapM (atomLiveness k) eqs :: M [Bool])
--                                `using` rpar -- (parListChunk ((length eqs) `div` 8) rpar)
--                     list' <- list
--                     return $ L.and list'


-- metaLiveness :: Int -> M [Eqn] -> M Bool
-- metaLiveness k eqs' = helper
--   where helper = do eqs <- eqs'
--                     let (left,right) = splitAt ((length eqs) `div` 2) eqs
--                     t1 <- async return $ map (atomLiveness k) left
--                     t2 <- async return $ map (atomLiveness k) right
--                     w1 <- wait t1
--                     w2 <- wait t2
--                     return $ w1 && w2
