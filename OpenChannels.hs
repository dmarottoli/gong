{-# LANGUAGE BangPatterns #-}
module OpenChannels where

import GoTypes
import SymbolicSem
import Utils
import PrettyGoTypes
import Data.List


import Unbound.LocallyNameless
import Unbound.LocallyNameless.Ops

import Control.Parallel.Strategies
import Data.List as L
--import Data.Set as S (intersection, null, fromList, toList)

-- import Control.Monad.Trans.State.Lazy
-- import Control.Monad
-- import Control.Concurrent.Async

-- DEBUG
import System.IO.Unsafe
import Debug.Trace





getOpenChannels :: GoType -> M [(String,String)]
getOpenChannels (New line _ bnd) = 
    do (name,next) <- unbind bnd
       tmp <- getOpenChannels next
       return ((name2String name,line):tmp)
getOpenChannels _ = do return []




openChannels :: Bool -> Int -> M [Eqn] -> M String
openChannels debug k eqs =
    do list <- eqs
       case list of
         (sys@(EqnSys bnd):xs) ->
           do (defs, main) <- unbind bnd
              ty <- extractType $ return main
              let names = L.nub $ fv ty :: [ChName]
              sucs <- succsNode k names (EqnSys bnd)
              sucs2 <- genSuccs (unrec defs) main
              ocRep <- trace ("\n ACTUAL: " ++ show sys ++ "\n MAIN: " ++ show main ++ "\n SUC1: " ++ show sucs ++ "\n SUC2: " ++ show sucs2) $ getOpenChannels main
              let oc = nub ocRep
              if (L.null sucs) && (length oc > 0)
                then return ("There is an open channel")
                else openChannels debug k $ return xs
         [] -> return ("No open channels")
















