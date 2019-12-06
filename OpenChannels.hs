{-# LANGUAGE BangPatterns #-}
module OpenChannels where

import GoTypes
import SymbolicSem
import Utils
import PrettyGoTypes
import Data.List
import TypeSize


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





getOpenChannelsFinalState :: GoType -> [(String,String)] -> M [(String,String)]
getOpenChannelsFinalState (New line _ bnd) acum = 
    do (name,main) <- unbind bnd
       openChan <- anOpenBuffer main name
       if openChan
          then getOpenChannelsFinalState main ((name2String name, eliminateHistory line):acum)
          else getOpenChannelsFinalState main acum
getOpenChannelsFinalState _ acum = do return acum


anOpenBuffer :: GoType -> ChName -> M Bool
anOpenBuffer (New _ _ bnd) ch =
    do (name,main) <- unbind bnd
       anOpenBuffer main ch
anOpenBuffer (Send _ _ ty) ch = anOpenBuffer ty ch
anOpenBuffer (Recv _ _ ty) ch = anOpenBuffer ty ch
anOpenBuffer (Tau _ ty) ch = anOpenBuffer ty ch
anOpenBuffer (IChoice _ ty1 ty2) ch =
    do b1 <- anOpenBuffer ty1 ch
       b2 <- anOpenBuffer ty2 ch
       return $ b1 || b2
anOpenBuffer (OChoice _ tys) ch =
    do bools <- sequence $ map (\x -> anOpenBuffer x ch) tys
       let res = foldr (||) False bools
       return res
anOpenBuffer (Par _ tys) ch =
    do bools <- sequence $ map (\x -> anOpenBuffer x ch) tys
       let res = foldr (||) False bools
       return res
anOpenBuffer (Close _ _ ty) ch = anOpenBuffer ty ch
anOpenBuffer (Buffer name (b,_,_)) ch = return $ if ((name2String name) == (name2String ch)) then b else False
anOpenBuffer _ _ = do return False




getOpenChannelsLineGoType :: [String] -> [(String,String)] -> GoType -> M [(String,String)]
getOpenChannelsLineGoType xs acum (New line _ bnd) = 
            do (name, main) <- unbind bnd
               if (elem (name2String name) xs)
                  then getOpenChannelsLineGoType xs ((name2String name,eliminateHistory line):acum) main
                  else getOpenChannelsLineGoType xs acum main
getOpenChannelsLineGoType xs acum (ChanAbst bnd)=
            do (_,main) <- unbind bnd
               getOpenChannelsLineGoType xs acum main
getOpenChannelsLineGoType _ acum _ = do return acum


getOpenChannelsLine :: [GoType] -> [String] -> M [[(String,String)]]
getOpenChannelsLine ty xs = sequence $ map (getOpenChannelsLineGoType xs []) ty


getGoTypesList :: Eqn -> M [GoType]
getGoTypesList (EqnSys bnd) = 
    do (defs,main) <- unbind bnd
       let aux = map snd (unrec defs)
       let aux2 = main:(foldr (\(Embed ty) rec -> ty:rec) [] aux)
       return aux2

openChannels :: Int -> M [Eqn] -> [(String,String)] -> M String
openChannels k eqs elims =
    do list <- eqs
       case list of
         (sys@(EqnSys bnd):xs) ->
           do (defs, main) <- unbind bnd
              ty <- extractType $ return main
              let names = L.nub $ fv ty :: [ChName]
              sucs <- genSuccs (unrec defs) main
              if (L.null sucs)
                then do opens <- getOpenChannelsFinalState main []
                        openChannels k (return xs) (opens++elims)
                else do let elim = concat $ map (normalise' k names (unrec defs)) sucs
                        tys <- getGoTypesList sys
                        tup <- getOpenChannelsLine tys (map name2String elim)
                        let tup2 = nub $ concat $ tup
                        openChannels k (return xs) (tup2++elims)
         [] -> return $ prettyshow $ norm elims


norm :: [(String,String)] -> [String]
norm xs = foldr (\x rec -> if ((snd x) == "") then rec else (snd x):rec) [] (nub xs)

eliminateHistory :: String -> String
eliminateHistory = foldr (\x rec -> if (x == '\n') then "" else x:rec) []


prettyshow :: [String] -> String
prettyshow [] = "No open channels"
prettyshow [x] = "Open channel created at line " ++ x
prettyshow xs = "Open channels created at lines " ++ prettyshow' xs

prettyshow' :: [String] -> String
prettyshow' [x,y] = x ++ " and " ++ y
prettyshow' (x:xs) = x ++ ", " ++ prettyshow' xs















