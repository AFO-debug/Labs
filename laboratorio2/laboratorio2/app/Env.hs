{-# LANGUAGE CPP #-}

module Env where

import AbsCPP
import PrintCPP
import ErrM

import Data.Map as Map

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
import Control.Monad hiding (fail)

fail = Bad
#endif

type Env = (Sig,[Context])
type Sig = Map Id ([Type],Type)
type Context = Map Id Type

lookupVar :: Env -> Id -> Err Type
lookupVar (_, ctxs) x = lookUpContext ctxs x

lookUpContext :: [Context] -> Id -> Err Type
lookUpContext []     x = Bad ("variable no declarada: " ++ show x)
lookUpContext (c:cs) x = case Map.lookup x c of
  Just t  -> Ok t
  Nothing -> lookUpContext cs x

lookupFun :: Env -> Id -> Err ([Type], Type)
lookupFun (s, _) x = case Map.lookup x s of
    Just t -> Ok t
    Nothing -> Bad ("funcion no declarada")

updateVar :: Env -> Id -> Type -> Err Env
updateVar (sig, (c:ctxs)) x t 
    |Map.member x c = Bad ("muy malo")
    |otherwise = Ok (sig, ((Map.insert x t c):ctxs))

updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun (s, cs) x t
    |Map.member x s = Bad ("muy mala")
    |otherwise = Ok ((Map.insert x t s), cs)

newBlock :: Env -> Env
newBlock (s, cs) = (s, (Map.empty:cs)) 

endBlock :: Env -> Env
endBlock (s, []) = (s, [])
endBlock (s, c:cs) = (s, cs)

emptyEnv :: Env
emptyEnv = (Map.empty, [Map.empty])
