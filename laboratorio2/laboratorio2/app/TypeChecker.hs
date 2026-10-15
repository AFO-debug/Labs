{-# LANGUAGE CPP #-}

module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Env

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
#endif

typecheck :: Program -> Err ()
typecheck p = checkProg (llenarEnv emptyEnv p) p 

llenarEnv :: Env -> Program -> Env
llenarEnv e p = e

checkProg :: Env -> Program -> Err ()
checkProg _ (PDefs []) = Ok ()
checkProg e (PDefs (d:ds))
    |e == [] = Bad("hola")
    |otherwise = checkDef e d

checkDef :: Env -> Def -> Err ()
checkDef = undefined

inferExp :: Env -> Exp -> Err Type
inferExp = undefined

checkExp :: Env -> Exp -> Type -> Err ()
checkExp = undefined

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms = undefined
