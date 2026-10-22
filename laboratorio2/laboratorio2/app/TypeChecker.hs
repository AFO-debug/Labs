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
typecheck p = do
    e <- llenarEnv emptyEnv p
    checkProg e p

llenarEnv :: Env -> Program -> Err Env
llenarEnv e (PDefs []) = Ok e
llenarEnv e (PDefs (DFun t i as _): ds) = do
    e' <- updateFun e i (ponerTypes as, t)
    llenarEnv e' (PDefs ds)

ponerTypes :: [Arg] -> [Type]
ponerTypes as = [t | ADecl t _ <- as]

checkProg :: Env -> Program -> Err ()
checkProg e (PDefs ds) = mapM_ (checkDef e) ds

checkDef :: Env -> Def -> Err ()
checkDef = undefined

inferExp :: Env -> Exp -> Err Type
inferExp = undefined

checkExp :: Env -> Exp -> Type -> Err ()
checkExp = undefined

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms = undefined
