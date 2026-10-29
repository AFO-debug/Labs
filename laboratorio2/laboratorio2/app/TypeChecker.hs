{-# LANGUAGE CPP #-}

module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Env
import Control.Monad (foldM, foldM_)
import Data.IntMap (update)

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
#endif

typecheck :: Program -> Err ()
typecheck p = do
    e <- llenarEnv emptyEnv p
    checkProg e p

llenarEnv :: Env -> Program -> Err Env
llenarEnv e (PDefs []) = Ok e
llenarEnv e (PDefs (DFun t i as body : ds)) = do
    e' <- updateFun e i (ponerTypes as, t)
    llenarEnv e' (PDefs ds)

ponerTypes :: [Arg] -> [Type]
ponerTypes as = [t | ADecl t _ <- as]

checkProg :: Env -> Program -> Err ()
checkProg e (PDefs ds) = mapM_ (checkDef e) ds

checkDef :: Env -> Def -> Err ()
checkDef env (DFun x i args stms) = do
    env2 <- foldM (\env' (ADecl t ident) -> updateVar env' ident t) env args
    foldM_ (checkStm x) env2 stms

inferExp :: Env -> Exp -> Err Type
inferExp e x = case x of
    ETrue -> Ok Type_bool
    EFalse -> Ok Type_bool
    EInt n -> Ok Type_int
    EDouble n -> Ok Type_double   
    EString s -> Ok Type_string
    EId i -> lookupVar e i
    EApp f args -> do
        (paramTypes, returnType) <- lookupFun e f
        argTypes <- mapM (inferExp e) args
        if paramTypes == argTypes
            then Ok returnType
            else Bad "tipos de argumentos incorrectos"
    EPIncr x' -> do
        t <- inferExp e x'
        if t == Type_int || t == Type_double
            then Ok t
            else Bad "EPIncr requiere un entero o double"
    EPDecr x' -> do
        t <- inferExp e x'
        if t == Type_int || t == Type_double
            then Ok t
            else Bad "EPDecr requiere un entero o double"
    EIncr x' -> do
        t <- inferExp e x'
        if t == Type_int || t == Type_double
            then Ok t
            else Bad "EIncr requiere un entero o double"
    EDecr x' -> do
        t <- inferExp e x'
        if t == Type_int || t == Type_double
            then Ok t
            else Bad "EDecr requiere un entero o double"
    ETimes x1 x2 -> do
        t1 <- inferExp e x1
        t2 <- inferExp e x2
        if t1 == t2 && (t1 == Type_int || t1 == Type_double)
            then Ok t1
            else Bad "tipos incompatibles en multiplicacion"
    EDiv x1 x2 -> do
        t1 <- inferExp e x1
        t2 <- inferExp e x2
        if t1 == t2 && (t1 == Type_int || t1 == Type_double)
            then Ok t1
            else Bad "tipos incompatibles en division"
    EPlus x1 x2 -> do
        t1 <- inferExp e x1
        t2 <- inferExp e x2
        if t1 == t2 && (t1 == Type_int || t1 == Type_double)
            then Ok t1
            else Bad "tipos incompatibles en suma"
    EMinus x1 x2 -> do
        t1 <- inferExp e x1
        t2 <- inferExp e x2
        if t1 == t2 && (t1 == Type_int || t1 == Type_double)
            then Ok t1
            else Bad "tipos incompatibles en resta"
    ELt x1 x2 -> do
        t1 <- inferExp e x1
        t2 <- inferExp e x2
        if t1 == t2 && (t1 == Type_int || t1 == Type_double)
            then Ok Type_bool
            else Bad "tipos incompatibles en menor que"
    EGt x1 x2 -> do
        t1 <- inferExp e x1
        t2 <- inferExp e x2
        if t1 == t2 && (t1 == Type_int || t1 == Type_double)
            then Ok Type_bool
            else Bad "tipos incompatibles en mayor que"
    ELtEq x1 x2 -> do
        t1 <- inferExp e x1
        t2 <- inferExp e x2
        if t1 == t2 && (t1 == Type_int || t1 == Type_double)
            then Ok Type_bool
            else Bad "tipos incompatibles en menor o igual"
    EGtEq x1 x2 -> do
        t1 <- inferExp e x1
        t2 <- inferExp e x2
        if t1 == t2 && (t1 == Type_int || t1 == Type_double)
            then Ok Type_bool
            else Bad "tipos incompatibles en mayor o igual"
    EEq x1 x2 -> do
        t1 <- inferExp e x1
        t2 <- inferExp e x2
        if t1 == t2
            then Ok Type_bool
            else Bad "tipos incompatibles en igualdad"
    ENEq x1 x2 -> do
        t1 <- inferExp e x1
        t2 <- inferExp e x2
        if t1 == t2
            then Ok Type_bool
            else Bad "tipos incompatibles en desigualdad"
    EAnd x1 x2 -> do
        t1 <- inferExp e x1
        t2 <- inferExp e x2
        if t1 == Type_bool && t2 == Type_bool
            then Ok Type_bool
            else Bad "AND requiere booleanos"
    EOr x1 x2 -> do
        t1 <- inferExp e x1
        t2 <- inferExp e x2
        if t1 == Type_bool && t2 == Type_bool
            then Ok Type_bool
            else Bad "OR requiere booleanos"
    EAss x1 x2 -> do
        t1 <- inferExp e x1
        t2 <- inferExp e x2
        if t1 == t2
            then Ok t1
            else Bad "tipos incompatibles en asignacion"

checkExp :: Env -> Exp -> Type -> Err ()
checkExp e exp t = do
    t' <- inferExp e exp
    if t' == t
        then Ok ()
        else Bad "Los tipos no coinciden"


checkStm :: Type -> Env -> Stm -> Err Env
checkStm x e stm = case stm of
    SExp exp -> do
        _ <- inferExp e exp
        Ok e

    SDecls t ids ->
        foldM (\env ident -> updateVar env ident t) e ids

    SInit t ident exp -> do
        checkExp e exp t
        updateVar e ident t

    SReturn exp -> do
        checkExp e exp x
        Ok e

    SReturnVoid ->
        if x == Type_void
            then Ok e
            else Bad "Los tipos no coinciden"

    SWhile exp body -> do
        checkExp e exp Type_bool
        _ <- checkStm x (newBlock e) body
        Ok e

    SBlock stms -> do
        env' <- foldM (checkStm x) (newBlock e) stms
        Ok (endBlock env')

    SIfElse exp stm1 stm2 -> do
        checkExp e exp Type_bool
        _ <- checkStm x (newBlock e) stm1
        _ <- checkStm x (newBlock e) stm2
        Ok e
