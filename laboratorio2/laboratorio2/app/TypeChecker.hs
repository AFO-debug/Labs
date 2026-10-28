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
checkDef e (DFun x i e s) = mapM_ (checkStms x e) s

inferExp :: Env -> Exp -> Err Type
inferExp e x = case x of
    ETrue -> Ok Type_bool
    EFalse -> Ok Type_bool
    EInt n -> Ok Type_int
    EDouble n -> Ok Type_double   
    EString s -> Ok Type_string
    EId i -> Ok (lookupVar e i)
    EApp f args -> do
        (paramTypes, returnType) <- lookupFun e f
        argTypes <- mapM (inferExp e) args
        if paramTypes == argTypes
            then Ok returnType
            else Bad "tipos de argumentos incorrectos"
    EPIncr x' -> do
        t <- inferExp e x'
        if t == Type_int
            then Ok Type_int
            else Bad "EPIncr requiere un entero"
    EPDecr x' -> do
        t <- inferExp e x'
        if t == Type_int
            then Ok Type_int
            else Bad "EPDecr requiere un entero"
    EIncr x' -> do
        t <- inferExp e x'
        if t == Type_int
            then Ok Type_int
            else Bad "EIncr requiere un entero"
    EDecr x' -> do
        t <- inferExp e x'
        if t == Type_int
            then Ok Type_int
            else Bad "EDecr requiere un entero"
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
    ETyped x' t -> do
        actualType <- inferExp e x'
        if actualType == t
            then Ok t
            else Bad "el tipo declarado no coincide con la expresion"

checkExp :: Env -> Exp -> Type -> Err ()
checkExp = undefined

checkStm :: Type -> Env -> Stm -> Err Env
checkStm x e [] = Ok e
checkStm x e t = case t of 
    SExp t' -> datas
    SDecls x' i -> data
    SInit x' i exp -> data
    SReturn exp -> data
    SReturnVoid -> data
    SWhile exp t' -> data
    SBlock ts' -> data
    SIfElse exp t' t'' -> data
