{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Compiler where

-- java virtual machine instructions reference
-- http://cs.au.dk/~mis/dOvs/jvmspec/ref-Java.html

-- cabal run laboratorio3 ./good

import AbsCPP
import TypeChecker

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Data.Functor.Identity

instance MonadFail Identity where
  fail = error

type Instruction = String
type FunType     = String
type Label       = Int

data Env = Env {
  funs      :: Map.Map Id FunType,
  vars      :: [Map.Map Id Int],    -- Stack of variables blocks !
  maxvars   :: [Int],               -- Stack of counter for variable addresses in each block
  maxstk    :: Int,                 -- counter for maximal stack depth
  labels    :: Int,                 -- counter for jump labels
  code      :: [Instruction] 
}

emptyEnv :: Env
emptyEnv = Env { funs=Map.empty,
                 vars=[],
                 maxvars=[0],
                 maxstk=0,
                 labels=0,
                 code=[]}

emit :: Instruction -> State Env ()
emit i = modify (\s -> s {code = i : code s})

typeSize :: Type -> Int
typeSize Type_double = 2
typeSize _           = 1

extendVar :: Type -> Id -> State Env ()
extendVar t i = modify (\s -> s {vars = Map.insert i (typeSize t) (head (vars s)) : vars s , 
                  maxvars = typeSize t + head (maxvars s) : maxvars s})
  

extendVars :: Type -> [Id] -> State Env ()
extendVars t = mapM_ (extendVar t)

typeJVM :: Type -> String
typeJVM Type_int    = "I"
typeJVM Type_double = "D"
typeJVM Type_void   = "V"
typeJVM Type_bool   = "Z"
typeJVM Type_string = "Ljava/lang/String;"

funJVMType :: String -> [Type] -> Type -> FunType
funJVMType i typs rty = i ++ "(" ++ (foldr (\ t s -> typeJVM t ++ s) "" typs) ++ ")" ++ typeJVM rty

funJVM :: String -> String -> [Type] -> Type -> FunType
funJVM i clas argty rty = "invokestatic " ++ clas ++"/" ++ funJVMType i argty rty

extendFunEnv :: String -> FunType -> State Env ()
extendFunEnv i ft = modify (\s -> s {funs = Map.insert (Id i) ft (funs s)})

extendDef :: String -> Def -> State Env ()
extendDef cls (DFun t (Id i) args stmts) = undefined

-- Besides added in type checker
extendBuiltinDefs :: State Env ()
extendBuiltinDefs = mapM_ ( \ ((Id i),(argTys,rty)) -> extendFunEnv i $ funJVM i "Runtime" argTys rty) buildInFunctions

newBlock :: State Env ()
newBlock = modify (\s -> s {vars = Map.empty : vars s})

exitBlock :: State Env ()
exitBlock = modify (\s -> s {vars = tail (vars s)})

newLabel :: State Env String
newLabel  = do 
  currentState <- get
  put currentState { labels = (labels currentState) + 1}
  return ("L" ++ show (labels currentState))

lookupFun :: Id -> State Env FunType
lookupFun i = do
  currentState <- get
  case Map.lookup i (funs currentState) of
    Just ft -> return ft
    Nothing -> error ("Function not found: " ++ show i)

lookupVar :: Id -> State Env Int
lookupVar i = do
  currentState <- get
  case findVar i (vars currentState) of
    Just address -> return address
    Nothing -> error ("Variable not found: " ++ show i)

findVar :: Id -> [Map.Map Id Int] -> Maybe Int
findVar _ [] = Nothing
findVar i (block:blocks) =
  case Map.lookup i block of
    Just address -> Just address
    Nothing -> findVar i blocks

-- exec state
-- Entry point from ccpp.
-- Arguments: cls is the class name and p is the typed embedded abstract syntax tree (returned by the type checker).
-- Hints: call compileP and run the State monad !
compile :: String -> Program -> [Instruction]
compile cls p = code (execState (compileP cls p) emptyEnv)

compileP :: String -> Program -> State Env () 
compileP cls (PDefs defs) = do
  emit $ ".class public " ++ cls
  emit $ ".super java/lang/Object"
  emit $ ""
  emit $ ".method public <init>()V"
  emit $ "  aload_0"
  emit $ "  invokenonvirtual java/lang/Object/<init>()V"
  emit $ "  return"
  emit $ ".end method"
  emit $ ""
  extendBuiltinDefs
  mapM  (extendDef cls) defs
  mapM_ compileDef defs

compileDef :: Def -> State Env ()
compileDef (DFun t (Id i) args stmts) = do
  newBlock 
  if i == "main" then do
       emit $ ".method public static main([Ljava/lang/String;)V"
       extendVar Type_string (Id "args") -- in fact is an array of strings ([Ljava/lang/String;)
  else emit $ ".method public static " ++ (funJVMType i (map (\ (ADecl t _) -> t) args) t)
  emit $ ".limit locals 1000"  -- aca se puede ser mas precisoo !
  emit $ ".limit stack  1000"  -- aca se puede ser mas precisoo !
  mapM (\ (ADecl t i) -> extendVar t i) args
  mapM compileStm stmts
  exitBlock
  emit $ "return"
  emit $ ".end method"
  emit ""

compileStm :: Stm -> State Env ()
compileStm stm = case stm of
  SExp exp -> compileExp exp
  SDecls t ids -> extendVars t ids
  SInit t i e -> do
    extendVar t i
    compileExp e
    posicion <- lookupVar i
    case t of
      Type_bool -> emit ("istore " ++ show posicion)
      Type_int -> emit ("istore " ++ show posicion)
      Type_double -> emit ("dstore " ++ show posicion)
      Type_string -> emit ("astore " ++ show posicion)
  SReturn e -> do
    compileExp e
    t <- lookType(e)
    case t of 
      Type_bool -> emit "ireturn"
      Type_int -> emit "ireturn"
      Type_double -> emit "dreturn"
      Type_string -> emit "areturn"
  SReturnVoid -> emit "return" 
  SWhile e s -> do
    testLabel <- newLabel
    endLabel <- newLabel
    emit (testLabel ++ ":")
    compileExp e
    emit ("ifeq " ++ endLabel)
    compileStm s
    emit("goto " ++ testLabel)
    emit(endLabel ++ ":")
  SBlock sts -> do
    newBlock
    mapM_ compileStm sts
    exitBlock
  SIfElse e st1 st2 -> do
    falseLabel <- newLabel
    trueLabel <- newLabel
    compileExp e
    emit ("ifeq " ++ falseLabel)
    compileStm st1
    emit ("goto " ++ trueLabel)
    emit (falseLabel ++ ":")
    compileStm st2
    emit (trueLabel ++ ":")

lookType :: Exp -> Type
lookType exp = case exp of
  ETrue -> Type_bool
  EFalse -> Type_bool
  EInt i -> Type_int
  EDouble d -> Type_double
  EString s -> Type_string
  EId i -> error "EId sin anotacion de tipo"
  EApp i es -> error "EApp sin anotacion de tipo"
  EPIncr i -> error "EPIncr sin anotacion de tipo"
  EPDecr i -> error "EPDecr sin anotacion de tipo"
  EIncr i -> error "EIncr sin anotacion de tipo"
  EDecr i -> error "EDecr sin anotacion de tipo"
  ETimes e1 e2 -> lookType e1
  EDiv e1 e2 -> lookType e1
  EPlus e1 e2 -> lookType e1
  EMinus e1 e2 -> lookType e1
  ELt e1 e2 -> Type_bool
  EGt e1 e2 -> Type_bool
  ELtEq e1 e2 -> Type_bool
  EGtEq e1 e2 -> Type_bool
  EEq e1 e2 -> Type_bool
  ENEq e1 e2 -> Type_bool
  EAnd e1 e2 -> Type_bool
  EOr e1 e2 -> Type_bool
  EAss i e -> lookType e
  ETyped e t -> t

compileExp :: Exp -> State Env ()
compileExp exp = case exp of
  ETrue -> emit "iconst_1"
  EFalse -> emit "iconst_0"
  EInt i -> emit ("ldc " ++ show i)
  EDouble d -> emit ("ldc2_w " ++ show d) 
  EString s -> emit ("ldc " ++ show s)
  EId i -> do
    posicion <- lookupVar i
    emit ("iload " ++ show posicion)
    -- ????????
  EApp i es -> do
    f <- lookupFun i
    mapM_ compileExp es
    emit ("invokestatic Main/" ++ f)
  EPIncr i -> do
    posicion <- lookupVar i
    emit ("iload " ++ show posicion)
    emit ("iinc " ++ show posicion ++ " " ++ show 1)
  EPDecr i -> do
    posicion <- lookupVar i
    emit ("iload " ++ show posicion)
    emit ("iinc " ++ show posicion ++ " " ++ show -1)
  EIncr i -> do
    posicion <- lookupVar i
    emit ("iinc " ++ show posicion ++ " " ++ show 1)
    emit ("iload " ++ show posicion)
  EDecr i -> do
    posicion <- lookupVar i
    emit ("iinc " ++ show posicion ++ " " ++ show -1)
    emit ("iload " ++ show posicion)

    -- estas cuatro falta para saber si son int o double

  ETimes e1 e2 -> do
    compileExp e1
    compileExp e2
    emit 
  EDiv e1 e2 -> undefined
  EPlus e1 e2 -> undefined
  EMinus e1 e2 -> undefined
  ELt e1 e2 -> undefined
  EGt e1 e2 -> undefined
  ELtEq e1 e2 -> undefined
  EGtEq e1 e2 -> undefined
  EEq e1 e2 -> undefined
  ENEq e1 e2 -> undefined
  EAnd e1 e2 -> undefined
  EOr e1 e2 -> undefined
  EAss i e -> undefined
  ETyped e t -> undefined

-- Hints: usefull auxiliary functions for comparations compilation
data Cmp = Equal | NEqual | Lt | Gt | Ge | Le
  deriving (Eq)

instance Show Cmp where
  show Equal  = "if_icmpeq "
  show NEqual = "if_icmpne "
  show Lt     = "if_icmplt "
  show Gt     = "if_icmpgt "
  show Ge     = "if_icmpge "
  show Le     = "if_icmple "

showDbl :: Cmp -> Instruction
showDbl Equal  = "ifeq "
showDbl NEqual = "ifne "
showDbl Lt     = "iflt "
showDbl Gt     = "ifgt "
showDbl Ge     = "ifge "
showDbl Le     = "ifle "

  



