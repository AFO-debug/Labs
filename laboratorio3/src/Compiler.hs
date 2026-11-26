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
    let t = case e of
          ETyped _ tipo -> tipo
          _ -> error "Return expression without type annotation"
    case t of
      Type_bool   -> emit "ireturn"
      Type_int    -> emit "ireturn"
      Type_double -> emit "dreturn"
      Type_string -> emit "areturn"
      _           -> error "Invalid return type"
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

compileExp :: Exp -> State Env ()

compileExp (ETyped ETrue _) = emit "iconst_1"

compileExp (ETyped EFalse _) = emit "iconst_0"

compileExp (ETyped (EInt i) _) = emit ("ldc " ++ show i)

compileExp (ETyped (EDouble d) _) = emit ("ldc2_w " ++ show d)

compileExp (ETyped (EString s) _) = emit ("ldc " ++ show s)

compileExp (ETyped (EId i) t) = do
  posicion <- lookupVar i
  case t of
    Type_bool   -> emit ("iload " ++ show posicion)
    Type_int    -> emit ("iload " ++ show posicion)
    Type_double -> emit ("dload " ++ show posicion)
    Type_string -> emit ("aload " ++ show posicion)
    _ -> error "Invalid type for variable"

compileExp (ETyped (EApp i es) _) = do
  f <- lookupFun i
  mapM_ compileExp es
  emit ("invokestatic Main/" ++ f)

compileExp (ETyped (EPIncr i) t) = do
  posicion <- lookupVar i
  case t of
    Type_int -> do
      emit ("iload " ++ show posicion)
      emit ("iinc " ++ show posicion ++ " 1")

    Type_double -> do
      emit ("dload " ++ show posicion)
      emit "dup2"
      emit "dconst_1"
      emit "dadd"
      emit ("dstore " ++ show posicion)

    _ -> error "Invalid type for post-increment"

compileExp (ETyped (EPDecr i) t) = do
  posicion <- lookupVar i
  case t of
    Type_int -> do
      emit ("iload " ++ show posicion)
      emit ("iinc " ++ show posicion ++ " -1")

    Type_double -> do
      emit ("dload " ++ show posicion)
      emit "dup2"
      emit "dconst_1"
      emit "dsub"
      emit ("dstore " ++ show posicion)

    _ -> error "Invalid type for post-decrement"

compileExp (ETyped (EIncr i) t) = do
  posicion <- lookupVar i
  case t of
    Type_int -> do
      emit ("iinc " ++ show posicion ++ " 1")
      emit ("iload " ++ show posicion)

    Type_double -> do
      emit ("dload " ++ show posicion)
      emit "dconst_1"
      emit "dadd"
      emit "dup2"
      emit ("dstore " ++ show posicion)

    _ -> error "Invalid type for pre-increment"

compileExp (ETyped (EDecr i) t) = do
  posicion <- lookupVar i
  case t of
    Type_int -> do
      emit ("iinc " ++ show posicion ++ " -1")
      emit ("iload " ++ show posicion)

    Type_double -> do
      emit ("dload " ++ show posicion)
      emit "dconst_1"
      emit "dsub"
      emit "dup2"
      emit ("dstore " ++ show posicion)

    _ -> error "Invalid type for pre-decrement"

compileExp (ETyped (ETimes e1 e2) t) = do
  compileExp e1
  compileExp e2
  case t of
    Type_int    -> emit "imul"
    Type_double -> emit "dmul"
    _ -> error "Invalid type for multiplication"

compileExp (ETyped (EDiv e1 e2) t) = do
  compileExp e1
  compileExp e2
  case t of
    Type_int    -> emit "idiv"
    Type_double -> emit "ddiv"
    _ -> error "Invalid type for division"

compileExp (ETyped (EPlus e1 e2) t) = do
  compileExp e1
  compileExp e2
  case t of
    Type_int    -> emit "iadd"
    Type_double -> emit "dadd"
    Type_string ->
      emit "invokestatic runtime/plusString(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"
    _ -> error "Invalid type for addition"

compileExp (ETyped (EMinus e1 e2) t) = do
  compileExp e1
  compileExp e2
  case t of
    Type_int    -> emit "isub"
    Type_double -> emit "dsub"
    _ -> error "Invalid type for subtraction"

compileExp (ETyped (ELt e1 e2) _) = do
  compileExp e1
  compileExp e2

  tipo <- case e1 of
    ETyped _ t -> return t
    _ -> error "Expression without type annotation"

  verdadero <- newLabel
  fin <- newLabel

  case tipo of
    Type_int -> do
      emit ("if_icmplt " ++ verdadero)

    Type_double -> do
      emit "dcmpg"
      emit ("iflt " ++ verdadero)

    _ -> error "Invalid type for less-than comparison"

  emit "iconst_0"
  emit ("goto " ++ fin)
  emit (verdadero ++ ":")
  emit "iconst_1"
  emit (fin ++ ":")

compileExp (ETyped (EGt e1 e2) _) = do
  compileExp e1
  compileExp e2

  tipo <- case e1 of
    ETyped _ t -> return t
    _ -> error "Expression without type annotation"

  verdadero <- newLabel
  fin <- newLabel

  case tipo of
    Type_int -> do
      emit ("if_icmpgt " ++ verdadero)

    Type_double -> do
      emit "dcmpl"
      emit ("ifgt " ++ verdadero)

    _ -> error "Invalid type for greater-than comparison"

  emit "iconst_0"
  emit ("goto " ++ fin)
  emit (verdadero ++ ":")
  emit "iconst_1"
  emit (fin ++ ":")

compileExp (ETyped (ELtEq e1 e2) _) = do
  compileExp e1
  compileExp e2

  tipo <- case e1 of
    ETyped _ t -> return t
    _ -> error "Expression without type annotation"

  verdadero <- newLabel
  fin <- newLabel

  case tipo of
    Type_int -> do
      emit ("if_icmple " ++ verdadero)

    Type_double -> do
      emit "dcmpg"
      emit ("ifle " ++ verdadero)

    _ -> error "Invalid type for less-than-or-equal comparison"

  emit "iconst_0"
  emit ("goto " ++ fin)
  emit (verdadero ++ ":")
  emit "iconst_1"
  emit (fin ++ ":")

compileExp (ETyped (EGtEq e1 e2) _) = do
  compileExp e1
  compileExp e2

  tipo <- case e1 of
    ETyped _ t -> return t
    _ -> error "Expression without type annotation"

  verdadero <- newLabel
  fin <- newLabel

  case tipo of
    Type_int -> do
      emit ("if_icmpge " ++ verdadero)

    Type_double -> do
      emit "dcmpl"
      emit ("ifge " ++ verdadero)

    _ -> error "Invalid type for greater-than-or-equal comparison"

  emit "iconst_0"
  emit ("goto " ++ fin)
  emit (verdadero ++ ":")
  emit "iconst_1"
  emit (fin ++ ":")

compileExp (ETyped (EEq e1 e2) _) = do
  compileExp e1
  compileExp e2

  tipo <- case e1 of
    ETyped _ t -> return t
    _ -> error "Expression without type annotation"

  verdadero <- newLabel
  fin <- newLabel

  case tipo of
    Type_int -> do
      emit ("if_icmpeq " ++ verdadero)

    Type_bool -> do
      emit ("if_icmpeq " ++ verdadero)

    Type_double -> do
      emit "dcmpg"
      emit ("ifeq " ++ verdadero)

    _ -> error "Invalid type for equality comparison"

  emit "iconst_0"
  emit ("goto " ++ fin)
  emit (verdadero ++ ":")
  emit "iconst_1"
  emit (fin ++ ":")

compileExp (ETyped (ENEq e1 e2) _) = do
  compileExp e1
  compileExp e2

  tipo <- case e1 of
    ETyped _ t -> return t
    _ -> error "Expression without type annotation"

  verdadero <- newLabel
  fin <- newLabel

  case tipo of
    Type_int -> do
      emit ("if_icmpne " ++ verdadero)

    Type_bool -> do
      emit ("if_icmpne " ++ verdadero)

    Type_double -> do
      emit "dcmpg"
      emit ("ifne " ++ verdadero)

    _ -> error "Invalid type for inequality comparison"

  emit "iconst_0"
  emit ("goto " ++ fin)
  emit (verdadero ++ ":")
  emit "iconst_1"
  emit (fin ++ ":")

compileExp (ETyped (EAnd e1 e2) _) = do
  compileExp e1
  compileExp e2
  emit "iand"

compileExp (ETyped (EOr e1 e2) _) = do
  compileExp e1
  compileExp e2
  emit "ior"

compileExp (ETyped (EAss i e) t) = do
  posicion <- lookupVar i
  compileExp e

  case t of
    Type_int -> do
      emit "dup"
      emit ("istore " ++ show posicion)

    Type_bool -> do
      emit "dup"
      emit ("istore " ++ show posicion)

    Type_double -> do
      emit "dup2"
      emit ("dstore " ++ show posicion)

    Type_string -> do
      emit "dup"
      emit ("astore " ++ show posicion)

    _ -> error "Invalid type for assignment"

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

  



