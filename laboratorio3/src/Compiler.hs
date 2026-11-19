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
newLabel  = undefined

lookupFun :: Id -> State Env FunType
lookupFun i = undefined

lookupVar :: Id -> State Env Int
lookupVar i = undefined

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
  SExp exp -> undefined
  SDecls t ids -> undefined
  SInit t i e -> undefined
  SReturn e -> undefined
  SReturnVoid -> undefined
  SWhile e s -> undefined
  SBlock sts -> undefined
  SIfElse e st1 st2 -> undefined


compileExp :: Exp -> State Env ()
compileExp exp = case exp of
  ETrue -> undefined
  EFalse -> undefined
  EInt i -> undefined
  EDouble d -> undefined
  EString s -> undefined
  EId i -> undefined
  EApp i es -> undefined
  EPIncr i -> undefined
  EPDecr i -> undefined
  EIncr i -> undefined
  EDecr i -> undefined
  ETimes e1 e2 -> undefined
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

  



