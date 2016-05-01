{-# LANGUAGE OverloadedStrings #-}

module Emit where

import Debug.Trace

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map
import JIT
import Utils

import Codegen
import qualified Syntax as S

one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)
false = zero
true = one

astTypeFromString :: String -> AST.Type
astTypeFromString varType
    | varType == "Double" = double
    | otherwise           = emptyType

varDefName :: S.Expr -> String
varDefName (S.VarDef varName _) = varName

varDefType :: S.Expr -> AST.Type
varDefType (S.VarDef _ varType) = astTypeFromString varType

toSig :: [S.Expr] -> [(AST.Type, AST.Name)]
toSig = map (\varDef -> (varDefType varDef, AST.Name (varDefName varDef)))

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \varDef -> do
        let varName = varDefName varDef
        var <- alloca double
        store var (local (AST.Name varName))
        assign varName var
      cgen body >>= ret

codegenTop (S.Extern name args) = do
  external double name fnargs
  where fnargs = toSig args

codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]

-- Example AST
-- [When [Clause (BinaryOp "<" (Var "x") (Float 3.0)) (Float 1.0),Clause Else (BinaryOp "+" (Call "fib" [BinaryOp "-" (Var "x") (Float 1.0)]) (Call "fib" [BinaryOp "-" (Var "x") (Float 2.0)]))]]
cgen :: S.Expr -> Codegen AST.Operand
--cgen a | trace ("cgen " ++ show a) False = undefined
cgen (S.UnaryOp op a) = do
  cgen $ S.Call ("unary" ++ op) [a]
cgen (S.BinaryOp "=" (S.Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval
cgen (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (S.Var x) = getvar x >>= load
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs
cgen (S.When clauses) = do
    entryBlock <- entry
    exitBlock <- addBlock "case.exit"

    -- Throw error if else clause is zero or greater than 1
    -- Else should always be last
    let elseClause = last clauses
    let otherClauses = reverse (init clauses)

    elsePhi <- processElse exitBlock elseClause

    descendants <- scanM (processClause exitBlock) elsePhi otherClauses

    setBlock entryBlock
    let firstBlock = snd (last descendants)
    br firstBlock

    setBlock exitBlock

    phi double (map fst descendants)


processElse :: AST.Name -> S.Expr -> Codegen ((AST.Operand, AST.Name), AST.Name)
processElse exitBlock (S.Clause S.Else code) = do
    elseBlock <- addBlock "case.else"

    setBlock elseBlock
    val <- cgen code
    br exitBlock
    elseBlock <- getBlock

    return ((val, elseBlock), elseBlock)

processClause :: AST.Name -> ((AST.Operand, AST.Name), AST.Name) -> S.Expr -> Codegen ((AST.Operand, AST.Name), AST.Name)
processClause exitBlock nextPhi (S.Clause cond code) = do
    let nextBlock = snd nextPhi

    caseTestBlock <- addBlock "case.test.block"
    caseCodeBlock <- addBlock "case.code.block"

    setBlock caseTestBlock
    cond <- cgen cond
    test <- fcmp FP.ONE false cond
    cbr test caseCodeBlock nextBlock
    caseTestBlock <- getBlock

    setBlock caseCodeBlock
    val <- cgen code       -- Generate code for the condition
    br exitBlock
    caseCodeBlock <- getBlock

    return ((val, caseCodeBlock), caseTestBlock)


-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = do
  res <- runJIT oldast
  case res  of
    Right newast -> return newast
    Left err     -> putStrLn err >> return oldast
  where
    modn    = mapM codegenTop fns
    oldast  = runLLVM mod modn