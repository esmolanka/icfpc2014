{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.Compiler where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Data.DList (DList)
import qualified Data.DList as DL
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import LispMachine.Instructions
import Scheme.Types
import Utils.RecursionSchemes

newtype LabelId = LabelId { getLabelId :: Int }
                deriving (Show, Eq, Ord)

newtype CompileM a = CompileM { runCompileM :: WriterT [Statement]
                                                       (State LabelId)
                                                       a }
                   deriving ( Functor
                            , Applicative
                            , Monad
                            , MonadWriter [Statement]
                            , MonadState LabelId
                            )

compileProg :: SchemeProg -> Program
compileProg prog = undefined
  where
    ([mainFunc], otherFuncs) = partition ((== Symbol "main") . defName) prog

    (_, stmts) = evalState (runWriterT $ do
                              compileFunc mainFunc
                              mapM_ compileFunc otherFuncs)
                           (LabelId 0)

-- (define (main world undocumented)
--   (cons initial-state (make-closure step)))
-- e.g.
-- (define (main world undocumented)
--   ;; compiler will load initial environment into closure
--   (cons (cons 0 world) (make-closure step)))

newtype Frame = Frame (Map Symbol Int)
              deriving (Show, Eq, Ord)
newtype Env = Env [Frame]
            deriving (Show, Eq, Ord)

compileFunc :: Definition -> CompileM ()
compileFunc (Define "main" [worldArg, undocumentedArg] body) = do
  compileExpr body
  tell [RTN]
compileFunc (Define name args body) = do
  tell [RTN]

compileExpr :: Sexp -> CompileM ()
compileExpr = cataM alg
  where
    alg :: SexpF (CompileM ()) -> CompileM ()
    alg (Lambda _ _) = error "lambda not supported yet"
    alg x = error $ show x ++ " not supported yet"
    -- alg (Cons x y)   = do
    --   tell []

