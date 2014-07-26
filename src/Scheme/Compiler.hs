{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.Compiler where

-- import Control.Applicative
import Control.Monad.Reader hiding (ap, join)
-- import Control.Monad.Error
-- import Control.Monad.State
-- import Control.Monad.Writer
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
-- import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import LispMachine.Instructions
import LispMachine.Gen
import Scheme.Types
import Utils.RecursionSchemes

import Prelude hiding (div)

newtype Frame = Frame { getFrame :: Map Symbol Int }
              deriving (Show, Eq, Ord)

newtype Env = Env { getEnv :: [Frame] }
            deriving (Show, Eq, Ord)

type CompileM a = GenM Env a
-- newtype CompileM a = CompileM { runCompileM :: GenM Env a }
--                    deriving ( Functor
--                             , Applicative
--                             , Monad
--                             , MonadReader Env
--                             , MonadWriter [Statement]
--                             , MonadState GenState
--                             , MonadError String
--                             )

mkFrame :: [Symbol] -> Frame
mkFrame args = Frame $ M.fromList $ zip args [0..]

withFrameForArgs :: [Symbol] -> CompileM a -> CompileM a
withFrameForArgs args action = local (addFrame (mkFrame args)) action
  where
    addFrame frame (Env frames) = Env $ frame: frames

-- Returns Nth frame in environment chain where ref is bound and location
-- inside that frame.
resolveRef :: Symbol -> CompileM (Int, Int)
resolveRef ref = do
  frames <- asks getEnv
  return $ go frames frames 0
  where
    go :: [Frame] -> [Frame] -> Int -> (Int, Int)
    go frames []     n =
      error $ "unresolved reference " ++ (T.unpack $ getSymbol ref) ++
      ": no encsoling binding place found after checking " ++ show n ++ " frames\nframes: " ++ show frames
    go frames (f:fs) n =
      maybe (go frames fs $ n + 1) (\k -> (n, k)) $ M.lookup ref $ getFrame f

compileProg :: SchemeProg -> Either String Program
compileProg prog =
  genProgram (Env []) $ do
    compileFunc mainFunc
    mapM_ compileFunc otherFuncs
  where
    ([mainFunc], otherFuncs) = partition ((== Symbol "main") . defName) prog

-- (define (main world undocumented)
--   (cons initial-state (make-closure step)))
-- e.g.
-- (define (main world undocumented)
--   ;; compiler will load initial environment into closure
--   (cons (cons 0 world) (make-closure step)))

compileFunc :: Definition -> CompileM ()
-- compileFunc (Define (Symbol "main") [worldArg, undocumentedArg] body) = do
--   compileExpr body
--   rtn
compileFunc (Define name args body) = do
  label <- mkNamedLabel $ getSymbol name
  block label $
    withFrameForArgs args $
      mapM_ compileExpr body
  rtn

compileExpr :: Sexp -> CompileM ()
compileExpr = para alg
  where
    alg :: SexpF (CompileM (), Fix SexpF) -> CompileM ()
    alg (Lambda _ _)        = error "lambda not supported yet"
    alg (Cons (x, _) (y, _)) =
      x >> y >> cons
    alg (Car (x, _)) =
      x >> car
    alg (Cdr (x, _)) =
      x >> cdr
    alg (Add (x, _) (y, _)) =
      x >> y >>
        -- data stack must contain two items after this
        add
    alg (Sub (x, _) (y, _)) =
      x >> y >> sub
    alg (Mul (x, _) (y, _)) =
      x >> y >> mul
    alg (Div (x, _) (y, _)) =
      x >> y >> div
    alg (Assign ref (x, _)) = do
      x -- x must store to data stack!
      (n, k) <- resolveRef ref
      st n k
    alg (Let bindings body) = do
      -- handle let as usual in scheme:
      -- (let ((foo 1)
      --       (bar 2))
      --   (body foo bar))
      -- =>
      -- ((lambda (foo bar)
      --     (body foo bar))
      --  1
      --  2))
      -- TODO: move it into desugaring phase
      label <- mkNamedLabel "let_body"
      mapM_ (\(_var, (initExpr, _)) -> initExpr) bindings
      -- make closure and call it
      ldf label
      ap (length bindings)
      withFrameForArgs (map fst bindings) $ do
        block label $
          mapM_ fst body
      -- label <- mkNamedLabel $ getSymbol "let_body"
      -- withFrameForArgs (map fst bindings) $ do
      --   dum $ length bindings
      --   mapM_ (\(var, (initExpr, _)) -> initExpr >> resolveRef var >>= uncurry st)
      --         bindings
      --   block label
      --         body
    alg (If (c, _) (t, _) (f, _)) = do
      trueLabel <- mkNamedLabel "true_branch"
      falseLabel <- mkNamedLabel "false_branch"
      c
      sel trueLabel falseLabel
      block trueLabel (t >> join)
      block falseLabel (f >> join)
    alg (Cmp op (x, _) (y, _)) = do
      x
      y
      case op of
        CmpEq -> ceq
        CmpGt -> cgt
        CmpGe -> cgte
    alg (Reference var) = do
      (n, k) <- resolveRef var
      ld n k
    alg (Constant (Fix (LiteralInt n))) = do
      ldc n
    alg (Constant (Fix (LiteralBool b))) = do
      ldc $ if b then 1 else 0
    alg x = error $ show (fmap snd x) ++ " form not supported yet"
    -- alg (Cons x y)   = do
    --   tell []

