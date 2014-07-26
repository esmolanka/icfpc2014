{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Scheme.Backend where

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader hiding (ap, join)
-- import Control.Monad.Error
-- import Control.Monad.State
-- import Control.Monad.Writer
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
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

data CompileEnv = CompileEnv
                { variableEnv :: Env
                , functionEnv :: Map Symbol RRef
                , constantEnv :: Map Symbol Sexp
                }
                deriving (Show, Eq, Ord)

type CompileM a = GenM CompileEnv a
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
    addFrame frame compileEnv@(CompileEnv (Env frames) _ _) =
      compileEnv { variableEnv = (Env $ frame: frames) }

functionLabel :: Symbol -> CompileM RRef
functionLabel name = do
  funcEnv <- asks functionEnv
  maybe (error $ "unresolved function name: " ++ show (getSymbol name)) return $
    M.lookup name funcEnv

resolveConstant :: Symbol -> CompileM Sexp
resolveConstant name = do
  constEnv <- asks constantEnv
  maybe (error $ "unresolved constant: " ++ show name) return $
    M.lookup name constEnv

-- Returns Nth frame in environment chain where ref is bound and location
-- inside that frame.
resolveVar :: Symbol -> CompileM (Int, Int)
resolveVar ref = do
  frames <- asks (getEnv . variableEnv)
  return $ go frames frames 0
  where
    go :: [Frame] -> [Frame] -> Int -> (Int, Int)
    go frames []     n =
      error $ "unresolved reference " ++ (T.unpack $ getSymbol ref) ++
      ": no encsoling binding place found after checking " ++ show n ++ " frames\nframes: " ++ show frames
    go frames (f:fs) n =
      maybe (go frames fs $ n + 1) (\k -> (n, k)) $ M.lookup ref $ getFrame f

isVar :: Symbol -> CompileM Bool
isVar name = do
  frames <- asks (getEnv . variableEnv)
  return $ any (M.member name . getFrame) frames

isFunc :: Symbol -> CompileM Bool
isFunc name = M.member name <$> asks functionEnv

isConstant :: Symbol -> CompileM Bool
isConstant name = M.member name <$> asks constantEnv

compileProg :: SchemeProg -> Either String Program
compileProg prog =
  genProgram initialEnv $ do
    labels <- mapM (\name -> (name, ) <$> mkNamedLabel (getSymbol name)) funcNames
    let funcEnv = M.fromList labels
    local (\compEnv -> compEnv { functionEnv = funcEnv, constantEnv = constEnv }) $ do
      compileFunc mainFunc
      mapM_ compileFunc funcs
  where
    initialEnv = CompileEnv (Env [])
                            (error "funcEnv not initialized")
                            (error "constantEnv not initialized")
    ([mainFunc], otherNames) = partition ((== Symbol "main") . defName) prog
    (constants, funcs)       = partition defIsConstant otherNames
    funcNames                = map defName funcs
    constEnv                 = M.fromList $ map (defName &&& getBody) constants
      where
        getBody :: Definition -> Sexp
        getBody def = case defBody def of
                        [x] -> x
                        xs  -> Fix $ Begin xs

-- (define (main world undocumented)
--   (cons initial-state (make-closure step)))
-- e.g.
-- (define (main world undocumented)
--   ;; compiler will load initial environment into closure
--   (cons (cons 0 world) (make-closure step)))

compileFunc :: Definition -> CompileM ()
compileFunc (Define (Symbol "main") args body _ _) = do
  withFrameForArgs args $ do
    mapM_ compileExpr body
    rtn
compileFunc (Define name args body _isInlinable isConst) = do
  when isConst $ error $ "cannot compile constant as a function: " ++ show (getSymbol name)
  -- label <- mkNamedLabel $ getSymbol name
  label <- functionLabel name
  block label $
    withFrameForArgs args $ do
      mapM_ compileExpr body
      rtn

compileExpr :: Sexp -> CompileM ()
compileExpr = para alg
  where
    alg :: SexpF (CompileM (), Fix SexpF) -> CompileM ()
    alg (Lambda _ _) = error "lambda not supported yet"
    alg (Cons (x, _) (y, _)) =
      x >> y >> cons
    alg (Car (x, _)) =
      x >> car
    alg (Cdr (x, _)) =
      x >> cdr
    alg (Add (x, _) (y, _)) =
      x >> y >> -- data stack must contain two items after this!
        add
    alg (Sub (x, _) (y, _)) =
      x >> y >> sub
    alg (Mul (x, _) (y, _)) =
      x >> y >> mul
    alg (Div (x, _) (y, _)) =
      x >> y >> div
    alg (Assign ref (x, _)) = do
      x -- x must store to data stack!
      (n, k) <- resolveVar ref
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
      withFrameForArgs (map fst bindings) $
        standaloneBlock label $ do
         mapM_ fst body
         rtn
    alg (If (c, _) (t, _) (f, _)) = do
      trueLabel <- mkNamedLabel "true_br"
      falseLabel <- mkNamedLabel "false_br"
      c
      sel trueLabel falseLabel
      standaloneBlock trueLabel (t >> join)
      standaloneBlock falseLabel (f >> join)
    alg (Cmp op (x, _) (y, _)) = do
      x
      y
      case op of
        CmpEq -> ceq
        CmpGt -> cgt
        CmpGe -> cgte
    alg (IsAtom (x, _)) =
      x >> atom
    alg (List xs) =
      foldr (\h t -> h >> t >> cons) (ldc 0) $ map fst xs
    alg (Begin xs) =
      sequence_ $ map fst xs
    alg (MakeClosure name) =
      functionLabel name >>= ldf
    alg (Call (x, _) args) = do
      mapM_ fst args
      x -- x must add closure cell on the stack
      ap $ length args
    alg (Debug (x, _)) =
      x >> dbug
    alg (Break) = brk
    alg (Reference name) = do
      var      <- isVar name
      func     <- isFunc name
      constant <- isConstant name
      -- structured-haskell-mode cannot handle multiway ifs, inconvenient
      case () of
        _ | var -> do
              (n, k) <- resolveVar name
              ld n k
          | func -> do
            functionLabel name >>= ldf
          | constant ->
            resolveConstant name >>= compileExpr
          | otherwise -> error $ "unresolved reference: " ++ show (getSymbol name)
    alg (Constant (LiteralInt n)) = do
      ldc n
    alg (Constant (LiteralBool b)) = do
      ldc $ if b then 1 else 0
    alg form@(TailCall _ _)                = error $ show (fmap snd form) ++ " form not supported yet"
    alg form@(Constant (LiteralClosure _)) = error $ show (fmap snd form) ++ " form not supported yet"
    alg form@(And _ _)                     = error $ show (fmap snd form) ++ " form must be desugared"
    alg form@(Or _ _)                      = error $ show (fmap snd form) ++ " form must be desugared"
    alg form@(Not _)                       = error $ show (fmap snd form) ++ " form must be desugared"
    alg form@(Cond _)                      = error $ show (fmap snd form) ++ " form must be desugared"
    -- alg x = error $ show (fmap snd x) ++ " form not supported yet"
    -- alg (Cons x y)   = do
    --   tell []

