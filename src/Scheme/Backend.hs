{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Scheme.Backend where

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader hiding (ap, join)
import Control.Monad.Error (throwError)
-- import Control.Monad.State
-- import Control.Monad.Writer
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
-- import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import LispMachine.Instructions
import LispMachine.Gen
import Scheme.Frontend
import Scheme.Types
import Utils.RecursionSchemes

import Prelude hiding (div)

newtype Frame = Frame { getFrame :: Map Symbol Int }
              deriving (Show, Eq, Ord)

newtype Env = Env { getEnv :: [Frame] }
            deriving (Show, Eq, Ord)

newtype Calls = Calls { getCalls :: [(Symbol, Int, RRef)] }
              deriving (Show, Eq, Ord)

data CompileEnv = CompileEnv
                { variableEnv :: Env
                , functionEnv :: Map Symbol (DefinitionF RRef)
                , constantEnv :: Map Symbol Sexp
                , callEnv     :: Calls
                }
                deriving (Show, Eq, Ord)

type CompileM a = GenM CompileEnv a

mkFrame :: [Symbol] -> Frame
mkFrame args = Frame $ M.fromList $ zip args [0..]

withCall :: Symbol -> Int -> RRef -> CompileM a -> CompileM a
withCall name argcount lbl action = local addCall action
  where
    addCall env@(CompileEnv _ _ _ (Calls calls)) =
        env { callEnv = Calls ((name, argcount, lbl) : calls)}

withFrameForArgs :: [Symbol] -> CompileM a -> CompileM a
withFrameForArgs args action = local (addFrame (mkFrame args)) action
  where
    addFrame frame compileEnv@(CompileEnv (Env frames) _ _ _) =
      compileEnv { variableEnv = (Env $ frame: frames) }

functionLabel :: Symbol -> CompileM RRef
functionLabel name = do
  funcEnv <- asks functionEnv
  maybe (error $ "unresolved function name: " ++ show (getSymbol name))
        (return . defBody) $
    M.lookup name funcEnv

functionArgumentCount :: Symbol -> CompileM (Maybe Int)
functionArgumentCount name = do
  funcEnv <- asks functionEnv
  return $ length . defArgs <$> M.lookup name funcEnv

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
      ": no enclosing binding place found after checking " ++ show n ++ " frames\nframes: " ++ show frames
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
    labels <- mapM (\func -> let name = defName func
                             in (\label -> (name, fmap (const label) func)) <$>
                             mkNamedLabel (getSymbol name))
                   otherFuncs
    let funcEnv = M.fromList labels
    local (\compEnv -> compEnv { functionEnv = funcEnv, constantEnv = constEnv }) $ do
      compileFunc mainFunc
      mapM_ compileFunc otherFuncs
  where
    initialEnv = CompileEnv (Env [])
                            (error "funcEnv not initialized")
                            (error "constantEnv not initialized")
                            (Calls [])
    (constants, funcs)       = partition defIsConstant prog
    funcs'                   = map (defDict M.!) $ S.toList $ allUsedFunctions funcs
    ([mainFunc], otherFuncs) = partition ((== Symbol "main") . defName) funcs'
    defDict                  = M.fromList $ map (defName &&& id) prog
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
compileFunc (Define name@(Symbol "main") args body _ _) = do
  withCall name 2 (RRef "main" 0) $ withFrameForArgs args $ do
    mapM_ (compileExpr name) body
    rtn
compileFunc (Define name args body _isInlinable isConst) = do
  when isConst $ error $ "cannot compile constant as a function: " ++ show (getSymbol name)
  -- label <- mkNamedLabel $ getSymbol name
  label <- functionLabel name
  block label $
    withCall name (length args) label $ withFrameForArgs args $ do
      mapM_ (compileExpr name) body
      rtn

compileExpr :: Symbol -> Sexp -> CompileM ()
compileExpr enclosingFunc = para alg
  where
    alg :: SexpF (CompileM (), Fix SexpF) -> CompileM ()
    alg (Lambda args body) = do
      label <- mkNamedLabel "lam"
      ldf label
      withFrameForArgs args
        $ withCall (Symbol "<lambda>") (length args) label
        $ standaloneBlock label
        $ mapM_ fst body >> rtn

    alg (Cons (x, _) (y, _)) =
      x >> y >> cons
    alg (Car (x, _)) =
      x >> car
    alg (Cdr (x, _)) =
      x >> cdr
    alg (Add (x, _) (y, _)) =
      x >> y >> add
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
      mapM_ (\(var, (initExpr, _)) -> annotate ("let " ++ (T.unpack $ getSymbol var)) >> initExpr) bindings
      -- make closure and call it
      ldf label
      annotate "to let body" >> ap (length bindings)
      withFrameForArgs (map fst bindings) $
        standaloneBlock label $ do
         mapM_ fst body
         rtn

    alg (LetRec bindings body) = do
      label <- mkNamedLabel "letrec_body"
      withFrameForArgs (map fst bindings) $ do
        dum (length bindings)
        mapM_ (\(var, (initExpr, _)) -> annotate ("letrec " ++ (T.unpack $ getSymbol var)) >> initExpr) bindings
        -- make closure and call it
        ldf label
        annotate "to letrec body" >> rap (length bindings)
        standaloneBlock label $ do
          mapM_ fst body
          rtn

    alg (If (c, _) (t, tExp) (f, fExp)) = do
      trueLabel <- mkNamedLabel $ getSymbol enclosingFunc `T.append` "_true_br"
      falseLabel <- mkNamedLabel $ getSymbol enclosingFunc `T.append` "_false_br"
      annotate "condition" >> c
      case (isTailOp tExp, isTailOp fExp) of
        (True, True) -> do
          tsel trueLabel falseLabel
          standaloneBlock trueLabel  t
          standaloneBlock falseLabel f
        (False, True) -> do
          tsel trueLabel falseLabel
          standaloneBlock trueLabel  (t >> rtn)
          standaloneBlock falseLabel f
        (True, False) -> do
          tsel trueLabel falseLabel
          standaloneBlock trueLabel  t
          standaloneBlock falseLabel (f >> rtn)
        (False, False) -> do
          sel trueLabel falseLabel
          standaloneBlock trueLabel  (t >> join)
          standaloneBlock falseLabel (f >> join)
      where
        isTailOp (Fix (TailCall _ _)) = True
        isTailOp (Fix (TailExit _))   = True
        isTailOp _ = False

    alg (Cmp op (x, _) (y, _)) =
        x >> y >> case op of
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

    alg form@(Call (x, xExpr) args) = do
      mapM_ (\(a,_) -> annotate "arg" >> a) args
      case xExpr of
        Fix (Reference name) -> do
          argcount <- functionArgumentCount name
          case argcount of
            Just count
              | count /= length args ->
                throwError $ "Function " ++ show (getSymbol name) ++ " expects " ++ show argcount ++
                 " arguments: \n" ++ show (fmap snd form)
              | otherwise -> x
            _ -> x
        Fix (Lambda largs _) -> do
          let argcount = length largs
          when (length args /= argcount) $
               throwError $ "Lambda expects " ++ show argcount ++
                              " arguments: \n" ++ show (fmap snd form)
          x
        _ -> x -- x must add closure cell on the stack
      annotate "call"
      ap $ length args

    alg (TailCall clos args) = do
      mapM_ fst args
      (n,i) <- resolveVar clos
      ld n i
      annotate ("jump to " ++ T.unpack (getSymbol clos)) >> tap (length args)

    alg (TailExit res) = do
      fst res
      rtn

    alg (Debug (x, _)) =
      annotate "traced value" >> x >> dbug

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
            resolveConstant name >>= compileExpr enclosingFunc
          | otherwise -> do
            frames <- asks (getEnv . variableEnv)
            throwError $ "unresolved reference " ++ show (getSymbol name) ++
              " while compiling function " ++ show (getSymbol enclosingFunc) ++ "\nframes:\n" ++
              intercalate "\n" (showFrames frames)
      where
        showFrames = map (\frame -> intercalate "\n" $
                          map (\(name, n) -> "    " ++ show (getSymbol name) ++ " " ++ show n) $
                          M.toList $
                          getFrame frame)
    alg (Constant (LiteralInt n)) = do
      ldc n

    alg (Constant (LiteralBool b)) = do
      ldc $ if b then 1 else 0

    alg form@(Constant (LiteralClosure _)) = throwError $ show (fmap snd form) ++ " form not supported yet"

    alg form@(And _ _)                     = throwError $ show (fmap snd form) ++ " form must be desugared"
    alg form@(Or _ _)                      = throwError $ show (fmap snd form) ++ " form must be desugared"
    alg form@(Not _)                       = throwError $ show (fmap snd form) ++ " form must be desugared"
    alg form@(Cond _)                      = throwError $ show (fmap snd form) ++ " form must be desugared"
    alg form@(LetStar _ _)                 = throwError $ show (fmap snd form) ++ " form must be desugared"

