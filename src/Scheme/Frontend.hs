{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}

module Scheme.Frontend (parseSexp, desugar, optimize) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char
import qualified Data.Foldable as F
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

-- import Data.Sexp (List, Atom)
import qualified Data.Sexp as S
import Language.Sexp.Parser (parse)

import Scheme.Types
import Utils.RecursionSchemes

bsToText :: ByteString -> Text
bsToText = T.pack . BS.unpack

mkSymbol :: ByteString -> Symbol
mkSymbol = Symbol . bsToText

parseSexp :: Text -> Either String SchemeProg
parseSexp input =
  either Left validateSchemeProg $
  either (\(msg, rest) -> Left $ msg ++ BS.unpack rest) (Right . map mkDef) $
  parse (BS.pack $ T.unpack input)
  where
    atomToSymbol :: S.Sexp -> Symbol
    atomToSymbol (S.Atom x) = mkSymbol x
    atomToSymbol list         =
      error $ "atomToSymbol: expected symbol but got list: " ++ show list

    mkDef :: S.Sexp -> Definition
    mkDef = fmap (map mkExpr) . defCoalg

    defCoalg :: S.Sexp -> DefinitionF [S.Sexp]
    defCoalg form@(S.List (S.Atom "define": rest)) =
      case rest of
        -- constant, but looks like
        S.Atom name: body
          | inlinableName name -> Define (mkSymbol name)
                                            []
                                            body
                                            True
                                            True
          | otherwise -> error "non-inlinable toplevel constants are not supported"
        -- function
        S.List (S.Atom name: args): body -> Define (mkSymbol name)
                                                   (map atomToSymbol args)
                                                   body
                                                   (inlinableName name)
                                                   False
        _ -> error $ "invalid define form: " ++ show form
      where
        inlinableName :: ByteString -> Bool
        inlinableName x = BS.length x > 1 && BS.head x == '+' && BS.last x == '+'
    defCoalg form = error $ "expected define form instead of: " ++ show form

    mkExpr :: S.Sexp -> Sexp
    mkExpr = ana coalg

    coalg :: S.Sexp -> SexpF S.Sexp
    coalg form@(S.List (S.Atom "lambda": rest)) =
      case rest of
        S.List args: body -> Lambda (map atomToSymbol args)
                                    body
        _ -> error $ "invalid lambda form: " ++ show form
    coalg form@(S.List (S.Atom "cons": rest)) =
      case rest of
        [x, y] -> Cons x y
        _ -> error $ "invalid cons form: " ++ show form
    coalg form@(S.List (S.Atom "car": rest)) =
      case rest of
        [x] -> Car x
        _ -> error $ "invalid car form: " ++ show form
    coalg form@(S.List (S.Atom "cdr": rest)) =
      case rest of
        [x] -> Cdr x
        _ -> error $ "invalid cdr form: " ++ show form
    coalg form@(S.List (S.Atom "+": rest)) =
      case rest of
        [x, y] -> Add x y
        _ -> error $ "invalid + form: " ++ show form
    coalg form@(S.List (S.Atom "*": rest)) =
      case rest of
        [x, y] -> Mul x y
        _ -> error $ "invalid * form: " ++ show form
    coalg form@(S.List (S.Atom "-": rest)) =
      case rest of
        [x, y] -> Sub x y
        _ -> error $ "invalid - form: " ++ show form
    coalg form@(S.List (S.Atom "/": rest)) =
      case rest of
        [x, y] -> Div x y
        _ -> error $ "invalid / form: " ++ show form
    coalg form@(S.List (S.Atom "set!": rest)) =
      case rest of
        [S.Atom x, y] -> Assign (mkSymbol x) y
        _ -> error $ "invalid / form: " ++ show form
    coalg form@(S.List (S.Atom "let": rest)) =
      case rest of
        S.List bindings: body ->
          Let (map analyzeBinding bindings) body
        _ -> error $ "invalid let form: " ++ show form
      where
        analyzeBinding :: S.Sexp -> (Symbol, S.Sexp)
        analyzeBinding (S.List [S.Atom x, y]) = (mkSymbol x, y)
        analyzeBinding b = error $ "invalid let binding: " ++ show b
    coalg form@(S.List (S.Atom "let*": rest)) =
      case rest of
        S.List bindings: body ->
          LetStar (map analyzeBinding bindings) body
        _ -> error $ "invalid let* form: " ++ show form
      where
        analyzeBinding :: S.Sexp -> (Symbol, S.Sexp)
        analyzeBinding (S.List [S.Atom x, y]) = (mkSymbol x, y)
        analyzeBinding b = error $ "invalid let* binding: " ++ show b
    coalg form@(S.List (S.Atom "and": rest)) =
      case rest of
        [x, y] -> And x y
        _ -> error $ "invalid and form: " ++ show form
    coalg form@(S.List (S.Atom "or": rest)) =
      case rest of
        [x, y] -> Or x y
        _ -> error $ "invalid or form: " ++ show form
    coalg form@(S.List (S.Atom "not": rest)) =
      case rest of
        [x] -> Not x
        _ -> error $ "invalid not form: " ++ show form
    coalg form@(S.List (S.Atom "cond": rest)) =
      Cond $ map extractCondCase rest
      where
        extractCondCase (S.List xs) =
          case xs of
            test: body -> (test, body)
            []         -> error $ "invalid cond form: " ++ show form
        extractCondCase _           = error $ "invalid cond form: " ++ show form
    coalg form@(S.List (S.Atom "if": rest)) =
      case rest of
        [x, y, z] -> If x y z
        _ -> error $ "invalid if form: " ++ show form
    coalg form@(S.List (S.Atom "==": rest)) =
      case rest of
        [x, y] -> Cmp CmpEq x y
        _ -> error $ "invalid == form: " ++ show form
    coalg form@(S.List (S.Atom ">": rest)) =
      case rest of
        [x, y] -> Cmp CmpGt x y
        _ -> error $ "invalid > form: " ++ show form
    coalg form@(S.List (S.Atom ">=": rest)) =
      case rest of
        [x, y] -> Cmp CmpGe x y
        _ -> error $ "invalid == form: " ++ show form
    coalg form@(S.List (S.Atom "atom?": rest)) =
      case rest of
        [x] -> IsAtom x
        _ -> error $ "invalid atom? form: " ++ show form
    coalg (S.List (S.Atom "list": rest)) =
      List rest
    coalg form@(S.List (S.Atom "begin": rest)) =
      -- Begin rest
      case rest of
        _ :_ -> Begin rest
        [] -> error $ "invalid begin form, at least one body statement expected: " ++ show form
    coalg form@(S.List (S.Atom "make-closure": rest)) =
      case rest of
        [S.Atom x] -> MakeClosure $ mkSymbol x
        _ -> error $ "invalid make-closure form: " ++ show form
    coalg form@(S.List (S.Atom "debug": rest)) =
      case rest of
        [x] -> Debug x
        _ -> error $ "invalid debug form: " ++ show form
    coalg form@(S.List (S.Atom "break": rest)) =
      case rest of
        [] -> Break
        _ -> error $ "invalid break form: " ++ show form
    coalg form@(S.List (S.Atom "if-then-recur": rest)) =
        case rest of
          [cond, retval, S.List args] -> Recur cond retval args
          _ -> error $ "invalid if-then-recur form: " ++ show form
    coalg (S.List (func: rest)) =
        Call func rest
    coalg (S.List []) = error "empty list"

    coalg (S.Atom "#t") = constTrue
    coalg (S.Atom "#f") = constFalse
    coalg (S.Atom name)
      | BS.all isDigit name = Constant $ LiteralInt $ read $ BS.unpack name
      | otherwise = Reference $ mkSymbol name

validateSchemeProg :: SchemeProg -> Either String SchemeProg
validateSchemeProg []   = Left "no functions defined in program"
validateSchemeProg prog
  | any ((> 1) . length) groupedNames = Left $ "functions " ++ intercalate ", " redefinedNames ++
                                        " are defined more than once"
  | not $ elem (Symbol "main") names  = Left "progam does not define main function"
  | Just (Define _ args _ _ _) <- mainFunc,
    length args /= 2 = Left "main function should take 2 arguments: the initial state of the world and a dummy one."
  | Just (Define _ _ _ _ isConstant) <- mainFunc,
    isConstant = Left "main function should not be defined as constant"
  | otherwise                         = Right prog
  where
    names        = map defName prog
    groupedNames :: [[Symbol]]
    groupedNames = group names
    redefinedNames = map (T.unpack . getSymbol . head) $ filter ((> 1) . length) groupedNames
    mainFunc = find ((== (Symbol "main")) . defName) prog

desugar :: SchemeProg -> SchemeProg
desugar prog =
  map desugarDefine prog
  where
    desugarDefine :: Definition -> Definition
    desugarDefine (Define name args body isInlinable isConstant) =
      Define name args (map desugarSexp body) isInlinable isConstant

    desugarSexp :: Sexp -> Sexp
    desugarSexp = cata alg
      where
        alg :: SexpF Sexp -> Sexp
        alg (And x y)    = Fix $ If x y (Fix constFalse)
        alg (Or x y)     = Fix $ If x (Fix constTrue) y
        alg (Not x)      = Fix $ If x (Fix constFalse) (Fix constTrue)
        alg (Cond cases) =
          -- foldr1 (\(test, body) cont -> Fix $ If test (Fix $ Begin body) cont) cases
          foldr (\(test, body) cont -> Fix $ If test (Fix $ Begin body) cont)
                (Fix constNil)
                cases
        alg (LetStar bindings body) =
          foldr (\binding rest -> Fix $ Let [binding] [rest]) (Fix $ Begin body) bindings
        alg form         = Fix form

optimize :: SchemeProg -> SchemeProg
optimize = map (fmap (map optimizeExpr))
  where
    optimizeExpr :: Sexp -> Sexp
    optimizeExpr = cata alg

    alg :: SexpF Sexp -> Sexp
    alg (If (Fix (Not z)) x y) = Fix $ If z y x
    alg (Begin [x])            = x
    alg (Let [] xs)            = optimizeExpr $ Fix $ Begin xs
    alg (Not (Fix (Not x)))    = x
    alg e                      = Fix e

collectReferences :: Sexp -> Set Symbol
collectReferences = cata alg
  where
    alg :: SexpF (Set Symbol) -> Set Symbol
    alg (Reference name)    = S.singleton name
    alg (Lambda args body) =
      F.fold body `S.difference` S.fromList args
    alg (Let bindings body) =
      F.fold body `S.difference` boundNames
      where
        boundNames = S.fromList $ map fst bindings
    alg (LetStar bindings body) =
      F.fold body `S.difference` boundNames
      where
        boundNames = S.fromList $ map fst bindings
    alg e                   = F.fold e

constNil :: SexpF a
constNil = Constant $ LiteralInt 0

constTrue :: SexpF a
constTrue = Constant $ LiteralBool True

constFalse :: SexpF a
constFalse = Constant $ LiteralBool False

