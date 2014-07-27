{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}

module Scheme.Frontend
  ( parseSexp
  , desugar
  , optimize
  , allUsedFunctions
  , collectDefRefs
  , collectRefs
  )
where

import Control.Arrow
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char
import qualified Data.Foldable as F
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
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
    coalg form@(S.List (S.Atom "letrec": rest)) =
      case rest of
        S.List bindings: body ->
          LetRec (map analyzeBinding bindings) body
        _ -> error $ "invalid letrec form: " ++ show form
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

    coalg form@(S.List (S.Atom "tailcall": rest)) =
        case rest of
          (S.Atom func : rest) -> TailCall (mkSymbol func) rest
          _ -> error $ "invalid tailcall form: " ++ show form

    coalg (S.List (func: rest)) =
        Call func rest
    coalg (S.List []) = error "empty list"

    coalg (S.Atom "#t") = constTrue
    coalg (S.Atom "#f") = constFalse
    coalg (S.Atom name)
      | BS.all (\c -> isDigit c || (BS.length name > 1 && (c == '+' || c == '-'))) name =
        Constant $ LiteralInt $ read $ BS.unpack name
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
    groupedNames = group $ sort names
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
optimize prog = map (fmap (map (optimizeExpr . inlineSingularBindings . optimizeExpr))) prog
  where
    inlinableFunctions = -- M.empty
      M.fromList $
      map (defName &&& id) $
      filter (\def -> defInlinable def || sum (map exprSize (defBody def)) <= 15) prog

    optimizeExpr :: Sexp -> Sexp
    optimizeExpr = cata alg
      where
        alg :: SexpF Sexp -> Sexp
        alg (If (Fix (Not z)) x y) = Fix $ If z y x
        alg (If (Fix (Constant (LiteralBool True))) x _)  = x
        alg (If (Fix (Constant (LiteralBool False))) _ y) = y
        alg (Begin [x])                                   = x
        alg (Let [] xs)                                   = optimizeExpr $ Fix $ Begin xs
        alg (Not (Fix (Not x)))                           = x
        alg form@(Call (Fix (Reference name)) argVals)
          | M.member name inlinableFunctions =
            let (Define _ args body _ _) = inlinableFunctions M.! name
            in
            if length args /= length argVals
            then error $ "function " ++ show (getSymbol name) ++
                 " called with " ++ show (length argVals) ++
                 " arguments, but expects " ++ show (length args) ++
                 ": " ++ show form
            else Fix $ Let (zip args argVals) body
          | otherwise = Fix form
        alg e                      = Fix e

    inlineSingularBindings :: Sexp -> Sexp
    inlineSingularBindings = cata alg
      where
        alg :: SexpF Sexp -> Sexp
        alg (Let bindings body) =
          case otherBindings of
            [] -> Fix $ Begin $ inlinedBody
            _  -> Fix $ Let otherBindings inlinedBody
          where
            refUses :: Map Symbol Int
            refUses    = foldr (M.unionWith (+)) M.empty $ map collectRefUses body
            singularRefs :: Set Symbol
            singularRefs = S.fromList $ map fst $ filter ((== 1) . snd) $ M.toList refUses

            (singleBindings, otherBindings) = partition (\(name, _) -> S.member name singularRefs)
                                                        bindings
            inlinedBody = foldr (\(name, initExpr) body' -> map (inline name initExpr) body')
                                      body
                                      singleBindings

            inline :: Symbol -> Sexp -> Sexp -> Sexp
            inline sym val = cata inlineAlg
              where
                inlineAlg :: SexpF Sexp -> Sexp
                inlineAlg e@(Reference name)
                  | name == sym = val
                  | otherwise   = Fix e
                inlineAlg e = Fix e
        alg e = Fix e

allUsedFunctions :: SchemeProg -> Set Symbol
allUsedFunctions prog = go (S.singleton (Symbol "main"))
                           (mainReferences `S.intersection` funcNames)
  where
    progDict       = M.fromList $ map (defName &&& id) prog
    funcNames      = M.keysSet progDict
    mainReferences = collectDefRefs $ progDict M.! (Symbol "main")

    go :: Set Symbol -> Set Symbol -> Set Symbol
    go visited toVisit
      | S.null toVisit = visited
      | otherwise      =
        go visited' ((toVisit' `S.intersection` funcNames) `S.difference` visited')
      where
        visited' = visited `S.union` toVisit
        toVisit' = F.foldMap (\sym -> collectDefRefs $ progDict M.! sym)
                             toVisit

collectDefRefs :: Definition -> Set Symbol
collectDefRefs (Define _ args body _ _) =
  (F.foldMap collectRefs body) `S.difference` S.fromList args

collectRefs :: Sexp -> Set Symbol
collectRefs = M.keysSet . collectRefUses

newtype MonoidalMap k v = MonoidalMap { getMonoidalMap :: Map k v }
                        deriving (Show, Eq, Ord)

instance (Ord k, Monoid v) => Monoid (MonoidalMap k v) where
  mempty = MonoidalMap mempty
  mappend (MonoidalMap m1) (MonoidalMap m2) = MonoidalMap $ M.unionWith mappend m1 m2

-- count how many times each reference is used in expression
collectRefUses :: Sexp -> Map Symbol Int
collectRefUses = M.map getSum . getMonoidalMap . cata alg
  where
    alg :: SexpF (MonoidalMap Symbol (Sum Int)) -> MonoidalMap Symbol (Sum Int)
    alg (Reference name)        = MonoidalMap $ M.singleton name (Sum 1)
    alg (Lambda args body)      = MonoidalMap $
      getMonoidalMap (F.fold body) `M.difference` argMap args
    alg (LetRec bindings body)  = MonoidalMap $
      getMonoidalMap (F.fold body <> F.foldMap snd bindings) `M.difference`
      argMap (map fst bindings)
    alg (Let bindings body)     = MonoidalMap $
      getMonoidalMap (F.fold body <> F.foldMap snd bindings) `M.difference`
      argMap (map fst bindings)
    alg (LetStar _ _)           = error "collectRefs: let* not supported, should be desugared"
    alg e                       = F.fold e

    argMap :: [Symbol] -> Map Symbol (Sum Int)
    argMap args = M.fromList $ zip args (repeat mempty)

exprSize :: Sexp -> Int
exprSize = getSum . cata alg
  where
    alg :: SexpF (Sum Int) -> (Sum Int)
    alg e = Sum 1 <> F.fold e

constNil :: SexpF a
constNil = Constant $ LiteralInt 0

constTrue :: SexpF a
constTrue = Constant $ LiteralBool True

constFalse :: SexpF a
constFalse = Constant $ LiteralBool False

