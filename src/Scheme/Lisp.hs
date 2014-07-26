{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.Lisp where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char
import Data.Foldable (Foldable)
import Data.List
import Data.Traversable (Traversable)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

-- import Data.Sexp (List, Atom)
import qualified Data.Sexp as SXP
import Language.Sexp.Parser (parse)

import Utils.RecursionSchemes

newtype Symbol = Symbol { getSymbol :: Text }
               deriving (Show, Eq, Ord)

bsToText :: ByteString -> Text
bsToText = T.pack . BS.unpack

mkSymbol :: ByteString -> Symbol
mkSymbol = Symbol . bsToText

data CmpOp = CmpEq
           | CmpGt
           | CmpGe
           deriving (Show, Eq, Ord)

data Literal f = LiteralInt Int
               | LiteralBool Bool
               | LiteralCons f f
               | LiteralClosure Symbol -- cannot be directly parsed from source
               deriving (Show, Eq, Ord)

data SexpF f = Define Symbol [Symbol] f
             | Lambda [Symbol] f -- must be lifted
             | Cons f f
             | Car f
             | Cdr f
             | Add f f
             | Sub f f
             | Mul f f
             | Div f f
             | Assign Symbol f
             | Let [(Symbol, f)] f -- parallel let
             | If f f f
             | Cmp CmpOp f f
             | IsAtom f
             | Call f [f]
             | Reference Symbol
             -- | Quote f
             | Constant (Fix Literal)
             deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Sexp = Fix SexpF
type Program = [Sexp]

parseSexp :: Text -> Either String Program
parseSexp input =
  either Left validateProgram $
  either (\(msg, rest) -> Left $ msg ++ BS.unpack rest) (Right . map go) $
  parse (BS.pack $ T.unpack input)
  where
    atomToSymbol :: SXP.Sexp -> Symbol
    atomToSymbol (SXP.Atom x) = mkSymbol x
    atomToSymbol list         =
      error $ "atomToSymbol: expected symbol but got list: " ++ show list

    go :: SXP.Sexp -> Sexp
    go = ana coalg

    coalg :: SXP.Sexp -> SexpF SXP.Sexp
    coalg form@(SXP.List (SXP.Atom "define": rest)) =
      case rest of
        -- constant
        [SXP.Atom name, body]         -> Define (mkSymbol name) [] body
        -- function
        [SXP.List (name: args), body] -> Define (atomToSymbol name)
                                                (map atomToSymbol args)
                                                body
        _ -> error $ "invalid define form: " ++ show form
    coalg form@(SXP.List (SXP.Atom "lambda": rest)) =
      case rest of
        [SXP.List args, body] -> Lambda (map atomToSymbol args)
                                        body
        _ -> error $ "invalid lambda form: " ++ show form
    coalg form@(SXP.List (SXP.Atom "cons": rest)) =
      case rest of
        [x, y] -> Cons x y
        _ -> error $ "invalid cons form: " ++ show form
    coalg form@(SXP.List (SXP.Atom "car": rest)) =
      case rest of
        [x] -> Car x
        _ -> error $ "invalid car form: " ++ show form
    coalg form@(SXP.List (SXP.Atom "cdr": rest)) =
      case rest of
        [x] -> Cdr x
        _ -> error $ "invalid cdr form: " ++ show form
    coalg form@(SXP.List (SXP.Atom "+": rest)) =
      case rest of
        [x, y] -> Add x y
        _ -> error $ "invalid + form: " ++ show form
    coalg form@(SXP.List (SXP.Atom "*": rest)) =
      case rest of
        [x, y] -> Mul x y
        _ -> error $ "invalid * form: " ++ show form
    coalg form@(SXP.List (SXP.Atom "-": rest)) =
      case rest of
        [x, y] -> Sub x y
        _ -> error $ "invalid - form: " ++ show form
    coalg form@(SXP.List (SXP.Atom "/": rest)) =
      case rest of
        [x, y] -> Div x y
        _ -> error $ "invalid / form: " ++ show form
    coalg form@(SXP.List (SXP.Atom "set!": rest)) =
      case rest of
        [SXP.Atom x, y] -> Assign (mkSymbol x) y
        _ -> error $ "invalid / form: " ++ show form
    coalg form@(SXP.List (SXP.Atom "let": rest)) =
      case rest of
        [SXP.List bindings, body] ->
          Let (map analyzeBinding bindings) body
        _ -> error $ "invalid let form: " ++ show form
      where
        analyzeBinding :: SXP.Sexp -> (Symbol, SXP.Sexp)
        analyzeBinding (SXP.List [SXP.Atom x, y]) = (mkSymbol x, y)
        analyzeBinding b = error $ "invalid let binding: " ++ show b
    coalg form@(SXP.List (SXP.Atom "if": rest)) =
      case rest of
        [x, y, z] -> If x y z
        _ -> error $ "invalid if form: " ++ show form
    coalg form@(SXP.List (SXP.Atom "==": rest)) =
      case rest of
        [x, y] -> Cmp CmpEq x y
        _ -> error $ "invalid == form: " ++ show form
    coalg form@(SXP.List (SXP.Atom ">": rest)) =
      case rest of
        [x, y] -> Cmp CmpGt x y
        _ -> error $ "invalid > form: " ++ show form
    coalg form@(SXP.List (SXP.Atom ">=": rest)) =
      case rest of
        [x, y] -> Cmp CmpGe x y
        _ -> error $ "invalid == form: " ++ show form
    coalg form@(SXP.List (SXP.Atom "atom?": rest)) =
      case rest of
        [x] -> IsAtom x
        _ -> error $ "invalid atom? form: " ++ show form
    coalg (SXP.List (func: rest)) =
      Call func rest
    coalg (SXP.List []) = error "empty list"

    coalg (SXP.Atom "#t") = Constant $ Fix $ LiteralBool True
    coalg (SXP.Atom "#f") = Constant $ Fix $ LiteralBool False
    coalg (SXP.Atom name)
      | BS.all isDigit name = Constant $ Fix $ LiteralInt $ read $ BS.unpack name
      | otherwise = Reference $ mkSymbol name

validateProgram :: Program -> Either String Program
validateProgram []   = Left "no functions defined in program"
validateProgram prog =
  case find (not . isOkToplevel . unFix) prog of
    Just sexp -> Left $ "invalid toplevel sexp: " ++ show sexp
    Nothing   -> Right prog
  where
    isOkToplevel :: SexpF (Fix SexpF) -> Bool
    isOkToplevel (Define _ _ _) = True
    isOkToplevel _              = False

