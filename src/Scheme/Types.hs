{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Scheme.Types where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Text.Lazy (Text)

import Utils.RecursionSchemes

newtype Symbol = Symbol { getSymbol :: Text }
               deriving (Show, Eq, Ord)

data CmpOp = CmpEq
           | CmpGt
           | CmpGe
           deriving (Show, Eq, Ord)

data Literal f = LiteralInt Int
               | LiteralBool Bool
               | LiteralCons f f
               | LiteralClosure Symbol -- cannot be directly parsed from source
               deriving (Show, Eq, Ord)

data SexpF f = -- Define Symbol [Symbol] f
               Lambda [Symbol] f -- must be lifted
             | Cons f f
             | Car f
             | Cdr f
             | Add f f
             | Sub f f
             | Mul f f
             | Div f f
             | Assign Symbol f
             | Let [(Symbol, f)] [f] -- parallel let
             | If f f f
             | Cmp CmpOp f f
             | IsAtom f
             | List [f]
             | Begin [f]
             | MakeClosure Symbol
             | Call f [f]
             | Reference Symbol
             -- | Quote f
             | Constant (Fix Literal)
             deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Sexp = Fix SexpF

data DefinitionF f = Define
                   { defName ::  Symbol
                   , defArgs :: [Symbol]
                   , defBody :: [f]
                   }
                   deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Definition = DefinitionF Sexp

type SchemeProg = [Definition]

