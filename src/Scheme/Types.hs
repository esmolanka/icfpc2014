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

data Literal = LiteralInt Int
             | LiteralBool Bool
             | LiteralClosure Symbol -- cannot be directly parsed from source
             deriving (Show, Eq, Ord)

data SexpF f = -- Define Symbol [Symbol] f
               Lambda [Symbol] [f] -- must be lifted
             | Cons f f
             | Car f
             | Cdr f
             | Add f f
             | Sub f f
             | Mul f f
             | Div f f
             | Assign Symbol f
             | Let [(Symbol, f)] [f] -- ^ parallel let
             | LetRec [(Symbol, f)] [f] -- ^ recursive let
             | LetStar [(Symbol, f)] [f] -- ^ sequential let
             | And f f
             | Or f f
             | Not f
             | Cond [(f, [f])]
             | If f f f
             | Cmp CmpOp f f
             | IsAtom f
             | List [f]
             | Begin [f]
             | MakeClosure Symbol -- ^ symbol must be a function name
             | Call f [f]
             | Debug f
             | Break
             | TailCall Symbol [f] -- ^ will be introduced by transformation
             | Reference Symbol
             {- | Quote f -}
             | Constant Literal
             | Recur f f [f] -- ^ cond, return value, recursion args; tail-calls current function with provided set of arguments
             deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Sexp = Fix SexpF

data DefinitionF f = Define
                   { defName       ::  Symbol
                   , defArgs       :: [Symbol]
                   , defBody       :: f
                   , defInlinable  :: Bool -- whether this form is inlinable
                   , defIsConstant :: Bool -- whether is was defined as a constant
                   }
                   deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Definition = DefinitionF [Sexp]

type SchemeProg = [Definition]

