{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Scheme.Types where

import Data.Foldable (Foldable)
import Data.Monoid
import Data.Traversable (Traversable)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Text.PrettyPrint.ANSI.Leijen

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
             | TailExit f
             | Reference Symbol
             {- | Quote f -}
             | Constant Literal
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

showSchemeProg :: SchemeProg -> Text
showSchemeProg prog = T.pack $ displayS (renderPretty 1.0 80 $ vcat $ map showDef prog) []
  where
    showDef:: Definition -> Doc
    showDef (Define name args body _ isConstant) =
      parens $
      text "define" <+>
      parens' (nameDoc name <+> hsep (map nameDoc args)) <$>
      indent 2 (vsep (map (cata alg) body))
      where
        parens' = if isConstant
                  then id
                  else parens
    alg :: SexpF Doc -> Doc
    alg (Lambda args body)      = parens $
                                  text "lambda" <+>
                                  parens (hsep $ map nameDoc args) <$>
                                  indent 2 (vcat body)
    alg (Cons x y)              = parens $ text "cons" <+> align (x <$> y)
    alg (Car x)                 = parens $ text "cdr" <+> x
    alg (Cdr x)                 = parens $ text "car" <+> x
    alg (Add x y)               = parens $ text "+" <+> align (x <$> y)
    alg (Sub x y)               = parens $ text "-" <+> align (x <$> y)
    alg (Mul x y)               = parens $ text "*" <+> align (x <$> y)
    alg (Div x y)               = parens $ text "/" <+> align (x <$> y)
    alg (Assign name x)         = parens $ text "set!" <+> nameDoc name <+> x
    alg (Let bindings body)     = parens $
                                  text "let" <+>
                                  align (parens (vcat $ map (\(name, x) -> parens (nameDoc name <+> x)) bindings)) <$>
                                  indent 2 (vcat body)
    alg (LetStar bindings body) = error "cannot prettyprint let*"
    alg (And x y)               = parens $ text "and" <+> align (x <$> y)
    alg (Or x y)                = parens $ text "or" <+> align (x <$> y)
    alg (Not x)                 = parens $ text "not" <+> x
    alg (Cond cases)            = error "cannot prettyprint cond"
    alg (If c t f)              = parens $ text "if" <+> align (c <$> t <$> f)
    alg (Cmp op x y)            = parens $ op' <+> align (x <$> y)
      where
        op' = text $ case op of
                       CmpEq -> "=="
                       CmpGt -> ">"
                       CmpGe -> ">="
    alg (IsAtom x)              = parens $ text "atom?" <+> x
    alg (List xs)               = parens $ text "list" <+> align (vsep xs)
    alg (Begin xs)              = parens $ text "begin" <+> align (vsep xs)
    alg (MakeClosure name)      = parens $ text "make-closure" <+> nameDoc name
    alg (Call f args)           = parens $ f <+> align (vsep args)
    alg (Debug x)               = parens $ text "debug" <+> x
    alg (Break)                 = parens $ text "break"
    alg (TailCall name x)       = error "cannot prettyprint tail call"
    alg (Reference name)        = nameDoc name
    alg (Constant lit)          = case lit of
                                    LiteralInt n -> int n
                                    LiteralBool b -> text $ if b then "#t" else "#f"
                                    _ -> error "cannot prettyprint literal closure"
    alg (Recur c t body)        = parens $ text "if-then-recur" <+> align (c <$> t <$> parens (hsep body))

    nameDoc :: Symbol -> Doc
    nameDoc = text . T.unpack . getSymbol

