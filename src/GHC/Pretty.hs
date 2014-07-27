module GHC.Pretty where

import Data.List

import GHC.AST

class Pretty a where
  pretty :: a -> String

instance Pretty R where
  pretty (R c) = [c]
  pretty PC = "PC"

instance Pretty Arg where
  pretty (Reg  r) = pretty r
  pretty (IReg r) = "[" ++ pretty r ++ "]"
  pretty (Lit  i) = show i
  pretty (ILit i) = "[" ++ show i ++ "]"

instance Pretty Cmp where
  pretty (Cmp op x y) =
    intercalate " " [pretty x, pretty op, pretty y]

instance Pretty Op where
  pretty Lt = "<"
  pretty Eq = "="
  pretty Gt = ">"

binPretty :: (Pretty a, Pretty b) => String -> a -> b -> String
binPretty op x y = op ++ " " ++ pretty x ++ "," ++ pretty y

unaryPretty :: Pretty a => String -> a -> String
unaryPretty op x = op ++ " " ++ pretty x

instance Pretty Cmd where
  pretty cmd = case cmd of
    Inc x   -> unaryPretty "inc" x
    Dec x   -> unaryPretty "dec" x
    Mov x y -> binPretty "mov" x y
    Add x y -> binPretty "add" x y
    Sub x y -> binPretty "sub" x y
    Mul x y -> binPretty "mul" x y
    Div x y -> binPretty "div" x y
    And x y -> binPretty "and" x y
    Or  x y -> binPretty "or" x y
    Xor x y -> binPretty "xor" x y
    Intr i  -> "int " ++ show i
    Hlt     -> "hlt"

    Labeled (Label l) p ->
      show l ++ ":(\n" ++ pretty p ++ ")"

    JumpIf  cmp (Label l) -> jumpPretty "j" l cmp
    BreakIf cmp (Label l) -> jumpPretty "b" l cmp
    Comment cmd msg       -> pretty cmd ++ " ; " ++ msg
    Nop                   -> pretty (And (ILit 0) (ILit 0))

jumpPretty prefix label (Cmp op x y) =
  concat $ [ prefix
           , opString op, " "
           , show label, ","
           , pretty x, ","
           , pretty y
           ]
  where
    opString Lt = "lt"
    opString Eq = "eq"
    opString Gt = "gt"

instance Pretty Program where
  pretty (Program cs) = unlines $ map pretty cs

prettyProgram :: Program -> String
prettyProgram (Program cs) =
  unlines $
  map (\(i,s) -> show i ++ ": " ++ s) $
  zip [0..] (map pretty cs)
