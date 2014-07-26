{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable  #-}

module LispMachine.Instructions where

import Data.Traversable
import Data.Foldable

newtype Addr = Addr Int deriving (Show, Eq, Ord)

type Label = String

data AnnotatedAddr = AnnotatedAddr
    { aAddr :: Addr
    , aLabel :: Maybe Label
    } deriving (Show, Eq, Ord)

data Ref =
    At Addr
  | Ref Label
    deriving (Show, Eq, Ord)

data Statement = SetLabel Label
               | Instr (Instruction Ref)
                 deriving (Show)

newtype Program = Program [Statement] deriving (Show)

newtype FlatProgram = FlatProgram [Instruction AnnotatedAddr] deriving (Show)

data Instruction a =
    LDC Int             -- load constant, args: constant
  | LD Int Int          -- load from environment, frame and i'th element of frame, ARGS: frame index, element index
  | ST Int Int          -- update environment, work like LD
  | ADD                 -- integer addition
  | SUB                 -- integer subtraction
  | MUL                 -- integer multiplication
  | DIV                 -- integer division
  | CEQ                 -- compare equal
  | CGT                 -- compare greater than
  | CGTE                -- compare greater than or equal
  | ATOM                -- test if value is an integer
  | CONS                -- allocate a CONS cell
  | CAR                 -- extract first element from CONS cell
  | CDR                 -- extract second element from CONS cell
  | SEL a a             -- conditional branch, args: true branch, false branch
  | JOIN                -- return from branch
  | LDF a               -- load function
  | AP Int              -- call function, number of arguments to copy
  | RTN                 -- return from function call
  | DUM Int             -- create empty environment frame, size of frame to allocate
  | RAP Int             -- recursive environment call function, number of arguments to copy
  | STOP                -- terminate co-processor execution
  | TSEL a a            -- tail-call conditional branch, args: true branch, false branch
  | TAP Int             -- tail-call function, args: number of arguments to copy
  | TRAP Int            -- recursive environment tail-call function, args: number of arguments to copy
  | DBUG                -- printf debugging
  | BRK                 -- breakpoint debugging
    deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

