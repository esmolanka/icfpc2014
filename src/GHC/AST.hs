{-# language
   ViewPatterns
 , GeneralizedNewtypeDeriving
 #-}

module GHC.AST where

import Prelude hiding (cycle)

import Data.List hiding (cycle)
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad
import Control.Arrow
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

type Var = Arg

mem :: Var -> Var
mem (Reg r) = IReg r
mem (Lit i) = ILit i
mem v = error $ "Can't take value at address " ++ show v

data Label = Label Int
  deriving (Show, Eq, Ord)

data R = R Char
       | PC {- probably wouldn't be needed at all -}
  deriving (Show, Eq, Ord)

data Arg = Reg R
         | IReg R
         | Lit Int
         | ILit Int
  deriving (Show, Eq, Ord)

instance Num Arg where
  (+) = error "Num.(+) for Arg isn't implemented."
  (*) = error "Num.(*) for Arg isn't implemented."
  abs = error "Num.abs for Arg isn't implemented."
  signum = error "Num.signum for Arg isn't implemented."
  fromInteger = Lit . fromIntegral

data Cmp = Cmp Op Var Var
  deriving Show

data Op = Lt | Eq | Gt
  deriving Show

data Cmd = Inc Arg
         | Dec Arg
         | Mov Arg Arg
         | Add Arg Arg
         | Sub Arg Arg
         | Mul Arg Arg
         | Div Arg Arg
         | And Arg Arg
         | Or  Arg Arg
         | Xor Arg Arg
         | Intr Int
         | Hlt
         --
         | Labeled Label Program
         | JumpIf  Cmp Label
         | BreakIf Cmp Label
  deriving Show

flatten :: Program -> Program
flatten (Program ps) = Program $ concatMap substCmdLabels ps
  where
    substCmdLabels :: Cmd -> [Cmd]
    substCmdLabels (Labeled _label (Program ps)) =
      concatMap substCmdLabels ps
    substCmdLabels (JumpIf cmp label) =
      return $ JumpIf cmp (Label $ fst $ lookupInLMap label)
    substCmdLabels (BreakIf cmp label) =
      return $ JumpIf cmp (Label $ snd $ lookupInLMap label)
    substCmdLabels cmd = return cmd

    lookupInLMap :: Label -> (Int,Int)
    lookupInLMap label =
      case Map.lookup label lMap of
        Just  r -> r
        Nothing -> error $ "Label wasn't found: " ++ show label

    numberOfInstructions :: Program -> Int
    numberOfInstructions (Program cs) = sum $ map cmdSize cs
      where
        cmdSize (Labeled _ ps) = numberOfInstructions ps
        cmdSize _ = 1

    lMap :: Map.Map Label (Int,Int)
    lMap = labelMap 0 ps

    -- Label -> (exact position, after position)
    labelMap :: Int -> [Cmd] -> Map.Map Label (Int,Int)
    labelMap _ [] = mempty
    labelMap lineIdx (Labeled label program@(Program ps):cs) =
      let pSize = numberOfInstructions program
          exact = lineIdx
          after = exact + pSize
      in  Map.insert label (exact,after) $
          labelMap lineIdx (ps ++ cs)
    labelMap lineIdx (_cmd:cs) =
      labelMap (lineIdx+1) cs

newtype Program = Program [Cmd]
  deriving (Show, Monoid)

type VarSet = Set.Set Var
type LabelStack = [Label]

type GHCM a =
  ReaderT (VarSet,LabelStack)
    (WriterT Program
       (State Int))
  a

allVars :: [Var]
allVars =
    [Reg (R c) | c <- reverse ['A'..'H']] ++
    [ILit i    | i <- reverse [0..255]]

findNewVar :: VarSet -> (Var,VarSet)
findNewVar vs =
  let newVar = head $ allVars \\ (Set.toList vs)
  in (newVar, Set.insert newVar vs)

getVarSet :: GHCM VarSet
getVarSet = asks fst

modVarSet :: (VarSet -> VarSet) -> GHCM () -> GHCM ()
modVarSet f = local (first f)

getLabelStack :: GHCM LabelStack
getLabelStack = asks snd

modLabelStack :: (LabelStack -> LabelStack) -> GHCM () -> GHCM ()
modLabelStack f = local (second f)

normalize :: Int -> Int
normalize n | n < 0 = 256 - ((-n) `mod` 256)
            | otherwise = n `mod` 256

cmd :: Cmd -> GHCM ()
cmd c = tell (Program [c])

getNewLabel :: GHCM Label
getNewLabel = do
  labelCnt <- get
  put (labelCnt + 1)
  return $ Label labelCnt

labelWith :: Label -> GHCM () -> GHCM ()
labelWith label act =
  censor (\p -> Program [Labeled label p]) act

runGHCM :: GHCM () -> Program
runGHCM act =
  snd $
  flip evalState 0 $
  runWriterT $
  flip runReaderT mempty $
  (act >> cmd Hlt)

withLabel :: (Label -> GHCM ()) -> GHCM ()
withLabel f = do
  label <- getNewLabel
  labelWith label $
    f label
