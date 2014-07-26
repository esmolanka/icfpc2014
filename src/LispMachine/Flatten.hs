{-# LANGUAGE BangPatterns #-}
module LispMachine.Flatten
    ( flatten
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.List

import qualified Data.Traversable as Tr

import LispMachine.Instructions

import Utils.Utils

flatten :: Program -> Either String FlatProgram
flatten (Program lst) = FlatProgram . concat <$> mapM deref lst
    where
      refs :: M.Map Ref Addr
      refs = snd . foldl' gather (0, M.empty) $ lst

      gather (!offset, m) (SetLabel lbl) = (offset, M.insert (Ref lbl) (Addr offset) m)
      gather (!offset, m) (Instr _)      = (offset + 1, m)

      deref :: Statement -> Either String [ Instruction AnnotatedAddr ]
      deref (SetLabel _) = return []
      deref (Instr i)    = do
        i <- Tr.sequence $ lookupRef `fmap` i
        return [ i ]

      lookupRef :: Ref -> Either String AnnotatedAddr
      lookupRef ref@(Ref lbl) = do
        addr <- M.lookup ref refs <?> msg ref
        return $ AnnotatedAddr addr (Just lbl)
      lookupRef (At addr) = return $ AnnotatedAddr addr Nothing

      msg ref = "Could not dereference label" ++ show ref

