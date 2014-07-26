{-# LANGUAGE BangPatterns #-}
module LispMachine.Flatten
    ( flatten
    ) where

import Control.Applicative
import qualified Data.Map as M
import Data.List

import qualified Data.Traversable as Tr

import LispMachine.Instructions

import Utils.Utils

flatten :: Program -> Either String FlatProgram
flatten (Program lst) = FlatProgram . concat <$> (sequence . snd . Tr.mapAccumL deref Nothing $ lst)
    where
      refs :: M.Map Ref Addr
      refs = snd . foldl' gather (0, M.empty) $ lst

      gather (!offset, m) (SetLabel lbl) = (offset, M.insert (Ref lbl) (Addr offset) m)
      gather (!offset, m) (Instr _)      = (offset + 1, m)

      deref :: Maybe String -> Statement -> (Maybe String, Either String [ AnnotatedInstruction ])
      deref _   (SetLabel lbl)   = (Just lbl, return [])
      deref ann (Instr i)        = (Nothing, resolvedI)
          where resolvedI = do
                  i <- Tr.sequence $ lookupRef `fmap` i
                  return [ AnnotatedInstruction i ann ]

      lookupRef :: Ref -> Either String AnnotatedAddr
      lookupRef ref@(Ref lbl) = do
        addr <- M.lookup ref refs <?> msg ref
        return $ AnnotatedAddr addr (Just lbl)
      lookupRef (At addr) = return $ AnnotatedAddr addr Nothing

      msg ref = "Could not dereference label" ++ show ref

