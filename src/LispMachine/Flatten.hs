{-# LANGUAGE BangPatterns #-}
module LispMachine.Flatten
    ( flatten
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import qualified Data.Map as M
import Data.List

import qualified Data.Traversable as Tr

import LispMachine.Instructions

import Utils.Utils

data AnnotationState = AnnotationState
    { asLabel :: Maybe Label
    , asComment :: Maybe String
    }

emptyState :: AnnotationState
emptyState = AnnotationState Nothing Nothing

flatten :: Program -> Either String FlatProgram
flatten (Program lst) = evalStateT (FlatProgram . concat <$> mapM deref (SetLabel "main":lst)) emptyState
    where
      refs :: M.Map Ref Addr
      refs = snd . foldl' gather (0, M.singleton (Ref "main") (Addr 0)) $ lst

      gather state        (Annotate _)   = state
      gather (!offset, m) (SetLabel lbl) = (offset, M.insert (Ref lbl) (Addr offset) m)
      gather (!offset, m) (Instr _)      = (offset + 1, m)

      deref :: Statement -> StateT AnnotationState (Either String) [ AnnotatedInstruction ]
      deref (Annotate msg) = do
        modify (\s -> s { asComment = Just msg } )
        return [ ]
      deref (SetLabel lbl) = do
        modify (\s -> s { asLabel = Just lbl } )
        return [ ]
      deref (Instr i) = do
        (AnnotationState label comment) <- get
        put emptyState
        ires <- lift $ Tr.sequence $ lookupRef `fmap` i
        lbl <- lift $ maybe (return Nothing) (\lbl -> Just <$> ((,) <$> (aAddr <$> lookupRef (Ref lbl)) <*> pure lbl)) label
        return [ AnnotatedInstruction ires lbl comment ]

      lookupRef :: Ref -> Either String AnnotatedAddr
      lookupRef ref@(Ref lbl) = do
        addr <- M.lookup ref refs <?> msg ref
        return $ AnnotatedAddr addr (Just lbl)
      lookupRef (At addr) = return $ AnnotatedAddr addr Nothing

      msg ref = "Flatten: Could not dereference label " ++ show ref

