
module LispMachine.Flatten where

import qualified Data.Map as M
import Data.Maybe

import LispMachine.Instructions

flatten :: Program -> FlatProgram
flatten (Program lst) = FlatProgram $ mapMaybe deref lst
    where
      refs :: M.Map Ref Addr
      refs = snd . foldr gather (0, M.empty) $ lst

      gather (SetLabel lbl) (offset, m) = (offset, M.insert (Ref lbl) (Addr offset) m)
      gather (Instr _)  (offset, m) = (offset + 1, m)

      deref (SetLabel _) = Nothing
      deref (Instr i) = Just (fmap lookupRef i)

      lookupRef ref@(Ref lbl) = AnnotatedAddr (fromMaybe (err ref) $ M.lookup ref refs) (Just lbl)
      lookupRef (At addr) = AnnotatedAddr addr Nothing

      err ref = error $ "Could not dereference label" ++ show ref

