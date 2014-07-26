module GHC.AI where

import Prelude hiding (cycle,and)

import GHC.AST
import GHC.DSL
import GHC.DSL.Interrupt
import GHC.Pretty

smart :: GHCM ()
smart = undefined

fickle :: GHCM ()
fickle = do
   withVar3 inifinity 0 (-1) $ \a b c -> do
     cycle $ do
       -- 0 <= c <= 3
       inc c
       ifNot (mem c >: a) $ do
         a =: mem c
         b =: c
       breakIf (c >: 2)

     go b

   withMyIndex $ \idx ->
     withDirectionAndVitality idx $ \_vit dir -> do
       inc (mem dir)

   halt

miner :: GHCM ()
miner = do
  goDown
  halt

flipper :: GHCM ()
flipper = do
  withMyIndex $ \idx ->
    withGhostPosition Current idx $ \x _y -> do
      x `and` 1
      ifte (x =:= 1)
        goDown
        goUp
  halt

main :: IO ()
main = putStrLn $ prettyProgram $ flatten $ runGHCM fickle

test :: IO ()
test = putStrLn $ pretty $ flatten $ runGHCM fickle
