module GHC.AI where

import Prelude hiding (cycle,and)

import GHC.AST
import GHC.DSL
import GHC.DSL.Interrupt
import GHC.Pretty

-- Always move in the direction of the lambda man (first one)
-- End up cycling very fast
simple :: GHCM ()
simple = do
  withLambdaManPosition 1 $ \lx ly -> do
    withMyPosition $ \px py -> do
      ifte (px <: lx)
        goRight $
        ifte (px >: lx)
           goLeft $
           ifte (py <: ly)
              goDown
              goUp
  halt

-- hand == 1 - follow right hand
-- hand == 0 - follow left  hand
followHand :: Int -> GHCM ()
followHand hand = do
  withMyIndex $ \idx ->
    withDirectionAndVitality idx $ \_vit dir -> do
      if hand == 0
      then
        ifte (dir =:= 0)
          (dir =: 3)
          (dec dir)
      else
        ifte (dir =:= 3)
          (dir =: 0)
          (inc dir)
      go dir
  halt

-- Always move in the direction of the lambda man (first one)
-- Check X coordinate first if current direction is 0|1
-- Otherwise checks Y coordinate first
follower1 :: GHCM ()
follower1 = do
  withLambdaManPosition 1 $ \lx ly -> do
    withMyIndex $ \idx ->
      withGhostPosition Current idx $ \px py -> do
        withDirectionAndVitality idx $ \_vit dir ->
          ifte (dir <: 2)
            (checkYFirst (px,py) (lx,ly))
            (checkXFirst (px,py) (lx,ly))
  halt
  where
    checkXFirst (px,py) (lx,ly) = do
      ifte (px <: lx)
        goRight $
        ifte (px >: lx)
           goLeft $
           ifte (py <: ly)
              goDown
              goUp

    checkYFirst (px,py) (lx,ly) = do
      ifte (py <: ly)
        goDown $
        ifte (py >: ly)
           goUp $
           ifte (px <: lx)
              goRight
              goLeft

-- 148105
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
main = putStrLn $ prettyProgram $ flatten $ runGHCM $ followHand 1

test :: IO ()
test = putStrLn $ pretty $ flatten $ runGHCM $ followHand 1
