module GHC.AI where

import Prelude hiding (cycle,and,mod)

import GHC.AST
import GHC.DSL
import GHC.DSL.Interrupt
import GHC.Pretty

-- Switches from left to rigth hand strategy
-- every `period` moves
ambidexter :: Int -> GHCM ()
ambidexter period = do
  switchHand
  updateTimeVar
  makeMove
  where
    timeVar = mem 0
    handVar = mem 1

    makeMove =
      ifte (handVar =:= 0)
        (followHand 0)
        (followHand 1)

    updateTimeVar = do
      inc timeVar
      if' (timeVar =:= litVar period) $
        timeVar =: 0

    switchHand = do
      if' (timeVar =:= 0) $ do
        inc handVar
        handVar `and` 1

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

-- Always move in the direction of the lambda man (first one)
-- End up cycling very fast
follower0 :: GHCM ()
follower0 = do
  withLambdaManPosition 1 $ \lx ly -> do
    withMyPosition $ \px py -> do
      checkXFirst (px,py) (lx,ly) 0

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
            (checkYFirst (px,py) (lx,ly) 0)
            (checkXFirst (px,py) (lx,ly) 0)

predator :: Int -> Int -> GHCM ()
predator searchPeriod getOutPeriod = do
  updateVitality

  -- In case of "fright mode" switch
  -- to search mode.
  if' (vitalityVar =:= 1) $
    modeVar =: 0

  ifte (modeVar =:= 0)
    searchMode
    getOutMode
  where
    searchTimeVar = mem 0
    getoutTimeVar = mem 1

    handVar = mem 2
    xVar    = mem 3
    yVar    = mem 4
    modeVar = mem 5
    vitalityVar = mem 6

    updateVitality =
      withMyIndex $ \idx ->
        withDirectionAndVitality idx $ \vit _dir ->
          vitalityVar =: vit

    switchMode = do
      inc modeVar
      modeVar `and` 1

    switchHand = do
      inc handVar
      handVar `and` 1

    incGetoutTimeVar = do
      inc getoutTimeVar

    incSearchTimeVar = do
      inc searchTimeVar
      searchTimeVar `mod` (litVar searchPeriod)

    getOutMode = do
      ifte (getoutTimeVar =:= litVar getOutPeriod)
        toSearchMode
        (do makeGetOutMove
            incGetoutTimeVar)
      where
        toSearchMode = do
          switchMode
          switchHand
          searchTimeVar =: 0
          getoutTimeVar =: 0

        makeGetOutMove = do
          ifte (handVar =:= 0)
            (followHand 0)
            (followHand 1)

    searchMode = do
      withMyPosition $ \x y -> do
        ifte (searchTimeVar =:= 0)
          (do
             xVar =: x
             yVar =: y)
          (do
             withVar 0 $ \sameCoord -> do
               if' (x =:= xVar) (inc sameCoord)
               if' (y =:= yVar) (inc sameCoord)
               if' (sameCoord =:= 2)
                 toGetOutMode)
      makeSearchMove
      incSearchTimeVar
      where
        toGetOutMode = do
          switchMode
          getoutTimeVar =: 0
          searchTimeVar =: 0

        makeSearchMove =
          withLambdaManPosition 1 $ \lx ly -> do
            withMyPosition $ \px py ->
              withVar2 0 0 $ \dx dy -> do
                dx =: lx
                dy =: ly
                dx `absDiff` px
                dy `absDiff` py

                ifte (px <: lx)
                  goRight'
                  goLeft'

                withVar 0 $ \checkY -> do
                  if' (dy >: dx) $ inc checkY
                  if' (dx =:= 0) $ inc checkY
                  if' (checkY >: 0) $
                    ifte (py <: ly)
                      goDown'
                      goUp'

        isFrightMode = vitalityVar =:= 1
        goDown'  = ifte isFrightMode goUp    goDown
        goUp'    = ifte isFrightMode goDown  goUp
        goLeft'  = ifte isFrightMode goRight goLeft
        goRight' = ifte isFrightMode goLeft  goRight


checkXFirst :: (Var,Var) -> (Var,Var) -> Var -> GHCM ()
checkXFirst (px,py) (lx,ly) vitality = do
  ifte (px <: lx)
    goRight' $
    ifte (px >: lx)
       goLeft' $
       ifte (py <: ly)
          goDown'
          goUp'
  where
    isFrightMode = vitality =:= 1
    goDown'  = ifte isFrightMode goUp    goDown
    goUp'    = ifte isFrightMode goDown  goUp
    goLeft'  = ifte isFrightMode goRight goLeft
    goRight' = ifte isFrightMode goLeft  goRight

checkYFirst :: (Var,Var) -> (Var,Var) -> Var -> GHCM ()
checkYFirst (px,py) (lx,ly) vitality = do
  ifte (py <: ly)
    goDown' $
    ifte (py >: ly)
       goUp' $
       ifte (px <: lx)
          goRight'
          goLeft'
  where
    isFrightMode = vitality =:= 1
    goDown'  = ifte isFrightMode goUp    goDown
    goUp'    = ifte isFrightMode goDown  goUp
    goLeft'  = ifte isFrightMode goRight goLeft
    goRight' = ifte isFrightMode goLeft  goRight


-- Bots from the reference

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

miner :: GHCM ()
miner = goDown

flipper :: GHCM ()
flipper =
  withMyPosition $ \x _y -> do
    x `and` 1
    ifte (x =:= 1)
      goDown
      goUp

main :: IO ()
main = putStrLn $ prettyProgram $ flatten $ runGHCM $
       predator 50 20

test :: IO ()
test = putStrLn $ pretty $ flatten $ runGHCM $
       predator 50 20
