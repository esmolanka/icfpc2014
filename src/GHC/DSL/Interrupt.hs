module GHC.DSL.Interrupt where

import GHC.AST
import GHC.DSL

import qualified Data.Set as Set

withRegisters :: [R] -> [Var] ->
                 ([Var] -> [Var] -> GHCM ()) -> GHCM ()
withRegisters [] vars func = func [] vars
withRegisters (r:rs) vars func =
  withRegister r vars $
    \r'  vars'  -> withRegisters rs vars' $
    \rs' vars'' -> func (r':rs') vars''

withRegister :: R -> [Var] ->
                (Var -> [Var] -> GHCM ()) -> GHCM ()
withRegister PC _ _ = error "withRegister: PC register"
withRegister (R r) vars f = do
  let regVar = Reg $ R r
  vs <- getVarSet
  if Set.member regVar vs
  then do
    withVar 0 $ \x -> do
      x =: regVar
      let vars' = map (\y -> if y == regVar
                             then x
                             else y)
                  vars
      modVarSet (Set.insert regVar) $
        f regVar vars'
      regVar =: x
  else
    modVarSet (Set.insert regVar) $
      f regVar vars

debug :: GHCM ()
debug = int 8

withMyIndex :: (Var -> GHCM ()) -> GHCM ()
withMyIndex f =
  withVar 0 $ \idx -> do
    withRegisters [R 'A'] [idx] $ \[a] [idx] -> do
      int 3
      idx =: a
    f idx

withMapContent :: Var -> Var -> (Var -> GHCM ()) -> GHCM ()
withMapContent x y f = do
  withVar 0 $ \cont -> do
    withRegisters [R 'A', R 'B'] [x,y,cont] $ \[a,b] [x,y,cont] -> do
      a =: x
      b =: y
      int 7
      cont =: a
    f cont

withDirectionAndVitality :: Var -> (Var -> Var -> GHCM ()) -> GHCM ()
withDirectionAndVitality idx f =
  withVar2 0 0 $ \vit dir -> do
    withRegisters [R 'A', R 'B'] [idx,vit,dir] $ \[a,b] [idx,vit,dir] -> do
      a =: idx
      int 6
      vit =: a
      dir =: b
    f vit dir

data GPos = Current | Starting

withGhostPosition :: GPos -> Var -> (Var -> Var -> GHCM ()) -> GHCM ()
withGhostPosition gpos idx f =
  withVar2 0 0 $ \x y -> do
    withRegisters [R 'A', R 'B'] [idx,x,y] $ \[a,b] [idx,x,y] -> do
      a =: idx
      let intCode = case gpos of
            Starting -> 4
            Current -> 5
      int intCode
      x =: a
      y =: b
    f x y

-- player = 1 | 2
withLambdaManPosition :: Int -> (Var -> Var -> GHCM ()) -> GHCM ()
withLambdaManPosition player f =
  withVar2 0 0 $ \x y -> do
    withRegisters [R 'A', R 'B'] [x,y] $ \[a,b] [x,y] -> do
      int player
      x =: a
      y =: b
    f x y

int :: Int -> GHCM ()
int = cmd . Intr

go :: Var -> GHCM ()
go dir = do
  withRegisters [R 'A'] [dir] $ \[a] [dir] -> do
    a =: dir
    int 0

goDown, goLeft, goUp, goRight :: GHCM ()
goUp    = go 0
goRight = go 1
goDown  = go 2
goLeft  = go 3

