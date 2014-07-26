{-# language
   ViewPatterns
 #-}

module GHC.DSL where

import GHC.AST

litVar :: Int -> Var
litVar = Lit

(<:),(=:=),(>:) :: Var -> Var -> Cmp
(<:)  = Cmp Lt
(=:=) = Cmp Eq
(>:)  = Cmp Gt

withVar :: Int -> (Arg -> GHCM ()) -> GHCM ()
withVar (normalize -> initVal) func = do
  vs <- getVarSet
  let (var,vs') = findNewVar vs
  modVarSet (const vs') $ do
    var =: litVar initVal
    func var

withVar2 :: Int -> Int -> (Arg -> Arg -> GHCM ()) -> GHCM ()
withVar2 v1 v2 func = do
  withVar v1 $ \r1 ->
    withVar v2 $ \r2 ->
      func r1 r2

withVar3 :: Int -> Int -> Int -> (Arg -> Arg -> Arg -> GHCM ()) -> GHCM ()
withVar3 v1 v2 v3 func = do
  withVar v1 $ \r1 ->
    withVar v2 $ \r2 ->
      withVar v3 $ \r3 ->
        func r1 r2 r3

mov :: Var -> Var -> GHCM ()
mov dest src =
  cmd $ Mov dest src

inc :: Var -> GHCM ()
inc src =
  cmd $ Inc src

(=:) :: Var -> Var -> GHCM ()
(=:) = mov

add :: Var -> Var -> GHCM ()
add dest src =
  cmd $ Add dest src

and :: Var -> Var -> GHCM ()
and dest src =
  cmd $ And dest src

true :: Cmp
true = Lit 0 =:= Lit 0

false :: Cmp
false = Lit 0 =:= Lit 1

goto :: Label -> GHCM ()
goto = cmd . JumpIf true

gotoAfter :: Label -> GHCM ()
gotoAfter = cmd . BreakIf true

cycle :: GHCM () -> GHCM ()
cycle body = do
  label <- getNewLabel
  modLabelStack (label:) $ do
    labelWith label $ do
      body
      goto label

ifte :: Cmp -> GHCM () -> GHCM () -> GHCM ()
ifte cmp t f = do
  withLabel $ \label -> do
    ifNot cmp $ do
      f
      gotoAfter label
    t

breakIf :: Cmp -> GHCM ()
breakIf cmp = do
  (label:_) <- getLabelStack
  cmd $ BreakIf cmp label

continueIf :: Cmp -> GHCM ()
continueIf cmp = do
  (label:_) <- getLabelStack
  cmd $ JumpIf cmp label

ifNot :: Cmp -> GHCM () -> GHCM ()
ifNot cmp thenBranch = do
  withLabel $ \label -> do
    cmd $ BreakIf cmp label
    thenBranch

halt :: GHCM ()
halt = cmd Hlt

inifinity :: Int
inifinity = 255
