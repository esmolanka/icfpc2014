
import LispMachine.Gen

program :: GenM e ()
program = do
  fn1 <- mkLabel
  dum 2
  ldc 42
  ldc 4
  ldf fn1
  rap 2
  rtn
  block fn1 $ do
    ld 0 0
    ld 0 0
    add
    ldc 0
    ldf fn1
    rap 2
    rtn

outer :: GenM e ()
outer = do
  r <- mkLabel
  ldc  100
  ldf r
  ap 1
  ldc  200
  rtn
  block r recur

recur :: GenM e ()
recur = do
  main <- mkLabel
  to   <- mkLabel
  go   <- mkLabel
  ld   0 0
  dum  2        -- 2 top-level declarations
  ldf  go       -- declare function go
  ldf  to       -- declare function to
  ldf  main     -- main function
  trap  2        -- load declarations into environment and run main
  rtn           -- final return
  block main $ do
    ldc   3
    ld    0 0      -- var go
    tap   1        -- call go(1)
    rtn
  block to $ do
    t <- mkLabel
    f <- mkLabel
    ld   0 0      -- var n
    ldc  2
    sub
    tsel t f
    block t $ do
      ld   0 0
      ldc  2
      sub
      ld   1 0       -- var go
      tap  1         -- call go(n-1)
    block f $ do
      rtn
  block go $ do
    ld   0 0      -- var n
    ldc  1
    add
    ld   1 1      -- var to
    tap   1        -- call to(n+1)
    rtn


recur2 :: GenM e ()
recur2 = do
  to   <- mkLabel
  go   <- mkLabel
  ldc  1
  ldf  go
  ldf  to       -- main function
  ap   2        -- load declarations into environment and run main
  rtn           -- final return
  block to $ do
    ld   0 0      -- var n
    ldc  1
    sub
    ldf  to
    ld   0 1
    ap   2
    rtn
  block go $ do
    ld   0 0      -- var n
    ldc  1
    add
    ldf  go
    ld   0 1
    ap   2        -- call to(n+1)
    rtn

test :: GenM () () -> IO ()
test = putStrLn . genToString initEnv
