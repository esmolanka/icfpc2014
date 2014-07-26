
import LispMachine.Gen

program :: GenM ()
program = do
  fn1 <- mkLabel
  ldc  21
  ldf  $ fn1
  ap   1
  rtn
  block fn1 $ do
    ld   0 0
    ld   0 0
    add
    ldc 0
    cons
    rtn

test :: GenM () -> IO ()
test = putStrLn . genToString

main :: IO ()
main = test program
