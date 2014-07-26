
import LispMachine.Parse
import LispMachine.Print
import LispMachine.Flatten

main :: IO ()
main = do
  src <- getContents
  let program = either error id (flatten =<< parseLispAsm "<stdin>" src)
  putStrLn $ showProgram program
