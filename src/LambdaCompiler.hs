
import qualified Data.Text.Lazy.IO as T
import LispMachine.Flatten
import LispMachine.Print
import Scheme.Compiler
import Scheme.Parse

main :: IO ()
main = do
  src <- T.getContents
  let prog = either error id $ parseSexp src
  putStrLn $ showProgram $ flatten $ compileProg prog

