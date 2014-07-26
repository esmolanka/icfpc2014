
import Control.Monad

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import LispMachine.Flatten
import LispMachine.Print

import Scheme.Compiler
import Scheme.Parse

compile :: T.Text -> Either String String
compile = parseSexp
      >=> compileProg
      >=> flatten
      >=> (return . showProgram)

main :: IO ()
main = do
  src <- T.getContents

  case compile src of
    Left msg -> error $ "Compilation error: \n" ++ msg
    Right prg -> putStrLn prg

