
import Control.Monad

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Directory

import LispMachine.Flatten
import LispMachine.Print

import Scheme.Backend
import Scheme.Frontend

compile :: T.Text -> Either String String
compile = parseSexp
      >=> compileProg . optimize . desugar
      >=> flatten
      >=> (return . showProgram)

prelude :: FilePath
prelude = "prelude.scm"

main :: IO ()
main = do
  src <- T.getContents
  havePrelude <- doesFileExist prelude
  src' <- if havePrelude
          then T.readFile prelude >>= return . T.append src
          else return src
  case compile src' of
    Left msg -> error $ "Compilation error: \n" ++ msg
    Right prg -> putStrLn prg

