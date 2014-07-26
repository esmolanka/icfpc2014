
import Control.Monad

import System.Environment

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

compileScheme :: T.Text -> IO ()
compileScheme src =
    case compile src of
      Left msg -> error $ "Compilation error: \n" ++ msg
      Right prg -> putStrLn prg

debugScheme :: T.Text -> IO ()
debugScheme src = do
  let parsed = either error id $ parseSexp src
  putStrLn "Parsed: "
  print parsed
  putStrLn "\n----\n"
  let optimized = optimize . desugar $ parsed
  putStrLn "Optimized: "
  print optimized
  putStrLn "\n----\n"
  let compiled = either error id $ compileProg optimized
  putStrLn "Compiled: "
  print compiled

prelude :: FilePath
prelude = "prelude.scm"

main :: IO ()
main = do
  args <- getArgs
  let printDebugMode = "-d" `elem` args

  src <- T.getContents
  havePrelude <- doesFileExist prelude
  src' <- if havePrelude
          then T.readFile prelude >>= return . T.append src
          else return src

  if printDebugMode
     then debugScheme src'
     else compileScheme src'

