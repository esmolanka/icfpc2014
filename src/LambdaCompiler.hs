
import Control.Monad

import System.Environment
import System.Directory
import System.FilePath

import Control.Applicative

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import LispMachine.Flatten
import LispMachine.Print

import Scheme.Backend
import Scheme.Frontend
import Scheme.Types

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
  T.putStrLn $ showSchemeProg optimized
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
  let printHelpMode = "-h" `elem` args

  when (printHelpMode) $ do
         error "LambdaCompiler [-h|-d] [filename.scm]\n\
               \If filename is omitted, stdin is used."

  let filenames = filter ((/= '-') . head) args

  (preludeFns, src') <-
      case filenames of
        (fn:_) -> do
          src <- T.readFile fn
          let preludeFns = [ replaceBaseName fn "prelude", prelude ]
          return (preludeFns, src)
        [] -> do
          src <- T.getContents
          return ([prelude], src)

  havePrelude <- filter (snd) <$> mapM (\fn -> (,) <$> pure fn <*> doesFileExist fn) preludeFns
  src <- case map fst havePrelude of
           (fn:_) -> T.readFile fn >>= return . T.append src'
           []     -> return src'

  if printDebugMode
     then debugScheme src
     else compileScheme src

