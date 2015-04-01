import System.IO
import System.FilePath.Posix
import System.Environment
import System.Exit
import System.Process

import Control.Monad.Reader

import AbsJavalette
import ParJavalette
import LexJavalette
import PrintJavalette
import ErrM

import TypeChecker



main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= check file
    _      -> do
      putStrLn "Usage: lab3 <SourceFile>"
      exitFailure




check :: FilePath -> String -> IO ()
check file s = case pProgram (myLexer s) of
  Bad err  -> do
    hPutStrLn stderr "ERROR"
    putStrLn "SYNTAX ERROR"
    putStrLn err
    exitWith $ ExitFailure 1
  Ok tree -> do
    case typecheck tree of
      Bad err -> do
        hPutStrLn stderr "ERROR"
        putStrLn "TYPE ERROR"
        putStrLn err
        exitWith $ ExitFailure 1
      Ok a -> do 
        hPutStrLn stderr "OK"
        putStrLn "OK"
        putStrLn $ printTree a
        exitWith $ ExitSuccess
      
      
      
      
      
