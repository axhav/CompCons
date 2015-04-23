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
import CodeGen



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
                --putStrLn $ printTree a
                let code = codeGen a
                let llvmFile = replaceExtension file ".ll"
                writeFile llvmFile code
                pAs <- runProcess "llvm-as" [llvmFile] Nothing Nothing Nothing (Just stderr) Nothing
                exitCodeAs <- waitForProcess pAs
                case exitCodeAs of
                    ExitFailure i -> do
                        hPutStrLn stderr "ERROR"
                        putStrLn "llvm assembler ERROR"
                        exitWith exitCodeAs
                    ExitSuccess   -> do
                        let llvmLinkBC = replaceExtension llvmFile ".bc"
                        pLink <- runProcess "llvm-link" [llvmLinkBC, "./lib/runtime.bc", "-o", llvmLinkBC] Nothing Nothing Nothing (Just stderr) Nothing
                        exitCodeLINK <- waitForProcess pLink
                        case exitCodeLINK of
                            ExitFailure i -> do
                                hPutStrLn stderr "ERROR"
                                putStrLn "llvm link ERROR. Must run jlc when you are located in the main folder."
                                exitWith exitCodeAs
                            ExitSuccess   -> do
                                pLLC <- runProcess "llc" ["-filetype=obj", llvmLinkBC] Nothing Nothing Nothing (Just stderr) Nothing
                                waitForProcess pLLC
                                let llvmGCC = replaceExtension llvmFile ".o"
                                pGCC <- runProcess "gcc" [llvmGCC] Nothing Nothing Nothing (Just stderr) Nothing
                                waitForProcess pGCC
                                hPutStrLn stderr "OK"
                                exitWith $ ExitSuccess
      
      
      
      
      
