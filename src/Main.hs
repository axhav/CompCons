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
import qualified CodeGenX86



main :: IO ()
main = do
    args <- getArgs
    when ((length args) < 1) $ fail "The first argument must be the path to the Javalette file."
    arch <- case (length args) > 1 of
        True -> return (args !! 1)
        False -> return "LLVM"
    readFile (args !! 0) >>= check (args !! 0) arch


check :: FilePath -> String -> String -> IO ()
check file arch s = case pProgram (myLexer s) of
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
                case arch == "-x86" of
                    True -> do
                        let code = CodeGenX86.codeGen a
                        let asmFile = replaceExtension file ".asm"
                        writeFile asmFile code
                        pAs <- runProcess "nasm" ["-f", "elf", asmFile] Nothing Nothing Nothing (Just stderr) Nothing
                        exitCodeAs <- waitForProcess pAs
                        case exitCodeAs of
                            ExitFailure i -> do
                                hPutStrLn stderr "ERROR"
                                putStrLn "nasm assembler ERROR"
                                exitWith exitCodeAs
                            ExitSuccess   -> do
                                pLLC <- runProcess "llc" ["-filetype=asm", "-march=x86", "./lib/runtime.bc"] Nothing Nothing Nothing (Just stderr) Nothing
                                waitForProcess pLLC
                                let asmGCC = replaceExtension asmFile ".o"
                                pGCC <- runProcess "gcc" [asmGCC, "./lib/runtime.s", "-m32"] Nothing Nothing Nothing (Just stderr) Nothing
                                waitForProcess pGCC
                                hPutStrLn stderr "OK"
                                exitWith $ ExitSuccess
                    False -> do
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
                                pOpt <- runProcess "opt" [llvmLinkBC, "-std-compile-opts", "-o", llvmLinkBC] Nothing Nothing Nothing (Just stderr) Nothing
                                waitForProcess pOpt                        
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
      
      
      
      
      
