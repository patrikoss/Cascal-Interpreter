module Main where
 
import LexCascal
import ParCascal
import AbsCascal
import ErrM

import Control.Monad.Trans.State.Strict
import qualified Data.Map as M
import Interpreter
import Validator

import System.Environment
import System.IO
import System.IO.Error
import Control.Exception    
    
main :: IO ()
main = getInput `catch` handler

getInput :: IO ()
getInput = do
    args <- getArgs
    case args of
        [] -> do
            source <- getContents
            interpretProgram source
        [filename] -> do
            source <- readFile filename
            interpretProgram source
        otherwise -> error "Too many command line arguments"

interpretProgram :: String -> IO ()
interpretProgram source = do
    case pProgram $ myLexer source of

        Ok prog -> flip catches [Handler handleValidatorError, Handler handleRuntimeError] 
            (do
                typeCheck <- evalStateT (validateProg prog) (M.empty :: M.Map Ident VType)
                interpret <- evalStateT (evalProg prog) (M.empty :: M.Map Ident Loc, M.empty :: M.Map Loc IVal, 0)
                return ())
        Bad err -> error $ "Whoops. Syntax error. " ++ err


handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = error "The file doesn't exist!"  
    | otherwise = ioError e
