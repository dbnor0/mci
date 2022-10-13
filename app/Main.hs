module Main where

import qualified Data.Text.IO as T
import           Introduction.Syntax
import           Introduction.Interpreter as IState
import           Introduction.InterpreterIO as IIO
import           Introduction.Parser (stmt, seqExpr, opExpr)
import           Text.Megaparsec (runParser)
import qualified Data.Map as M
import           Control.Monad.State
import           Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Lens.Micro.Platform

interpState :: Stmt -> IState.Env
interpState ast = execState (runExceptT $ IState.interpStmt ast) IState.mkEnv

interpIO :: Stmt -> IO ()
interpIO ast = do
  env <- IIO.mkEnv
  runReaderT (runExceptT $ IIO.interpStmt ast) env
  return ()

main :: IO ()
main = do
  program <- T.readFile "./app/Introduction/examples/program"
  -- either print (print . interpState) (runParser stmt "" program)
  either print interpIO (runParser stmt "" program)
