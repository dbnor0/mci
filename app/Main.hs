module Main where

import qualified Data.Text.IO as T
import           Introduction.Syntax
import           Introduction.Interpreter (maxPrintArgs, interpStmt, EvalError, mkEnv, Env)
import           Introduction.Parser (stmt, seqExpr, opExpr)
import           Text.Megaparsec (runParser)
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

interp :: Stmt -> Env
interp ast = execState (runExceptT $ interpStmt ast) mkEnv

main :: IO ()
main = do
  program <- T.readFile "program"
  case runParser stmt "" program of
    Left err -> print err
    Right ast -> do
      print $ interp ast
