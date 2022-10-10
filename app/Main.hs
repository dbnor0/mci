module Main where

import qualified Data.Text.IO as T
import           Introduction.Syntax
import           Introduction.Interpreter (maxPrintArgs, interpStmt, EvalError)
import           Introduction.Parser (stmt, seqExpr, opExpr)
import           Text.Megaparsec (runParser)
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

interp :: Stmt -> Either EvalError ()
interp ast = evalState (runExceptT $ interpStmt ast) M.empty

main :: IO ()
main = do
  program <- T.readFile "program"
  case runParser stmt "" program of
    Left err -> print err
    Right ast -> print $ interp ast
