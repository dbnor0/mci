module Main where

import qualified Data.Text.IO as T
import           Introduction.Syntax
import           Introduction.Interpreter (maxPrintArgs)
import           Introduction.Parser (stmt, seqExpr, opExpr)
import           Text.Megaparsec (runParser)

main :: IO ()
main = do
  program <- T.readFile "program"
  print $ runParser stmt "" program
