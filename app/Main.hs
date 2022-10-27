module Main where

import Data.Text.IO as T (readFile)
import Parser.Parser
import Data.ByteString.Lazy.Internal
import Text.Megaparsec
import Syntax.Syntax (Expr (..), Operator (..))

main :: IO ()
main = do
  p <- T.readFile "./examples/program"
  case runParser program "" p of
    Left err -> print err
    Right e -> print e