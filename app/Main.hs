module Main where

import Data.Text.IO as T (readFile)
import Parser.Parser
import Data.ByteString.Lazy.Internal
import Text.Megaparsec
import Syntax.Syntax (Expr (..), Operator (..))
import Analysis.Environment
import Control.Monad.Reader (ReaderT(runReaderT), runReader)
import Analysis.Resolution (NameRes, uniqueIds, ResError)
import Control.Monad.Except (runExceptT)

validate :: Expr -> Either ResError ()
validate prg = runReader (runExceptT (uniqueIds prg)) prelude

main :: IO ()
main = do
  p <- T.readFile "./examples/program"
  case runParser program "" p of
    Left err -> print err
    Right e -> print $ validate e