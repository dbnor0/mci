module Main where

import Analysis.Environment 
import Analysis.Resolution (ResError, uniqueIds, validReferences)
import Control.Monad.Except (runExceptT, MonadError)
import Control.Monad.State (execState, evalState, MonadState, modify)
import Data.Text.IO         as T (readFile)
import Parser.Parser
import Syntax.Syntax        as S
import Text.Megaparsec

run :: (MonadError ResError m, MonadState Env m) => Expr -> m ()
run prg = do
  uniqueIds prg
  validReferences prg

main :: IO ()
main = do
  p <- T.readFile "./examples/program"
  case runParser program "" p of
    Left err -> print err
    Right e -> print $ evalState (runExceptT (run e)) prelude