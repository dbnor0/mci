module Main where

import qualified Data.Text.IO as T
import           Data.Text.Encoding
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
import Lexer.Lexer
import Data.ByteString.Lazy.Internal



interpState :: Stmt -> IState.Env
interpState ast = execState (runExceptT $ IState.interpStmt ast) IState.mkEnv

interpIO :: Stmt -> IO ()
interpIO ast = do
  env <- IIO.mkEnv
  runReaderT (runExceptT $ IIO.interpStmt ast) env
  return ()

scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go


main :: IO ()
main = do
  program <- readFile "./app/Introduction/examples/program"
  print $ scanMany (packChars program)
  -- either print (print . interpState) (runParser stmt "" program)
  -- either print interpIO (runParser stmt "" program)
