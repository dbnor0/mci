{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Eval.InterpreterIO where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.IORef
import qualified Data.Map                   as M
import           Data.Maybe (fromMaybe)
import qualified Data.Text                  as T
import           Eval.Syntax
import           Lens.Micro.Platform
import           Data.Foldable


data Env = Env
  { _symbols :: IORef (M.Map Ident Int) } 
  deriving (Eq)

makeLenses ''Env

mkEnv :: IO Env
mkEnv = do
  s <- newIORef M.empty
  o <- newIORef ""
  return $ Env { _symbols = s }


data EvalError
  = IdNotDefined
  | DivisionByZero
  deriving (Eq, Show)

type Eval a = ExceptT EvalError (ReaderT Env IO) a

interpStmt :: (MonadError EvalError m, MonadReader Env m, MonadIO m) => Stmt -> m ()
interpStmt =
  \case
    (CompoundStmt s1 s2) -> do
      interpStmt s1
      interpStmt s2
    (AssignStmt id e) -> do
      v <- interpExp e
      env <- ask
      liftIO $ modifyIORef (env ^. symbols) (M.insert id v)
    (PrintStmt es) -> do
      vals <- traverse interpExp es
      liftIO $ print vals
    NoOpStmt -> return ()


interpExp :: (MonadError EvalError m, MonadReader Env m, MonadIO m) => Exp -> m Int
interpExp =
  \case
    (IdentExp ident) -> interpIdentExp ident
    (NumExp n) -> return n
    (OpExp op e1 e2) -> interpOpExp op e1 e2
    (SeqExp (s, e)) -> do
      interpStmt s
      interpExp e

interpIdentExp :: (MonadError EvalError m, MonadReader Env m, MonadIO m) => Ident -> m Int
interpIdentExp ident = do
  env <- ask
  symbols <- liftIO $ readIORef (env ^. symbols)
  maybe (throwError IdNotDefined) return (M.lookup ident symbols)

interpOpExp :: (MonadError EvalError m, MonadReader Env m, MonadIO m) => BinOp -> Exp -> Exp -> m Int
interpOpExp op e1 e2 = do
  r1 <- interpExp e1
  r2 <- interpExp e2
  when (r2 == 0 && op == Div) 
    (throwError DivisionByZero)
  return $ interpOp op r1 r2

interpOp :: BinOp -> (Int -> Int -> Int)
interpOp Plus = (+)
interpOp Minus = (-)
interpOp Times = (*)
interpOp Div = div
