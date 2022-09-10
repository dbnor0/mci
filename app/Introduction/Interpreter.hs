module Introduction.Interpreter where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           Data.Maybe (fromMaybe)
import qualified Data.Text                  as T
import           Introduction.Syntax

type Env = M.Map Ident Int

data EvalError
  = IdNotDefined
  | DivisionByZero

type Eval a = ExceptT EvalError (State Env) a

maxPrintArgs :: Stmt -> Int
maxPrintArgs (CompoundStmt s1 s2) = max (maxPrintArgs s1) (maxPrintArgs s2)
maxPrintArgs (AssignStmt _ e)     = expArgs e
maxPrintArgs (PrintStmt es)       = max (length es) (maximum $ expArgs <$> es)
maxPrintArgs NoOpStmt             = 0

expArgs :: Exp -> Int
expArgs (SeqExp (p@(PrintStmt _), _)) = maxPrintArgs p
expArgs _                          = 0

interp :: Stmt -> IO ()
interp  = undefined

interpStmt :: (MonadState Env m, MonadError EvalError m) => Stmt -> m ()
interpStmt =
  \case
    (CompoundStmt s1 s2) -> do
      interpStmt s1
      interpStmt s2
    (AssignStmt id e) -> do
      v <- interpExp e
      modify (M.insert id v)
    (PrintStmt es) -> undefined
    NoOpStmt -> undefined

interpExp :: (MonadState Env m, MonadError EvalError m) => Exp -> m Int
interpExp =
  \case
    (IdentExp ident) -> interpIdentExp ident
    (NumExp n) -> return n
    (OpExp op e1 e2) -> interpOpExp op e1 e2
    (SeqExp (s, e)) -> do
      interpStmt s
      interpExp e

interpIdentExp :: (MonadState Env m, MonadError EvalError m) => Ident -> m Int
interpIdentExp ident = do
  fromMaybe (throwError IdNotDefined) (M.lookup ident M.empty)

interpOpExp :: (MonadState Env m, MonadError EvalError m) => BinOp -> Exp -> Exp -> m Int
interpOpExp op e1 e2 = do
  r1 <- interpExp e1
  r2 <- interpExp e2
  when (r2 == 0 && op == Div) (throwError DivisionByZero)
  return $ interpOp op r1 r2

interpOp :: BinOp -> (Int -> Int -> Int)
interpOp Plus = (+)
interpOp Minus = (-)
interpOp Times = (*)
interpOp Div = div
