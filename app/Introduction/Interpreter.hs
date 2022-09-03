module Introduction.Interpreter where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           Data.Maybe (fromMaybe)
import qualified Data.Text                  as T
import           Introduction.Syntax

type Env   = M.Map Ident Int

data EvalError
  = IdNotDefined
  | DivisionByZero

type Eval a = ExceptT EvalError (State Env) a

maxPrintArgs :: Stmt -> Int
maxPrintArgs (CompoundStmt s1 s2) = max (maxPrintArgs s1) (maxPrintArgs s2)
maxPrintArgs (AssignStmt _ e)     = expArgs e
maxPrintArgs (PrintStmt es)       = max (length es) (maximum $ expArgs <$> es)

expArgs :: Exp -> Int
expArgs (SeqExp p@(PrintStmt _) _) = maxPrintArgs p
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

interpExp :: (MonadState Env m, MonadError EvalError m) => Exp -> m Int
interpExp =
  \case
    (IdentExp ident) -> interpIdentExp ident
    (NumExp n) -> return n
    (OpExp e1 op e2) -> interpOpExp e1 e2 op
    (SeqExp s e) -> do
      interpStmt s
      interpExp e

interpIdentExp :: (MonadState Env m, MonadError EvalError m) => Ident -> m Int
interpIdentExp ident = do
  fromMaybe (throwError IdNotDefined) (M.lookup ident get)

interpOpExp :: (MonadState Env m, MonadError EvalError m) => Exp -> Exp -> BinOp -> m Int
interpOpExp e1 e2 op = undefined

-- interpExp :: (MonadState Env m, MonadError T.Text m) => Exp -> m (Either T.Text Int)
-- interpExp =
--   \case 
--     (IdentExp ident) -> interpIdentExp ident
--     (NumExp n) -> return $ Right n
--     (OpExp e1 op e2) -> interpOpExp e1 e2 op
--     (SeqExp s e) -> do
--       interpStmt s
--       interpExp e

-- interpIdentExp :: (MonadState Env m, MonadError T.Text m) => Ident -> m (Either T.Text Int)
-- interpIdentExp = undefined

-- interpOpExp :: (MonadState Env m, MonadError T.Text m) => Exp -> Exp -> BinOp -> Eval Env Int
-- interpOpExp e1 e2 = do
--   v1 <- interpExp e1
--   v2 <- interpExp e2
--   -- case op of
--   --   Plus -> 
--   undefined