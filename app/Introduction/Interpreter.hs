module Introduction.Interpreter where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           Data.Maybe (fromMaybe)
import qualified Data.Text                  as T
import           Introduction.Syntax

data Env = Env
  { symbols :: M.Map Ident Int
  , output :: T.Text
  } deriving (Eq, Show)

mkEnv :: Env
mkEnv = Env { symbols = M.empty, output = "" }

withSymbols :: M.Map Ident Int -> Env -> Env
withSymbols s env = env { symbols = s }

withOutput :: T.Text -> Env -> Env
withOutput o env = env { output = o }

data EvalError
  = IdNotDefined
  | DivisionByZero
  deriving (Eq, Show)

type Eval a = ExceptT EvalError (State Env) a

maxPrintArgs :: Stmt -> Int
maxPrintArgs (CompoundStmt s1 s2) = max (maxPrintArgs s1) (maxPrintArgs s2)
maxPrintArgs (AssignStmt _ e)     = expArgs e
maxPrintArgs (PrintStmt es)       = max (length es) (maximum $ expArgs <$> es)
maxPrintArgs NoOpStmt             = 0

expArgs :: Exp -> Int
expArgs (SeqExp (p@(PrintStmt _), _)) = maxPrintArgs p
expArgs _                          = 0

interpStmt :: (MonadState Env m, MonadError EvalError m) => Stmt -> m ()
interpStmt =
  \case
    (CompoundStmt s1 s2) -> do
      interpStmt s1
      interpStmt s2
    (AssignStmt id e) -> do
      v <- interpExp e
      env <- get
      modify (withSymbols (M.insert id v (symbols env)))
    (PrintStmt es) -> do
      logs <- gets output
      vals <- traverse interpExp es
      modify (withOutput (foldMap (T.pack . show) vals))
    NoOpStmt -> return ()

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
  env <- gets symbols
  case M.lookup ident env of
    Nothing -> throwError IdNotDefined
    Just v -> return v

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
