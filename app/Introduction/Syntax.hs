module Introduction.Syntax where

import Data.Text

type Ident = Text

data BinOp
  = Plus
  | Minus
  | Times
  | Div
  deriving (Eq, Show)

data Stmt 
  = CompoundStmt Stmt Stmt
  | AssignStmt Ident Exp
  | PrintStmt [Exp]
  deriving (Eq, Show)

data Exp 
  = IdentExp Ident
  | NumExp Int
  | OpExp Exp BinOp Exp
  | SeqExp Stmt Exp
  deriving (Eq, Show)