module Syntax.Syntax where

import Data.Text as T
import Data.ByteString.Lazy.Char8

data Program
  = ExprP Expr
  | DeclP [Decl]
  deriving (Eq, Show)

data Expr
  = NilE
  | IntE Int
  | StringE T.Text
  | ArrayE Identifier Expr Expr
  | RecordE Identifier [RecordField]
  | LValueE LValue
  | NegE Expr
  | OpE Operator Expr Expr
  | ChainE [Expr]
  | FunctionCallE Identifier [Expr]
  | AssignmentE LValue Expr
  | IfE Expr Expr (Maybe Expr)
  | WhileE Expr Expr
  | ForE Identifier Expr Expr Expr
  | BreakE 
  | LetE [Decl] [Expr]
  deriving (Eq, Show)

data Decl
  = TypeAliasDecl Identifier Type
  | FunctionDecl Identifier [TypeField] (Maybe Identifier) Expr
  | PrimitiveDecl Identifier [TypeField] (Maybe Identifier)
  | VarDecl Identifier (Maybe Identifier) Expr
  deriving (Eq, Show)

data RecordField
  = RecordField Identifier Expr
  deriving (Eq, Show)

data LValue
  = IdentifierLV Identifier
  | MemberAccessLV LValue Identifier
  | SubscriptLV LValue Expr
  deriving (Eq, Show)

newtype Identifier
  = Identifier T.Text
  deriving (Eq, Show, Ord)

type TypeId = Int

data Type
  = TypeIdentifier Identifier
  | RecordType [TypeField] TypeId
  | ArrayType Identifier TypeId
  deriving (Eq, Show)

data TypeField = 
  TypeField Identifier Identifier
  deriving (Eq, Show)
  
data Operator
  = PlusOp
  | MinusOp 
  | TimesOp
  | DivOp
  | EqualsOp
  | NotEqualsOp
  | LtOp
  | GtOp
  | LteOp
  | GteOp
  | AndOp
  | OrOp
  deriving (Eq, Show)


