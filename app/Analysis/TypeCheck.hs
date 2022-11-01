
module Analysis.TypeCheck where

import Analysis.Environment
import qualified Data.Text as T
import qualified Syntax.Syntax as S
import qualified Syntax.Utils as S
import           Control.Monad.Except
import           Control.Monad.Reader
import Utils.Text
import Lens.Micro.Platform
import Data.Foldable
import Syntax.Syntax (Type(ArrayType))
import qualified Data.Map as M
import Syntax.Utils (typeId)


type TCError = T.Text
type TC a = ExceptT TCError (Reader Env) a

typecheckExpr :: (MonadError TCError m, MonadReader Env m) => S.Expr -> m S.Type
typecheckExpr S.NilE = return nilType
typecheckExpr (S.IntE _) = return intType
typecheckExpr (S.StringE _) = return stringType
typecheckExpr (S.OpE op  e1 e2)
  | S.arithOp op || S.relationalOp op = do
    t1 <- (getBaseType <=< typecheckExpr) e1
    t2 <- (getBaseType <=< typecheckExpr) e2
    unless (t1 == intType && t2 == intType)
      (throwError $ "Expected int operands to " <> showT op)
    return intType
  | S.eqOp op = do
    t1 <- (getBaseType <=< typecheckExpr) e1
    t2 <- (getBaseType <=< typecheckExpr) e2
    unless (t1 == t2)
      (throwError "Cannot compare operands of different types")
    return intType
  | S.logicalOp op = do
    t1 <- (getBaseType <=< typecheckExpr) e1
    t2 <- (getBaseType <=< typecheckExpr) e2
    unless (truthy t1)
      (throwError $ "Cannot apply logical operators to type " <> showT t1)
    unless (truthy t2)
      (throwError $ "Cannot apply logical operators to type " <> showT t2)
    return intType
  | otherwise = throwError "Unknown operator"
typecheckExpr (S.ArrayE id _ defaultValue) = do
  arrType <- (getBaseType <=< isDeclared) id
  case arrType of
    (ArrayType arrId _) -> do
      dvType <- typecheckExpr defaultValue
      nestedType <- getBaseType (S.TypeIdentifier arrId)
      unless (dvType == nestedType)
        (throwError $ "Expected array default value type to be " <> showT arrType)
      return arrType
    t -> throwError $ "Expected array type, got " <> showT t
-- Need scopes
-- type z = { a : int, b : string }
-- type x = { a : int, b : string }
-- type y = x
-- var n = z{ a = 5, b = "7" }
--

typecheckExpr (S.RecordE id fields) = do
  recordType <- (getBaseType <=< isDeclared) id
  case recordType of
    (S.RecordType tfs _) -> do
      return recordType
    t -> throwError $ "Expected record type, got " <> showT t

-- Need scopes
typecheckExpr (S.LValueE lv) = do
  return undefined
typecheckExpr (S.NegE e) = do
  t <- (getBaseType <=< typecheckExpr) e
  unless (t == intType)
    (throwError "Expected int operand to negation operator")
  return intType
typecheckExpr (S.ChainE exs) =
  traverse_ typecheckExpr exs >> return unitType
-- Need scopes
typecheckExpr (S.FunctionCallE iden exs) = do
  return undefined
typecheckExpr (S.AssignmentE lv e) = do
  t1 <- typecheckLValue lv
  t2 <- typecheckExpr e
  unless (t1 == t2)
    (throwError $ "Cannot assign value of type " <> showT t2 <> " to " <> showT t1)
  return t1
typecheckExpr (S.IfE cond e1 e2) = do
  t <- typecheckExpr cond
  t1 <- typecheckExpr e1
  unless (truthy t)
    (throwError "Expected if expression condition to be truthy")
  case e2 of
    Nothing -> return t1
    Just e2' -> do
      t2 <- typecheckExpr e2'
      unless (t1 == t2)
        (throwError "Expected if expression branches to resolve to the same type")
      return t1
typecheckExpr (S.WhileE cond e) = do
  t <- typecheckExpr cond
  unless (truthy t)
    (throwError "Expected while expression condition to be truthy")
  typecheckExpr e
  return unitType
-- Need scopes
typecheckExpr (S.ForE iden ex ex' ex2) = do
  return undefined
typecheckExpr S.BreakE = return unitType
-- Need scopes
typecheckExpr (S.LetE des exs) = do
  return undefined

isDeclared :: (MonadError TCError m, MonadReader Env m) => S.Identifier -> m S.Type
isDeclared (S.Identifier id) = do
  idType <- asks (lookupSym' id . _typeEnv)
  case idType of
    Nothing -> throwError $ "Undefined type " <> showT id
    Just t -> return t  

getBaseType :: (MonadError TCError m, MonadReader Env m) => S.Type -> m S.Type
getBaseType t@(S.TypeIdentifier (S.Identifier id'))
  | isPrimitive id' = return t
  | otherwise = getBaseType t
  where isPrimitive = (`elem` M.keys preludeTypes)
getBaseType a@(S.ArrayType _ _) = return a
getBaseType r@(S.RecordType _ _) = return r

resolves :: (MonadError TCError m, MonadReader Env m) => S.Identifier -> m ()
resolves (S.Identifier id) = do
  idType <- asks (lookupSym' id . _typeEnv)
  case idType of
    Nothing -> throwError $ "Undefined type " <> showT id
    Just t@(S.TypeIdentifier ti@(S.Identifier id'))
      | isPrimitive id' -> return ()
      | otherwise -> resolves ti
      where isPrimitive = (`elem` M.keys preludeTypes)
    Just a@(S.ArrayType aid _) -> resolves aid
    Just r@(S.RecordType fields _) ->
      traverse_ resolves (S.tfieldType <$> fields)

typecheckLValue :: (MonadError TCError m, MonadReader Env m) => S.LValue -> m S.Type
-- Need scopes
typecheckLValue (S.IdentifierLV iden) = do
  return undefined
-- Need scopes
typecheckLValue (S.MemberAccessLV lv' iden) = do
  return undefined
-- Need scopes
typecheckLValue (S.SubscriptLV lv' ex) = do
  return undefined