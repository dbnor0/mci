{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Analysis.Resolution where

import Analysis.Environment
import qualified Data.Text as T
import qualified Syntax.Syntax as S
import qualified Syntax.Utils as S
import           Control.Monad.Except
import           Control.Monad.Reader
import Data.Containers.ListUtils
import Syntax.Utils (rfieldVal)
import Data.Foldable
import qualified Data.Map as M
import Lens.Micro.Platform
import Utils.Text
import Data.Maybe (catMaybes, isJust)
import Syntax.Syntax (Decl(VarDecl))
import Data.List

data ResError
  = DupRecFields [S.RecordField]
  | DupAliasIds [S.Identifier]
  | DupFnIds [S.Identifier]
  | DupVarIds [S.Identifier]
  | DupFnArgIds [S.Identifier]
  | UndefinedRef S.Identifier
  | InvalidRecFields [S.Identifier] [S.Identifier]
  deriving (Eq, Show)

type NameRes a = ExceptT ResError (Reader Env) a

-- check that identifiers are unique on a scope basis
-- check that references to identifiers are valid
-- resolve type aliases to base types


uniqueIds :: (MonadError ResError m, MonadReader Env m) => S.Expr -> m ()
uniqueIds =
  \case
    S.ArrayE _ size defaultValue ->
      uniqueIds size
      >> uniqueIds defaultValue
    S.RecordE id fields -> do
      unless (unique fieldNames) (throwError $ DupRecFields fields)
      traverse_ uniqueIds fieldValues
      where fieldNames = S.rfieldName <$> fields
            fieldValues = S.rfieldVal <$> fields
    S.NegE e -> uniqueIds e
    S.OpE _ e1 e2 ->
      uniqueIds e1
      >> uniqueIds e2
    S.ChainE es -> traverse_ uniqueIds es
    S.FunctionCallE _ args -> traverse_ uniqueIds args
    S.AssignmentE _ e -> uniqueIds e
    S.IfE cond b1 b2 ->
      uniqueIds cond
      >> uniqueIds b1
      >> maybe (return ()) uniqueIds b2
    S.WhileE cond body ->
      uniqueIds cond
      >> uniqueIds body
    S.ForE (S.Identifier id) start end body -> do
      env <- ask
      let currentIds = S.Identifier <$> id : M.keys (head $ env ^. (varEnv . runSymbolTable))
      unless (unique currentIds) (throwError $ DupVarIds currentIds)
      uniqueIds start
      >> uniqueIds end
      >> uniqueIds body
    S.LetE decls exprs -> do
      asks enterScope
      ensureUnique decls
      updateEnv decls
      asks exitScope
      traverse_ uniqueIds exprs
    _ -> do
      return ()

uniqueIdsLV :: (MonadError ResError m, MonadReader Env m) => S.LValue -> m ()
uniqueIdsLV =
  \case
    S.MemberAccessLV lv _ -> uniqueIdsLV lv
    S.SubscriptLV lv e ->
      uniqueIdsLV lv
      >> uniqueIds e
    _ -> return ()

ensureUnique :: (MonadError ResError m) => [S.Decl] -> m ()
ensureUnique decls = do
  unless (unique aliases) (throwError $ DupAliasIds aliases)
  unless (unique fns) (throwError $ DupFnIds fns)
  unless (unique vars) (throwError $ DupVarIds vars)
  traverse_ (\args -> unless (unique args) (throwError $ DupFnArgIds args)) fnArgs
  where aliases = catMaybes $ getTypeId <$> decls
        vars = catMaybes $ getVarId <$> decls
        fns = catMaybes $ getFnId <$> decls
        fnArgs = getArgIds <$> decls

updateEnv :: (MonadReader Env m) => [S.Decl] -> m ()
updateEnv = traverse_ (asks . insertToEnv)

unique :: [S.Identifier] -> Bool
unique ids = length ids == length (nubOrd ids)

validReferences :: (MonadError ResError m, MonadReader Env m) => S.Expr -> m ()
validReferences =
  \case
    S.ArrayE (S.Identifier id) size defaultValue -> do
      defined <- asks (isJust . lookupType id)
      unless defined
        (throwError $ UndefinedRef (S.Identifier id))
      validReferences size
      validReferences defaultValue
    S.RecordE (S.Identifier id) rfields -> do
      traverse_ validReferences (S.rfieldVal <$> rfields)
      recType <- asks (lookupType id)
      case recType of
        Nothing -> throwError $ UndefinedRef (S.Identifier id)
        Just (S.RecordType tfields _) -> do
          unless (sort rnames == sort tnames)
            (throwError $ InvalidRecFields tnames rnames)
          where tnames = S.tfieldName <$> tfields
                rnames = S.rfieldName <$> rfields
        -- handled by typechecker
        _ -> return ()
    S.LValueE lv -> validReferencesLV lv
    S.NegE e -> validReferences e
    S.OpE _ e1 e2 ->
      validReferences e1
      >> validReferences e2
    S.ChainE es -> traverse_ validReferences es
    S.FunctionCallE (S.Identifier id) args -> do
      defined <- asks (isJust . lookupFn id)
      unless defined 
          (throwError $ UndefinedRef (S.Identifier id))
      traverse_ validReferences args
    S.AssignmentE lv e -> 
      validReferencesLV lv
      >> validReferences e
    S.IfE cond b1 b2 ->
      validReferences cond
      >> validReferences b1
      >> maybe (return ()) validReferences b2
    S.WhileE cond body ->
      validReferences cond
      >> validReferences body
    S.ForE _ start end body ->
      validReferences start
      >> validReferences end
      >> validReferences body
    S.LetE decls exprs -> return ()
    _ -> do
      return ()

{-

  let 
    type r1 = { x: int, y: int },
    type r2 = { a: r1, b: string },
    var x = r2{ a = r1{ x = 1, y = 2}, b = "yo" }
  in
    x = r2{ a = r1{ x = 1, y = 2}, b = "yo" }
  end

  lvalue 
    ::= id
    | lvalue . id
    | 
-}

validReferencesLV :: (MonadError ResError m, MonadReader Env m) => S.LValue -> m ()
validReferencesLV =
  \case 
    (S.IdentifierLV (S.Identifier id)) -> do
      defined <- asks (isJust . lookupVar id)
      unless defined 
        (throwError $ UndefinedRef (S.Identifier id))
    (S.MemberAccessLV lv id) -> do
      defined <- 
      validReferencesLV lv

    (S.SubscriptLV lv e) -> return ()