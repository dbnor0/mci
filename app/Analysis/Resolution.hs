{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Analysis.Resolution where

import           Analysis.Environment
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Containers.ListUtils
import           Data.Foldable
import           Data.List
import qualified Data.Map                  as M
import           Data.Maybe
import qualified Data.Text                 as T
import           Lens.Micro.Platform
import qualified Syntax.Syntax             as S
import qualified Syntax.Utils              as S

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

uniqueIds :: (MonadError ResError m, MonadState Env m) => S.Expr -> m ()
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
      env <- get
      let currentIds = S.Identifier <$> id : M.keys (head $ env ^. (varEnv . runSymbolTable))
      unless (unique currentIds) (throwError $ DupVarIds currentIds)
      uniqueIds start
      >> uniqueIds end
      >> uniqueIds body
    S.LetE decls exprs -> do
      ensureUnique decls
      traverse_ uniqueIds exprs
    _ -> do
      return ()

uniqueIdsLV :: (MonadError ResError m, MonadState Env m) => S.LValue -> m ()
uniqueIdsLV =
  \case
    S.MemberAccessLV lv _ -> uniqueIdsLV lv
    S.SubscriptLV lv e ->
      uniqueIdsLV lv
      >> uniqueIds e
    _ -> return ()

ensureUnique :: (MonadError ResError m, MonadState Env m) => [S.Decl] -> m ()
ensureUnique decls = do
  unless (unique aliases) (throwError $ DupAliasIds aliases)
  unless (unique fns) (throwError $ DupFnIds fns)
  unless (unique vars) (throwError $ DupVarIds vars)
  traverse_ (\args -> unless (unique args) (throwError $ DupFnArgIds args)) fnArgs
  traverse_ uniqueIds exprs
  where aliases = catMaybes $ getTypeId <$> decls
        vars = catMaybes $ getVarId <$> decls
        fns = catMaybes $ getFnId <$> decls
        fnArgs = getArgIds <$> decls
        exprs = catMaybes $ getExpr <$> decls

updateEnv :: [S.Decl] -> Env -> Env
updateEnv decls env = foldr insertToEnv env decls

unique :: [S.Identifier] -> Bool
unique ids = length ids == length (nubOrd ids)

validReferences :: MonadError ResError m => MonadState Env m => S.Expr -> m ()
validReferences =
  \case
    S.ArrayE (S.Identifier id) size defaultValue -> do
      defined <- gets (isJust . lookupType id)
      unless defined
        (throwError $ UndefinedRef (S.Identifier id))
      validReferences size
      validReferences defaultValue
    S.RecordE (S.Identifier id) rfields -> do
      traverse_ validReferences (S.rfieldVal <$> rfields)
      recType <- gets (lookupType id)
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
      defined <- gets (isJust . lookupFn id)
      syms <- gets getAllSyms
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
    S.LetE decls exprs -> do
      env <- get
      modify enterScope
      modify (\e -> foldr insertToEnv e decls)
      traverse_ validReferencesDecl decls
      traverse_ validReferences exprs
      modify exitScope
    _ -> do
      return ()

validReferencesDecl :: MonadError ResError m => MonadState Env m => S.Decl -> m ()
validReferencesDecl =
  \case 
    (S.VarDecl _ varId e) -> do
      validReferencesReturnType varId
      validReferences e
    (S.FunctionDecl id args r body) -> do
      validReferencesReturnType r
    (S.PrimitiveDecl id args r) -> do
      validReferencesReturnType r
    (S.TypeAliasDecl id t) -> return ()

validReferencesReturnType :: MonadError ResError m => MonadState Env m => Maybe S.Identifier -> m ()
validReferencesReturnType varId = do
  case varId of
    Nothing -> return ()
    Just (S.Identifier id) -> do
      defined <- gets (isJust . lookupType id)
      unless defined 
        (throwError $ UndefinedRef (S.Identifier id))

validReferencesLV :: MonadError ResError m => MonadState Env m => S.LValue -> m ()
validReferencesLV =
  \case 
    (S.IdentifierLV (S.Identifier id)) -> do
      defined <- gets (isJust . lookupVar id)
      unless defined 
        (throwError $ UndefinedRef (S.Identifier id))
    (S.MemberAccessLV lv id) -> validReferencesLV lv      
    (S.SubscriptLV lv e) ->
      validReferencesLV lv
      >> validReferences e