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
import Data.Maybe (catMaybes)
import Syntax.Syntax (Decl(VarDecl))

data ResError
  = DupRecFields [S.RecordField]
  | DupAliasIds [S.Identifier]
  | DupFnIds [S.Identifier]
  | DupVarIds [S.Identifier]
  | DupFnArgIds [S.Identifier]
  deriving (Eq, Show)

type NameRes a = ExceptT ResError (Reader Env) a

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
  