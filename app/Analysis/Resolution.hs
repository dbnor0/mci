{-# LANGUAGE LambdaCase #-}

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
import Lens.Micro.Platform
import Utils.Text

data ResError
  = DupRecFields [S.RecordField]
  | DupAliasIds [S.Identifier]
  | DupFnIds [S.Identifier]
  | DupVarIds [S.Identifier]
  | DupFnArgIds [S.Identifier]

type TC a = ExceptT ResError (Reader Env) a

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
    S.ForE _ start end body -> 
      uniqueIds start
      >> uniqueIds end
      >> uniqueIds body 
    S.LetE decls exprs -> do
      asks enterScope
      ensureUnique decls
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
     

aliasIds :: S.Decl -> [S.Identifier]
aliasIds (S.TypeAliasDecl id _) = [id]
aliasIds _                      = []

functionIds :: S.Decl -> [S.Identifier]
functionIds (S.FunctionDecl id _ _ _) = [id]
functionIds (S.PrimitiveDecl id _ _ ) = [id]
functionIds _                         = []

functionArgIds :: S.Decl -> [S.Identifier]
functionArgIds (S.FunctionDecl _ fields _ _) = S.tfieldName <$> fields
functionArgIds (S.PrimitiveDecl _ fields _) = S.tfieldName <$> fields
functionArgIds _ = []

varIds :: S.Decl -> [S.Identifier]
varIds (S.VarDecl id _ _) = [id]
varIds _                  = []

getIds :: [S.Decl] -> ([S.Identifier], [S.Identifier], [S.Identifier], [[[S.Identifier]]])
getIds [] = []
getIds _ = []

ensureUnique :: (MonadError ResError m) => [S.Decl] -> m ()
ensureUnique decls = do
  unless (unique aliases) (throwError $ DupAliasIds aliases)
  unless (unique fns) (throwError $ DupFnIds fns)
  unless (unique vars) (throwError $ DupVarIds vars)
  traverse_ (\args -> unless (unique args) (throwError $ DupFnArgIds args)) fnArgs
  where aliases = decls >>= aliasIds
        fns = decls >>= functionIds
        vars = decls >>= varIds
        fnArgs = decls <&> functionArgIds 

updateEnv :: (MonadReader Env m) => [S.Decl] -> m ()
updateEnv decls = do
  traverse_ (\asks (insertType Text Type Env)
  where aliases = decls >>= aliasIds
        fns = decls >>= functionIds
        vars = decls >>= varIds
        fnArgs = decls <&> functionArgIds 


unique :: [S.Identifier] -> Bool
unique ids = length ids == length (nubOrd ids)
  