{-# LANGUAGE TemplateHaskell #-}
module Analysis.Environment where

import qualified Data.Multimap as MM
import qualified Data.Text as T
import qualified Syntax.Syntax as S
import qualified Syntax.Utils as S
import qualified Data.Map as M
import Lens.Micro.Platform
import Control.Applicative
import Data.Foldable (Foldable(foldl'))
import Control.Monad
import Data.Maybe (fromMaybe)

newtype SymbolTable a = SymbolTable { _runSymbolTable :: [M.Map T.Text a] }
  deriving (Eq, Show)

makeLenses ''SymbolTable
data FnEntry = FnEntry 
  { fnArgs :: [S.Type]
  , fnReturnType :: S.Type 
  } deriving (Eq, Show)

data Env = Env
  { _typeEnv :: SymbolTable S.Type
  , _varEnv :: SymbolTable S.Type
  , _fnEnv :: SymbolTable FnEntry
  } deriving (Eq, Show)

makeLenses ''Env

class DeclId a where
  getVarId :: a -> Maybe S.Identifier
  getTypeId :: a -> Maybe S.Identifier
  getFnId :: a -> Maybe S.Identifier
  getArgIds :: a -> [S.Identifier]
  insertToEnv :: a -> Env -> Env

instance DeclId S.Decl where
  getVarId (S.VarDecl id _ _) = Just id
  getVarId _ = Nothing

  getFnId (S.FunctionDecl id _ _ _) = Just id
  getFnId (S.PrimitiveDecl id _ _) = Just id
  getFnId _ = Nothing

  getTypeId (S.TypeAliasDecl id _) = Just id
  getTypeId _ = Nothing

  getArgIds (S.FunctionDecl _ args _ _) = S.tfieldName <$> args
  getArgIds (S.PrimitiveDecl _ args _) = S.tfieldName <$> args
  getArgIds _ = []

  insertToEnv (S.VarDecl (S.Identifier id) t _) = insertVar id (S.TypeIdentifier $ fromMaybe (S.Identifier "()") t)
  insertToEnv (S.FunctionDecl (S.Identifier id) args r _) = insertFn id (S.TypeIdentifier . S.tfieldType <$> args) (S.TypeIdentifier $ fromMaybe (S.Identifier "()") r) 
  insertToEnv (S.PrimitiveDecl (S.Identifier id) args r) = insertFn id (S.TypeIdentifier . S.tfieldType <$> args) (S.TypeIdentifier $ fromMaybe (S.Identifier "()") r) 
  insertToEnv (S.TypeAliasDecl (S.Identifier id) t) = insertType id t


enterScope' :: SymbolTable a -> SymbolTable a
enterScope' st = st & runSymbolTable  %~ (M.empty :)

exitScope' :: SymbolTable a -> SymbolTable a
exitScope' st = st & runSymbolTable %~ drop 1

insertSym :: T.Text -> v -> SymbolTable v -> SymbolTable v
insertSym _ _ (SymbolTable [])     = error "No scope present in symbol table"
insertSym k v (SymbolTable (s:ss)) = SymbolTable (M.insert k v s:ss)

lookupSym :: T.Text -> SymbolTable v -> Maybe v
lookupSym k (SymbolTable ss) = foldl' (\found s -> found <|> M.lookup k s) Nothing ss

lookupSymCurrent :: T.Text -> SymbolTable v -> Maybe v
lookupSymCurrent _ (SymbolTable []) = error "No scope present in symbol table"
lookupSymCurrent k (SymbolTable (s:_)) = M.lookup k s

enterScope :: Env -> Env
enterScope (Env te ve fe) = Env 
  { _typeEnv = enterScope' te
  , _varEnv = enterScope' ve
  , _fnEnv = enterScope' fe
  }

exitScope :: Env -> Env
exitScope (Env te ve fe) = Env 
  { _typeEnv = exitScope' te
  , _varEnv = exitScope' ve
  , _fnEnv = exitScope' fe
  }

insertType :: T.Text -> S.Type -> Env -> Env
insertType id t env = env & typeEnv %~ insertSym id t  

insertVar :: T.Text -> S.Type -> Env -> Env
insertVar id t env = env & varEnv %~ insertSym id t

insertFn :: T.Text -> [S.Type] -> S.Type -> Env -> Env
insertFn id args r env = env & fnEnv %~ insertSym id (FnEntry args r)

unitType :: S.Type
unitType = S.typeId "()"

nilType :: S.Type
nilType = S.typeId "nil"

intType :: S.Type
intType = S.typeId "int"

stringType :: S.Type
stringType = S.typeId "string"

preludeTypes :: M.Map T.Text S.Type
preludeTypes = M.fromList
  [ ("()", unitType)
  , ("nil", nilType)
  , ("int", intType)
  , ("string", stringType)
  ]

truthy :: S.Type -> Bool
truthy t
  | t == intType || t == stringType = True
  | otherwise = False

preludeVars :: M.Map T.Text S.Type
preludeVars = M.empty

preludeFns :: M.Map T.Text FnEntry
preludeFns = M.empty

prelude :: Env
prelude = Env
  { _typeEnv = SymbolTable [preludeTypes]
  , _varEnv = SymbolTable [preludeVars]
  , _fnEnv = SymbolTable [preludeFns]
  }