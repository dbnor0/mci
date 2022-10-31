module Syntax.Utils where

import qualified Data.Text as T
import Syntax.Syntax

id :: T.Text -> Identifier
id = Identifier

tfieldName :: TypeField -> Identifier
tfieldName (TypeField id _) = id

tfieldType :: TypeField -> Identifier
tfieldType (TypeField _ id) = id

rfieldName :: RecordField -> Identifier
rfieldName (RecordField id _) = id

rfieldVal :: RecordField -> Expr
rfieldVal (RecordField _ v) = v

typeId :: T.Text -> Type
typeId = TypeIdentifier . Syntax.Utils.id

arithOp :: Operator -> Bool
arithOp = flip elem [PlusOp, MinusOp, TimesOp, DivOp]

eqOp :: Operator -> Bool
eqOp = flip elem [EqualsOp, NotEqualsOp]

relationalOp :: Operator -> Bool
relationalOp = flip elem [LtOp, GtOp, LteOp, GteOp]

logicalOp :: Operator -> Bool
logicalOp = flip elem [AndOp, OrOp]
