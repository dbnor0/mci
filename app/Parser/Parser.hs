module Parser.Parser where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Functor (($>), (<&>))
import Data.Text                  as T hiding (foldr, foldl')
import Data.Void
import Syntax.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer 
import Text.Megaparsec.Char (space1, char)

type Parser = Parsec Void Text

backtrack :: [Parser a] -> Parser a
backtrack = choice . (<$>) try

program :: Parser Expr
program = spaceOrComment *> expr <* eof

expr :: Parser Expr
expr = choice
  [ breakExpr
  , chainExpr
  , ifExpr
  , whileExpr
  , forExpr
  , opExpr
  ]

ifExpr :: Parser Expr
ifExpr
  =   IfE
  <$> (reserved "if" *> expr)
  <*> (reserved "then" *> expr)
  <*> optional (reserved "else" *> expr)

whileExpr :: Parser Expr
whileExpr = WhileE <$> (reserved "while" *> expr) <*> (reserved "do" *> expr)

forExpr :: Parser Expr
forExpr = ForE <$> id <*> init <*> end <*> body
  where id = reserved "for" *> identifier
        init = reserved ":=" *> expr
        end = reserved "to" *> expr
        body = reserved "do" *> expr

breakExpr :: Parser Expr
breakExpr = reserved "break" $> BreakE


assignmentExpr :: Parser Expr
assignmentExpr = AssignmentE <$> (lvalue <* reserved ":=") <*> expr

negateExpr :: Parser Expr
negateExpr = NegE <$> (reserved "-" *> expr)

opExpr :: Parser Expr
opExpr =
  factor
  `chainl1` multiplicativeExpr
  `chainl1` additiveExpr
  `chainl1` comparisonExpr
  `chainl1` logicalExpr

additiveExpr :: Parser (Expr -> Expr -> Expr)
additiveExpr = reserved "+" $> OpE PlusOp <|> reserved "-" $> OpE MinusOp

multiplicativeExpr :: Parser (Expr -> Expr -> Expr)
multiplicativeExpr = reserved "*" $> OpE TimesOp <|> reserved "/" $> OpE DivOp

comparisonExpr :: Parser (Expr -> Expr -> Expr)
comparisonExpr
  =   reserved ">=" $> OpE GteOp
  <|> reserved "<=" $> OpE LteOp
  <|> reserved "<>" $> OpE NotEqualsOp
  <|> reserved "<"  $> OpE LtOp
  <|> reserved ">"  $> OpE LtOp
  <|> reserved "="  $> OpE EqualsOp

logicalExpr :: Parser (Expr -> Expr -> Expr)
logicalExpr
  =   reserved "&" $> OpE AndOp
  <|> reserved "|" $> OpE OrOp

factor :: Parser Expr
factor = backtrack
  [ nilExpr
  , intExpr
  , stringExpr
  , functionCallExpr
  , negateExpr
  , assignmentExpr
  , arrayExpr
  , recordExpr
  , letExpr
  , lvalueExpr
  , parens expr
  ]

chainExpr :: Parser Expr
chainExpr = ChainE <$> parens exprs

nilExpr :: Parser Expr
nilExpr = NilE <$ reserved "nil"

intExpr :: Parser Expr
intExpr = IntE <$> intRaw

stringExpr :: Parser Expr
stringExpr = StringE <$> stringRaw

arrayExpr :: Parser Expr
arrayExpr = ArrayE <$> identifier <*> brackets expr <*> (reserved "of" *> expr)

functionCallExpr :: Parser Expr
functionCallExpr = FunctionCallE <$> identifier <*> parens (sepBy expr comma)

recordExpr :: Parser Expr
recordExpr = RecordE <$> identifier <*> braces (sepBy recordField comma)
  where recordField = RecordField <$> identifier <*> (reserved "=" *> expr)

lvalueExpr :: Parser Expr
lvalueExpr = LValueE <$> lvalue

lvalue :: Parser LValue
lvalue = chainl1' (chainl1' identifierLV identifier memberAccessLV) expr subscriptLV

memberAccessLV :: Parser (LValue -> Identifier -> LValue)
memberAccessLV = period $> MemberAccessLV

subscriptLV :: Parser (LValue -> Expr -> LValue)
subscriptLV = brackets $ pure SubscriptLV

identifierLV :: Parser LValue
identifierLV = IdentifierLV <$> identifier

letExpr :: Parser Expr
letExpr = LetE <$> (reserved "let" *> decls) <*> (reserved "in" *> exprs <* reserved "end")

exprs :: Parser [Expr]
exprs = sepBy expr semicolon

decls :: Parser [Decl]
decls = sepBy1 decl comma

decl :: Parser Decl
decl = backtrack
  [ aliasDecl
  , functionDecl
  , primitiveDecl
  , varDecl
  ]

aliasDecl :: Parser Decl
aliasDecl = TypeAliasDecl <$> (reserved "type" *> identifier <* reserved "=") <*> type'

functionDecl :: Parser Decl
functionDecl = FunctionDecl <$> functionName <*> argList <*> returnType <*> body
  where functionName = reserved "function" *> identifier
        argList = parens (sepBy typeField comma)
        returnType = optional (colon *> identifier)
        body = reserved "=" *> expr

primitiveDecl :: Parser Decl
primitiveDecl = PrimitiveDecl <$> functionName <*> argList <*> returnType
  where functionName = reserved "primitive" *> identifier
        argList = parens (sepBy typeField comma)
        returnType = optional (colon *> identifier)

varDecl :: Parser Decl
varDecl = VarDecl <$> varName <*> varType <*> value
  where varName = reserved "var" *> identifier
        varType = optional (colon *> identifier)
        value = reserved "=" *> expr

type' :: Parser Type
type' = backtrack
  [ typeIdentifier
  , recordType
  , arrayType
  ]

arrayType :: Parser Type
arrayType = ArrayType <$> (reserved "array" *> reserved "of" *> identifier ) <*> pure 0

recordType :: Parser Type
recordType = RecordType <$> braces (sepBy typeField comma) <*> pure 0

typeField :: Parser TypeField
typeField = TypeField <$> (identifier <* colon) <*> identifier

typeIdentifier :: Parser Type
typeIdentifier = TypeIdentifier <$> identifier

identifier :: Parser Identifier
identifier = Identifier <$> lexeme' (cons <$> satisfy isAlpha <*> takeWhileP Nothing isAlphaNum)

intRaw :: Parser Int
intRaw = lexeme' decimal

stringRaw :: Parser Text
stringRaw = pack <$> (char '\"' *> manyTill charLiteral (char '\"'))

lineComment :: Parser ()
lineComment = skipLineComment "//"

blockComment :: Parser ()
blockComment = skipBlockComment "/*" "*/"

whitespace :: Parser ()
whitespace = space1

spaceOrComment :: Parser ()
spaceOrComment = space whitespace lineComment blockComment

lexeme' :: Parser a -> Parser a
lexeme' = lexeme spaceOrComment

reserved :: Text -> Parser ()
reserved = void . lexeme' . chunk

comma :: Parser ()
comma = reserved ","

colon :: Parser ()
colon = reserved ":"

semicolon :: Parser ()
semicolon = reserved ";"

period :: Parser ()
period = reserved "."

parens :: Parser a -> Parser a
parens = between (reserved "(") (reserved ")")

brackets :: Parser a -> Parser a
brackets = between (reserved "[") (reserved "]")

braces :: Parser a -> Parser a
braces = between (reserved "{") (reserved "}")

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = do
      f <- op
      y <- p
      rest (f x y)
      <|> return x

chainl1' :: Parser a -> Parser b -> Parser (a -> b -> a) -> Parser a
chainl1' p1 p2 op = do
  x <- p1
  rest x
  where
    rest x = do
      f <- op
      y <- p2
      rest (f x y)
      <|> return x
