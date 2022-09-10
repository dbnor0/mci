module Introduction.Parser where
import Text.Megaparsec
import Data.Void
import Data.Text hiding (foldl')
import Introduction.Syntax (Exp (..), Ident, Stmt (AssignStmt, PrintStmt, CompoundStmt, NoOpStmt), BinOp (..))
import Data.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char (space1)
import Control.Monad
import Data.Functor (($>))
import Data.Foldable

type Parser = Parsec Void Text

backtrack :: [Parser a] -> Parser a
backtrack = choice . (<$>) try

stmt :: Parser Stmt
stmt = do
  stmts <- sepBy1 (backtrack [assignStmt, printStmt]) (reserved ";")
  return $ foldl' CompoundStmt NoOpStmt stmts

assignStmt :: Parser Stmt
assignStmt = AssignStmt <$> identifier <*> (reserved ":=" *> expr)

printStmt :: Parser Stmt
printStmt = PrintStmt <$> (reserved "print" *> parens (sepBy1 expr (reserved ",")))

expr :: Parser Exp
expr = backtrack [identExpr, numExpr, opExpr, seqExpr]

identExpr :: Parser Exp
identExpr = IdentExp <$> identifier

numExpr :: Parser Exp
numExpr = NumExp <$> intRaw

opExpr :: Parser Exp
opExpr
  = factor
  `chainl1` additiveExpr
  `chainl1` multiplicativeExpr

factor :: Parser Exp
factor = try $ parens opExpr <|> numExpr

additiveExpr :: Parser (Exp -> Exp -> Exp)
additiveExpr = reserved "+" $> OpExp Plus

multiplicativeExpr :: Parser (Exp -> Exp -> Exp)
multiplicativeExpr = reserved "*" $> OpExp Times

seqExpr :: Parser Exp
seqExpr = SeqExp <$> parens ((,) <$> stmt <*> (reserved "," *> expr))

identifier :: Parser Ident
identifier = lexeme' $ cons <$> satisfy isAlpha <*> takeWhileP Nothing isAlphaNum

intRaw :: Parser Int
intRaw = lexeme' decimal

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

parens :: Parser a -> Parser a
parens = between (reserved "(") (reserved ")")

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
