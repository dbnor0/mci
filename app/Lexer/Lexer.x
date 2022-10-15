{
-- At the top of the file, we define the module and its imports, similarly to Haskell.
module Lexer.Lexer
  ( -- * Invoking Alex
    Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan

  , Range (..)
  , RangedToken (..)
  , Token (..)
  ) where

import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

}
-- In the middle, we insert our definitions for the lexer, which will generate the lexemes for our grammar.
%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@id = ($alpha | \_) ($alpha | $digit | \_)*

tokens :-

-- whitespace
<0>          $white+    ;
<0>          "/*"       { nestComment `andBegin` comment }
<0>          "*/"       { \_ _ -> alexError "Error: unexpected closing comment" }
<comment>    "/*"       { nestComment }
<comment>    "*/"       { unnestComment }
<comment>    .          ;
<comment>    \n         ;

-- identifiers
<0>          @id        { tokId }

-- literals
<0>          $digit+    { tokInteger }
<0>          \"[^\"]*\" { tokString }


-- reserved words 
<0>          while      { tok While }
<0>          for        { tok For }
<0>          to         { tok To }
<0>          break      { tok Break }
<0>          let        { tok Let }
<0>          in         { tok In }
<0>          end        { tok End }
<0>          function   { tok Function }
<0>          var        { tok Var }
<0>          type       { tok Type }
<0>          array      { tok Array }
<0>          if         { tok If }
<0>          then       { tok Then }
<0>          else       { tok Else }
<0>          do         { tok Do }
<0>          of         { tok Of }
<0>          nil        { tok Nil }

-- separators
<0>          ","        { tok Comma }
<0>          ":"        { tok Colon }
<0>          ";"        { tok Semicolon }
<0>          "."        { tok Period }

-- parantheses
<0>          "("        { tok LParen }
<0>          ")"        { tok RParen}
<0>          "["        { tok LBracket }
<0>          "]"        { tok RBracket }
<0>          "{"        { tok LBrace }
<0>          "}"        { tok RBrace }

-- operators
<0>          "+"        { tok Plus }
<0>          "-"        { tok Minus }
<0>          "*"        { tok Times }
<0>          "/"        { tok Div }
<0>          "="        { tok Equals }
<0>          "<>"       { tok NotEquals }
<0>          "<="       { tok Lte }
<0>          ">="       { tok Gte }
<0>          "<"        { tok Lt }
<0>          ">"        { tok Gt }
<0>          "&"        { tok And }
<0>          "|"        { tok Or }
<0>          ":="       { tok Assign }

{

-- At the bottom, we may insert more Haskell definitions, such as data structures, auxiliary functions, etc.
data AlexUserState = AlexUserState
  { nestLevel :: Int
  }

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 
  { nestLevel = 0
  }

alexEOF :: Alex RangedToken
alexEOF = do
  startCode <- alexGetStartCode
  when (startCode == comment) $
    alexError "Error: unclosed comment"
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

data Token
  = Identifier ByteString
  | IntLit Int
  | StringLit ByteString
  | While
  | For
  | To
  | Break
  | Let
  | In
  | End
  | Function
  | Var
  | Type
  | Array
  | If 
  | Then
  | Else
  | Do
  | Of
  | Nil
  | Comma
  | Colon
  | Semicolon
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Period
  | Plus
  | Minus
  | Times
  | Div
  | Equals
  | NotEquals
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or
  | Assign
  | EOF
  deriving (Eq, Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
  pure RangedToken
    { rtToken = ctor
    , rtRange = mkRange inp len
    }

tokId :: AlexAction RangedToken
tokId inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Identifier $ BS.take len str
    , rtRange = mkRange inp len
    }

tokInteger :: AlexAction RangedToken
tokInteger inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = IntLit $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }

tokString :: AlexAction RangedToken
tokString inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = StringLit $ BS.take len str
    , rtRange = mkRange inp len
    }

nestComment :: AlexAction RangedToken
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len

unnestComment :: AlexAction RangedToken
unnestComment input len = do
  state <- get
  let level = nestLevel state - 1
  put state{nestLevel = level}
  when (level == 0) $
    alexSetStartCode 0
  skip input len

}
