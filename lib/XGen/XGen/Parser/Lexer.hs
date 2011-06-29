module XGen.Parser.Lexer 
( Character(..)
, showChar
, LexerError(..)
, LexerResult
, lex
) where

import XGen.Types
  
import Prelude   hiding (lex, showChar)
import Data.Char hiding (isAlphaNum)
import Data.List (find, intersperse)

data Character = CChar Char
               | CLBracket
               | CRBracket
               | CQuestion
               | CComma
               | CHyphen
               deriving (Show)

showChar :: Character -> String
showChar (CChar c) = [c]
showChar CLBracket = "("
showChar CRBracket = ")"
showChar CQuestion = "?"
showChar CComma    = ","
showChar CHyphen   = "-"

data LexerError = UnknownCharacter Char deriving (Show)

-- LexerReturn type, will return a list of all successfully
-- lexed characters and any possible errors
type LexerResult = [Either LexerError Character]


c2c :: Char -> Either LexerError Character
c2c '('  = Right CLBracket
c2c ')'  = Right CRBracket
c2c ','  = Right CComma
c2c '?'  = Right CQuestion
c2c '-'  = Right CHyphen
c2c c | c `elem` ['0'..'9'] = Right $ CChar c
      | c `elem` ['A'..'Z'] = Right $ CChar (toLower c)
      | c `elem` ['a'..'z'] = Right $ CChar c
      | otherwise           = Left  $ UnknownCharacter c

lex :: String -> [Either LexerError Character]
lex = map c2c