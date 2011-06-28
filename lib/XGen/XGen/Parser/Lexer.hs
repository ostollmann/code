module XGen.Parser.Lexer 
( lex
, Character(..)
) where

import XGen.Types

import Prelude   hiding (lex)
import Data.Char hiding (isAlphaNum)

data Character = CChar Char
               | CLBracket
               | CRBracket
               | CQuestion
               | CComma
               | CHyphen
               | CUnknown
               deriving (Show)


c2c :: Char -> Character
c2c '('  = CLBracket
c2c ')'  = CRBracket
c2c ','  = CComma
c2c '?'  = CQuestion
c2c '-'  = CHyphen
c2c c | c `elem` ['0'..'9'] = CChar c
      | c `elem` ['A'..'Z'] = CChar (toLower c)
      | c `elem` ['a'..'z'] = CChar c

lex :: String -> [Character]
lex = map c2c

printC2c s = mapM_ putStrLn $ (map (show . c2c) s)
