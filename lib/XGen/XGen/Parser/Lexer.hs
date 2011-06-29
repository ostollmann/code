module XGen.Parser.Lexer 
( lex
, eitherLex
, Character(..)
) where

import XGen.Types

import Prelude   hiding (lex)
import Data.Char hiding (isAlphaNum)
import Data.List (find, intersperse)

data Character = CChar Char
               | CLBracket
               | CRBracket
               | CQuestion
               | CComma
               | CHyphen
               | CUnknown Char
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
      | otherwise           = CUnknown c

eitherLex s = case unknowns lexed of
                [] -> Right lexed
                us  -> Left $ "Error! Invalid character(s): " ++ showUnknowns us
    where lexed = lex s
          unknowns cs = filter f cs
          f (CUnknown _) = True
          f _            = False
          showUnknowns us = concat . addCommas . addQuotes $ map (\(CUnknown c) -> c) us
          addQuotes = map (\c -> "'" ++ [c] ++ "'")
          addCommas cs = intersperse ", " cs

lex :: String -> [Character]
lex = map c2c

printC2c s = mapM_ putStrLn $ (map (show . c2c) s)
