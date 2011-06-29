module XGen.StringGenerator
(
generate
) where


import XGen.Types
import XGen.Parser.Lexer
import XGen.Parser.HappyParser

import Prelude hiding (lex, showChar)
import Data.List
import Data.Either
import Control.Applicative


data GenerationError = GenerationError String deriving (Show)

generate :: String -> Either GenerationError [String]
generate s = let
                lexResult = lex s
                parseResult = parse (rights lexResult)
                genResult   = findAll (getRight parseResult)
             in 
                if noLefts lexResult
                then if notLeft parseResult
                     then (Right genResult)
                     else (Left (GenerationError (getParserError parseResult)))
                else (Left (GenerationError (getLexerErrors lexResult)))

getParserError :: Either ParserError XString -> String
getParserError (Left (ParserError cs)) = "Parse error near: " ++ parserErrorToString cs
getParserError (Right _)               = error "Could not get ParserError from 'right' result!"
parserErrorToString cs = "'" ++ (concat $ map showChar cs) ++ "'"


getLexerErrors :: [Either LexerError Character] -> String
getLexerErrors es  = "Could not lex the following character(s): " ++ getLexerErrors'(lefts es) ++ ""
getLexerErrors' es = concat $ intersperse ", " $ map lexerErrorToString es
lexerErrorToString (UnknownCharacter c) = "'" ++ [c] ++ "'"
lexerErrorToString _                    = "<unknown-lexer-error>"


noLefts = null . lefts
notLeft (Left _ ) = False
notLeft (Right _ ) = True

getRight (Right x) = x
getRight (Left _) = error "Could not get left!"


--------------------------------------------------------------------------------------------------------
-- | findAll
-- Given an XString it will return a list of all possible permutations

findAll (XString x) = allSteps' [] x
    where allSteps' acc []     = acc
          allSteps' acc (x:xs) = allSteps' (nextSteps acc x) xs

---------------------------------------------------------------------------------------------------------
-- | nextSteps

nextSteps :: [String] -> XStringPart -> [String]
nextSteps [] x   = nextStep "" x
nextSteps accs x = concatMap (flip nextStep x) accs

---------------------------------------------------------------------------------------------------------
-- | nextStep

nextStep :: String -> XStringPart -> [String]
nextStep s (TLiteral c)  = [s ++ pure c]
nextStep s (TVowel)      = map ((\a c -> a ++ pure c) s) tVowel
nextStep s (TConsonant)  = map ((\a c -> a ++ pure c) s) tConsonant
nextStep s (TLetter)     = map ((\a c -> a ++ pure c) s) tLetter
nextStep s (TNumber)     = map ((\a c -> a ++ pure c) s) tNumber
nextStep s (TList cs)    = map ((\a c -> a ++ pure c) s) cs
nextStep s (TRange s' e) = map ((\a c -> a ++ pure c) s) (enumFromTo s' e)
nextStep s (TAny)        = map ((\a c -> a ++ pure c) s) tRangeAny

---------------------------------------------------------------------------------------------------------
-- | Character lists (vowels, consonants, letters, numbers, any

tVowel :: [Char]
tVowel = "aeiou"
tConsonant :: [Char]
tConsonant = filter (flip notElem tVowel) ['a'..'z']
tLetter :: [Char]
tLetter = ['a'..'z']
tNumber :: [Char]
tNumber = ['0'..'9']
tRangeAny :: [Char]
tRangeAny = ['a'..'z'] ++ ['0'..'9']

