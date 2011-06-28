module XGen.StringGenerator
( genFromExpression
, parseExpression
) where


import XGen.Types
import XGen.Parser.Lexer
import XGen.Parser.HappyParser

import Prelude hiding (lex)
import Data.List
import Control.Applicative



---------------------------------------------------------------------------------------------------------
-- | Given a valid expression all possible string permutations will be returned
genFromExpression = findAll . parseExpression


---------------------------------------------------------------------------------------------------------
-- | Given a string it will lex and parse it, returning an XString
parseExpression = parse . lex


---------------------------------------------------------------------------------------------------------
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

