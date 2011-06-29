module XGen.StringGenerator
(
 parseExpression
) where


import XGen.Types
import XGen.Parser.Lexer
import XGen.Parser.HappyParser
import XGen.Parser.ParseErrorMonad

import Prelude hiding (lex)
import Data.List
import Control.Applicative



---------------------------------------------------------------------------------------------------------
-- | Given a valid expression all possible string permutations will be returned
--genFromExpression ex = case parseExpression ex of
--             Right p -> findAll p
--             Left e -> [e]

---------------------------------------------------------------------------------------------------------
-- | Given a string it will lex and parse it, returning an XString or an error message
parseExpression = parse' . eitherLex

parse' lexed = case lexed of
    Left err -> Left err
    Right cs -> (\cs' ->
            let parsed = checkUnknown . parse $ cs'
                checkUnknown ts = if containsUnknown ts
                                  then (Failed "Error! Expression invalid (could not be parsed)")
                                  else ts
                containsUnknown (Ok (XString ts)) = not . null $ filter (let f TUnknown = True; f _ = False in f) ts
                containsUnknown (Failed p) = False
            in
                case parsed of
                    Ok p -> Right p
                    Failed p -> Left p
        ) cs

fparsed' cs' = case parsed' cs' of
                    Ok p -> Right p
                    Failed p -> Left p

parsed' cs' = checkUnknown' . parse $ cs'
checkUnknown' ts = if containsUnknown' ts
                   then (Failed "Error! Expression invalid (could not be parsed)")
                   else ts
containsUnknown' (Ok (XString ts)) = not . null $ filter (let f TUnknown = True; f _ = False in f) ts
containsUnknown' (Failed p) = False

--parse' s = case parse s of
--              (Ok p) -> (Just p)
--              (Failed p) -> Nothing



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

