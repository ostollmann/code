module XGen.Types 
( XString (..)
, XStringPart (..)
) where

---------------------------------------------------------------------------------------------------------
-- | XString

newtype XString = XString [XStringPart] deriving (Show)

data XStringPart = TLiteral Char
                 | TVowel
                 | TConsonant
                 | TLetter
                 | TNumber
                 | TList [Char]
                 | TRange Char Char
                 | TAny
                 deriving (Show)

