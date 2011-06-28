module Main where

import XGen.StringGenerator

main = mapM_ putStrLn (genFromExpression "hel(c)(v)")
