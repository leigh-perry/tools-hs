module Shared where

import Data.Char (toLower)
import qualified Text.Casing as Casing (pascal)

distinct :: Eq a => [a] -> [a]
distinct [] = []
distinct (x:xs) = x : filter (/= x) (distinct xs)

classCase :: String -> String
classCase s = Casing.pascal $ toLower <$> s

sym :: [String] -> String
sym = foldr ((<>) . classCase) []
