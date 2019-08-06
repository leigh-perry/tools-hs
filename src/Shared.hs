module Shared
  ( distinct
  ) where

distinct :: Eq a => [a] -> [a]
distinct [] = []
distinct (x:xs) = x : filter (/= x) (distinct xs)
