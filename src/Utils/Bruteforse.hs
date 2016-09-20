module Bruteforse(bruteforse) where

	import BSet
	import Data.Vector
	import qualified Data.List as L

	bruteforse :: BParams -> [Int] -> Int -> [BSet]
	bruteforse param xs d = [ B (fromList xs', param) | xs' <- bruteforse' xs d ]

	bruteforse' :: [Int] -> Int -> [[Int]]
	bruteforse' _ 0  = [[]]
	bruteforse' xs d = [ xs !! i : x | i <- [0..(L.length xs)-1] 
	                                  , x <- bruteforse' (L.drop i xs) (d-1) ]
