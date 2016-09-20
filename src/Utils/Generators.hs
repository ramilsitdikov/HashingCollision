module Generators where

	import BSet
	import Bruteforse
	import Random
	import Genetic
	import System.Random

	type ChooseMethod 	= (BParams -> (Int, Int) -> [BSet])

	getBSet :: BParams -> (Int, Int) -> ChooseMethod -> [BSet]
	getBSet param d choose = choose param d

	chooseBruteforse	:: BParams -> (Int, Int) -> [BSet]
	chooseBruteforse param (lowD, hightD)
		| lowD <= hightD = (bruteforse param [1..(ring param) `div` 2] lowD) ++ chooseBruteforse param (lowD+1, hightD)
		| otherwise = []
	
	chooseRandom 					:: RandomGen g => g -> BParams -> (Int, Int) -> [BSet]
	chooseRandom gen param (lowD, hightD) 
		| lowD <= hightD = lst ++ chooseRandom genn param ((lowD+1), hightD)
		| otherwise = []
			where (lst, genn) = randomBSets param 10000 (ring param) lowD gen

	chooseGenetic :: RandomGen g => g -> (BSet -> Int -> Bool) -> BParams -> (Int, Int) -> [BSet]
	chooseGenetic gen stopf' param d = fst (geneticGen param gen stopf' d)
