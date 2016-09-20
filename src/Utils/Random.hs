module Random(randomBSets, randomInts) where

	import System.Random
	import BSet
	import Helpers
	import qualified Data.Vector as V

	randomBSets 					:: RandomGen g => BParams -> Int -> Int -> Int -> g -> ([BSet], g)
	randomBSets _ 0 _ _ gen 	 		= ([], gen)
	randomBSets param size q d gen 		= 
		let	(x, genn) = randomInts (q `div` 2) d gen
			(xs, gennn) = randomBSets param (size - 1) q d genn
	    in ((B (V.fromList x, param)):xs, gennn)