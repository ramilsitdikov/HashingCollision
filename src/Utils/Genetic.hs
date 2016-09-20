module Genetic(geneticGen, stopf) where

	import System.Random
	import GeneticAlgorithm
	import BSet
	import Random
	import qualified Data.Vector as V

	stopf :: Float -> BSet -> Int -> Bool
	stopf delta set gnum = gnum > 20 || delta > err set

	randomIndividual :: RandomGen g => BParams -> Int -> g -> (BSet, g)
	randomIndividual param d gen = 
		let	(lst, gen') = randomInts (ring param) d gen
		in (B (V.fromList lst, param), gen')

	geneticGen :: RandomGen g => BParams -> g -> (BSet -> Int -> Bool) -> (Int, Int) -> ([BSet], g)
	geneticGen param gen stopf' (lowD, hightD) =
		let	(xs, gen') 	= runGA gen lowD 0.25 (randomIndividual param lowD) stopf'
			(xss, gen'')= geneticGen param gen' stopf' ((lowD + 1), hightD)
		in if lowD > hightD then 
				([], gen)
			else
				(xs:xss, gen'')