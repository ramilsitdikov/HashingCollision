module BSet where

	import GeneticAlgorithm
	import Control.DeepSeq
	import Helpers
	import System.Random
	import qualified Data.List as  L
	import qualified Data.Vector as V
	import Data.Array

	data BSet =  Null | B (V.Vector Int, BParams) deriving (Eq)

	instance Show BSet where
		show (B (xs, param)) 	= "Ring: " ++ (show $ ring param) ++ ", Deep: " 
			++ (show $ V.length xs) ++ ", Set: " ++ (show xs)
		show Null 				= "set is empty"

	data BParams = BParams {
		ring		:: Int,
		cosList 	:: Array Int Float
	} deriving (Show, Eq)

	instance NFData BSet where
		rnf (B (xs, _)) = rnf xs `seq` ()
		rnf (Null) = ()

	instance Individual BSet where
		crossover g Null _ = ([Null], g)
		crossover g _ Null = ([Null], g)
		crossover g (B (xs1, param)) (B (xs2, _)) =
			let	(idx, g') = randomR (1, V.length xs1 - 1) g
				xs   = xs1 V.// L.zip [0..idx-1] (L.take idx $ V.toList xs2)
			in ([B (xs, param)], g')

		mutation g Null = (Null, g)
		mutation g (B (xs, param)) =
			let	(idx, g') 	= randomR (0, V.length xs - 1) g
				(dx, g'') 	= randomR (-3, 3) g'
				t 			= xs V.! idx
				xs' 		= xs V.// [(idx, (t + dx)`mod`ring param)]
			in (B (xs', param), g'')

		fitness Null 	= 1.0
		fitness set 	= 
			let max_err = 1.0 in
			max_err - (min max_err (err set))

	err :: BSet -> Float
	err Null = 1.0
	err (B (xs, param)) = getMaxForAllX (cosList param) (ring param) 1.0 xs