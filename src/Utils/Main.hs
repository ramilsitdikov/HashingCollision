module Main where

	import System.Random
	import Helpers
	import Annealer
	import BSet
	import Minimization
	import Generators
	import Genetic
	import Construct
	import Bruteforse

	main :: IO ()
	main = do
		gen <- newStdGen
		print (minimize (2^10) 0.01 chooseBruteforse)
		-- work $ Params 4 1000000 10000 (2^5) 0.06 17