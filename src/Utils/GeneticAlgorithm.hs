module GeneticAlgorithm(
	Individual(..),
	runGA,
	zeroGeneration,
	nextGeneration
	) where

	import System.Random
	import qualified Data.List as L
	import Control.Parallel.Strategies

	class NFData a => Individual a where
		crossover :: RandomGen g => g -> a -> a -> ([a], g)

		mutation :: RandomGen g => g -> a -> (a, g)

		fitness :: a -> Float

	runGA 	:: (RandomGen g, Individual a)
			=> g -- ^ Random number generator
			-> Int -- ^ Population size
			-> Float -- ^ Mutation probability [0, 1]
			-> (g -> (a, g)) -- ^ Random chromosome generator (hint: use currying or closures)
			-> (a -> Int -> Bool) -- ^ Stopping criteria, 1st arg - best chromosome, 2nd arg - generation number
			-> (a, g) -- ^ Best chromosome
	runGA gen ps mp rnd stopf =
		let (pop, gen') = zeroGeneration gen rnd ps in
		runGA' gen' pop ps mp stopf 0

	runGA' gen pop ps mp stopf gnum =
		let best = head pop in
		if stopf best gnum
			then (best, gen)
			else
				let (pop', gen') = nextGeneration gen pop ps mp in
				runGA' gen' pop' ps mp stopf (gnum + 1)

	zeroGeneration 	:: (RandomGen g)
					=> g -- ^ Random number generator
					-> (g -> (a, g)) -- ^ Random chromosome generator (hint: use closures)
					-> Int -- ^ Population size
					-> ([a],g) -- ^ Zero generation and new RNG
	zeroGeneration initGen rnd ps =
		L.foldl'
			(\(xs,gen) _ -> let (c, gen') = rnd gen in ((c:xs),gen'))
			([], initGen) [1..ps]

	nextGeneration 	:: (RandomGen g, Individual a)
					=> g -- ^ Random number generator
					-> [a] -- ^ Current generation
					-> Int -- ^ Population size
					-> Float -- ^ Mutation probability
					-> ([a], g) -- ^ Next generation ordered by fitness (best - first) and new RNG
	nextGeneration gen pop ps mp =
		let	(gen':gens) = L.unfoldr (Just . split) gen
			chunks = L.zip gens $ init $ L.tails pop
			results = map (\(g, (x:ys)) -> [ (t, fitness t) | t <- nextGeneration' [ (x, y) | y <- ys ] g mp [] ]) chunks
				`using` parList rdeepseq
			lst = take ps $ L.sortBy (\(_, fx) (_, fy) -> fy `compare` fx) $ concat results
		in ( map fst lst, gen' )
			
	nextGeneration' [] _ _ acc = acc
	nextGeneration' ((p1,p2):ps) g0 mp acc =
		let	(children0, g1) = crossover g0 p1 p2
			(children1, g2) = L.foldl'
				(\(xs, g) x -> let (x', g') = mutate g x mp in (x':xs, g'))
				([],g1) children0
		in
			nextGeneration' ps g2 mp (children1 ++ acc)

	mutate :: (RandomGen g, Individual a) => g -> a -> Float -> (a, g)
	mutate gen x mp =
		let (r, gen') = randomR (0.0, 1.0) gen in
		if r <= mp then mutation gen' x
			else (x, gen')