module Helpers where

	import Data.List
	import Data.Array
	import System.Random
	import qualified Data.Vector as V

	partialSum	:: Array Int Float -> Int -> [Int] -> Int -> Float
	partialSum	cosList q xs x = 
		let	q'			= q `div` 2
			getIndex	= \ind -> let ind' = ind `mod` q in if ind' > q' then abs (ind' - q) else ind'
			getCosVal 	= \b -> cosList! (b*x `mod` q)
			deep 		= length xs
			-- sumList		= map getCosVal xs `using` parList rdeepseq
		in  ( foldl' (\f s -> (+) f $ getCosVal s) 0 xs ) / convertInt deep
		-- in (  sumList `seq` foldl' (+) 0 sumList ) / convertInt deep

	partialSum'	:: Array Int Float -> Int -> V.Vector Int -> Int -> Float
	partialSum'	cosList q xs x = 
		let	q'			= q `div` 2
			getIndex	= \ind -> let ind' = ind `mod` q in if ind' > q' then abs (ind' - q) else ind'
			getCosVal 	= \b -> cosList! getIndex (b*x)
			deep 		= V.length xs
		in  ( V.foldl' (\f s -> (+) f $ getCosVal s) 0 xs ) / convertInt deep

	getMaxForAllX :: Array Int Float -> Int -> Float-> V.Vector Int -> Float
	getMaxForAllX cosList q prevDelta xs = 
		let	partSum 	= partialSum' cosList q
			ring		= [1..q `div` 2]
		in foldl' (\f s -> if f < prevDelta then max f (abs $ partSum xs s) else 1.0) 0 ring

	findBetter :: Array Int Float -> Int -> Float -> [V.Vector Int] -> (V.Vector Int, Float)
	findBetter cosList q delta xs = foldl'
		(\(prevLst, prevD) lst -> 
			let currD = getMaxForAllX cosList q prevD lst
			in if currD > prevD then (prevLst, prevD) else (lst, currD))
		(V.fromList [], delta)
		xs

	getLowerBound :: Int -> Float -> Int
	getLowerBound q _ = round . log . convertInt $ q

	getHightBound :: Int -> Float -> Int
	getHightBound q delta = min q $ round ((2.0 * (log . convertInt $ (2 * q))) / delta / delta)

	convertInt :: Int -> Float
	convertInt x = fromInteger . toInteger $ x

	comb :: Int -> Int -> Int
	comb n m 
		| (n < 0) || (m < 0) || (n < m) = 0
		| n < 2 * m = comb' n (n-m)
		| otherwise = comb' n m

	comb' :: Int -> Int -> Int
	comb' _ 0 = 1
	comb' n m = (comb' (n-1) (m-1)) * n `div` m

	randomInts 		   				:: RandomGen g => Int -> Int -> g -> ([Int], g)
	randomInts _ 0 gen 				= ([], gen)
	randomInts q d gen 			= 
		let	(x, genn) = randomR (0, q) $ gen
			(xs, gennn) = randomInts q (d-1) genn
	    in (x:xs, gennn)

	calcCosList :: Int -> Array Int Float
	calcCosList q = array (0, q) [ (q', cos (2.0 * pi * (convertInt q') / (convertInt q) )) | q' <- [0..q] ]

	gcdExt :: Int -> Int -> (Int, Int, Int)
	gcdExt a 0 = (1, 0, a)
	gcdExt a b = let (q, r) = a `quotRem` b
	                 (s, t, g) = gcdExt b r
	             in (t, s - q * t, g)
	 
	modInv :: Int -> Int -> Maybe Int
	modInv a m = let (i, _, g) = gcdExt a m
	             in if g == 1 then Just (mkPos i) else Nothing
	  where mkPos x = if x < 0 then x + m else x
 