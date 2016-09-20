module Minimization where

	import Helpers
	import qualified Data.List as L
	import BSet
	import Generators
	import Debug.Trace

	type Result			= (Float, BSet)

	minimize :: Int -> Float -> ChooseMethod -> Maybe Result
	minimize q delta choose =
		let	lowerD			= getLowerBound q delta
			param 			= BParams q (calcCosList q)
			getMax			= getMaxForAllX (cosList param) q
			bSets			= getBSet param (100, q) choose
			result'			= L.scanl 
				(\(prevD, prevB) (B (xs, param)) ->
					let	currD = getMax prevD xs in
					if currD >= prevD then (prevD, prevB) else traceShow (show (currD, B (xs, param))) (currD, B (xs, param)))
				(1.0, Null) 
				bSets
		in L.find (\(delta', _) -> delta >= delta' ) result'

	-- binarySearch ::(Int, Int) ->
	-- 			(Float -> [Int] -> Float) ->
	-- 			((Float, BSet) -> Bool) ->
	-- 			(Int -> [BSet]) ->
	-- 			Result ->
	-- 			Result
	-- binarySearch (lowerD, hightD) getMax findMeth choose prevRes =
	-- 	let	middleD					= (lowerD + hightD) `div` 2
	-- 		bSets					= choose middleD
	-- 		result'					= L.scanl 
	-- 			(\(prevD, prevB) (B (xs, param)) ->
	-- 				let	currD = getMax prevD xs in
	-- 				if currD > prevD then (prevD, prevB) else (currD, B (xs, param)))
	-- 			(1.0, Null) 
	-- 			bSets 1
	-- 		result = L.find findMeth result'
	-- 	in if Nothing == result then
	-- 		if hightD -1 <= lowerD then
	-- 			prevRes else 
	-- 				binarySearch (middleD, hightD) cosList getMax findMeth choose prevRes else
	-- 		binarySearch (lowerD, middleD) cosList getMax findMeth choose (M.fromJust result)