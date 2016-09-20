module Annealer where

	import Control.Concurrent.Annealer
	import System.Random
	import Helpers
	import Control.Monad
	import Control.Monad.Random

	import qualified Data.Array as A
	import qualified Data.IntSet as IS
	import qualified Data.Vector as V

	data Params = Params { 	threads :: Int
							, time :: Int
							, len :: Int
							, m :: Int
							, eps :: Double
							, t :: Int } deriving (Show, Eq)

	type State  = V.Vector Int
	type Energy = Float

	energy :: A.Array Int Float -> Int -> State -> Energy
	energy cosList m state = getMaxForAllX cosList m 1.0 state

	swap :: Int -> Int -> State -> State
	swap j x state = state V.// [(j,x)]

	change t m state = do
		(j, x) <- evalRandIO $ do
			j <- getRandomR (0, t-1)
			x <- getRandomR (0, m-1)
			return (j,x)
		return $ swap j x state

	randomState :: (RandomGen g) => Int -> Int -> Rand g State
	randomState t m = liftM (V.fromList . take t) $ sequence $ replicate t $ getRandomR (0, m-1)

	work p = do
		let cosList = calcCosList (m p)
		initial <- evalRandIO $ sequence $ replicate (len p) $ randomState (t p) (m p)
		annealer <- initAnnealer initial (energy cosList (m p)) (length initial) (change (t p) (m p))
		best <- annealForTime (threads p) (time p) annealer
		print (V.toList best, energy cosList (m p) best, p)

	getT m eps = round $ 2/eps*log (fromIntegral $ 2*m) + 1