{-
This module holds the definitions of the descrete distributions used for this language.
-}

module Dist where

import Data.List
import System.Random

type Probability = Double

{- | Distribution class, Probabilities must sum to 1 and all be positive reals.

__Examples:__
@
Distribution { dist = [(-1, 0.4), (1, 0.6)] }
@
-}
data Distribution a = Distribution { dist :: [(a, Probability)] } deriving (Eq, Ord)
instance Functor Distribution where
    fmap func (Distribution dist) =  Distribution { dist = map (\(a, p) -> (func a, p)) dist }

instance Applicative Distribution where
    pure a = Distribution { dist = [(a, 1.0)]}

    (Distribution funcDist) <*> (Distribution dist) = Distribution {
        dist = [ (f a, pf * p)  | (f, pf) <- funcDist, (a, p) <- dist ]
    }

{-
Applies the data type to the bind function and multiplies the probability
of that data type through the new distribution. Concats the resulting distributions together.
-}
instance Monad Distribution where
    (Distribution distList) >>= func =
        Distribution {
            dist = concat (do
                (a, p) <- distList
                let (Distribution newDist) = func a
                return $ map (\(a, np) -> (a, p * np)) newDist
            )
        }

{- | A function to clean the distribution so that all equal parts are assigned to the same element.
-}
clean :: (Ord a, Eq a) => Distribution a -> Distribution a
clean (Distribution impDist) = Distribution {
    dist = 
        let tempDist = groupBy (\(a1, _) (a2, _) -> a1 == a2) $ sort impDist
            foldProbs xx@((a, _):_) = (a, sum $ map (\(a, p) -> p) xx)
        in map foldProbs tempDist
    }

{- | Cleans a distribution by filtering out via some predicate and then combining similar values.
-}
cleanPred :: (Ord a, Eq a) => Distribution (Maybe a) -> Distribution a
cleanPred (Distribution impDist) = clean (Distribution { dist = (impDist >>= isJust) }) where
    isJust (Just a, p) = [(a, p)]
    isJust _ = []

instance (Show a) => Show (Distribution a) where
    show (Distribution dist) =
        let first = take 10 dist
            strList = case (drop 10 dist) of
                [] -> getListStr first
                xx -> ("*: " ++ (show $ getOtherP first)):(getListStr first)
        in foldr (\s1 s2 -> "| " ++ s1 ++ "\n" ++ s2) "----" strList where 
            getListStr distList = map (\(a, p) -> (show a) ++ ": " ++ (show p)) distList
            getOtherP first = 1.0 - (sum $ map (\(_, p) -> p) first)

{- | Combines an array of Distributions into a single distribution of all the events, with the probabilities accounted for.
This is eqivalent of executing N trials, each with a different distribution.
-} 
join :: [Distribution a] -> Distribution [a]
join [] = error "Can't join 0 distributions"
join [d] = do
    m <- d
    return [m]
join (d:ds) = do
    measure <- d 
    otherM <- join ds
    return $ measure:otherM

{- | Filters a distribution given a predicate. Simplified a distribution.
-}
given :: Distribution a -> (a -> Bool) -> Distribution a
given (Distribution d) pred = 
    let d2 = filter (pred . fst) d
        d3 = normalizeDistribution d2
    in  Distribution { dist = d3 }

{- | Run a distribution until a predicate is true. Can be used to get the Geometric distribution for another.
-}
until :: Distribution a -> (a -> Bool) -> Distribution [a]
until distribution pred = do
    c <- distribution
    if pred c then return $ [c]
    else do
        l <- Dist.until distribution pred
        return $ c:l

{- | Combines two distributions together and simplifies the result.
-}
cat :: (Ord a) => [Distribution a] -> Distribution a
cat dists = 
    let measures = map dist dists
        newMeasure = normalizeDistribution $ concat measures
    in  clean $ Distribution { dist = newMeasure }

{- | Get the expectation of a distribution.
-}
expectation :: Distribution Double -> Double
expectation (Distribution dist) = sum $ map (\(a, p) -> ((a * p) :: Double)) dist

{- | Ensures the distribution sums to 1.

Occasionally a distribution may become invalid and not sum to 1. This is to ensure that does not occur.
Does not work with infinite distributions. 
-}
normalizeDistribution :: Fractional b => [(a, b)] -> [(a, b)]
normalizeDistribution dist = 
        let total = sum (map snd dist)
        in map (\(a, p) -> (a, p / total)) dist

{- | Run n trials, and count how many times a predicate is true in those trials.
Used to implement a Binomial distribution from another one.
-}
count :: Int -> Distribution a -> (a -> Bool) -> Distribution Int
count n distribution pred = clean countDist where
    countDist = do
        events <- join $ take n $ repeat distribution
        let count = length $ filter pred events
        return $ count

{- Sample N elements from a distribution.
-}
sample :: Int -> Int -> Distribution a -> [a]
sample seed n (Distribution dist) =
    let sampleVals = take n $ randomList seed
        cnfDist = getCnf dist 0
    in  map (\sp -> lookup sp cnfDist) sampleVals
    where
        getCnf [] _ = []
        getCnf ((a, p):xs) prevTotal = let newP = p + prevTotal in (a, newP):(getCnf xs newP)

        lookup sp cnfDist = fst $ head $ filter (\(a, p) -> sp < p) cnfDist

        randomList :: Int -> [Double]
        randomList seed = randoms (mkStdGen seed) 
