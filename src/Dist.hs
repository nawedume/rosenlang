{-
This module holds the definitions of the descrete distributions used for this language.
-}

module Dist where

import Data.List

type Probability = Double
data Distribution a = Distribution { dist :: [(a, Probability)] }

instance Functor Distribution where
    fmap func (Distribution dist) =  Distribution { dist = map (\(a, p) -> (func a, p)) dist }

instance Applicative Distribution where
    pure a = Distribution { dist = [(a, 1.0)]}

    (Distribution funcDist) <*> (Distribution dist) = Distribution {
        dist = [ (f a, pf * p)  | (f, pf) <- funcDist, (a, p) <- dist ]
    }

instance Monad Distribution where
    (Distribution distList) >>= func =
        Distribution {
            dist = concat (do
                (a, p) <- distList
                let (Distribution newDist) = func a
                return $ map (\(a, np) -> (a, p * np)) newDist
            )
        }

{-
A function to clean the distribution so that all equal parts are assigned to the same element.
-}
clean :: (Ord a, Eq a) => Distribution a -> Distribution a
clean (Distribution impDist) = Distribution {
    dist = 
        let tempDist = groupBy (\(a1, _) (a2, _) -> a1 == a2) $ sort impDist
            foldProbs xx@((a, _):_) = (a, sum $ map (\(a, p) -> p) xx)
        in map foldProbs tempDist
    }

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
    


