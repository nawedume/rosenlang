module Example where

import Dist

-- examples

data Light = Red | Yellow | Green deriving (Show, Eq, Ord)
data Code = X | Y deriving (Show, Eq, Ord)

lightToCode light = if light == Red then X else Y


funcDist :: Distribution (Integer -> Integer)
funcDist = Distribution {
    dist = [((+ 1), 0.5), ((+ 2), 0.5)]
}

intDist = Distribution {
    dist = [(10, 0.2), (9, 0.8)]
}

data CoinR = Heads | Tails deriving (Show, Eq, Ord)

coinDist = Distribution [(Heads, 0.5), (Tails, 0.5)]

twoFlips = do
    c1 <- coinDist
    c2 <- coinDist
    return (c1, c2)


nFlips :: Int -> Distribution [CoinR]
nFlips 1 = do
    c <- coinDist
    return [c]

nFlips n = do
    c <- coinDist
    rest <- nFlips $ n - 1
    return $ c:rest

lastHead :: Distribution [CoinR]
lastHead = do
    c <- coinDist
    if c == Heads then return $ [c]
    else do
        l <- lastHead
        return $ Tails:l



light1 :: Distribution Light
light1  = Distribution { dist = [(Red, 0.45), (Yellow, 0.1), (Green, 0.45)] }

data Action =  Driving | Stopped deriving (Show, Eq, Ord)

cautious_driver :: Light -> Distribution Action
cautious_driver light = do
    case light of
        Red -> Distribution [(Stopped, 1.0)]
        Yellow -> Distribution [(Stopped, 0.9), (Driving, 0.1)] 
        Green -> Distribution [(Driving, 1.0)]

aggressive_driver light = do
    case light of
        Red -> Distribution [(Stopped, 0.9), (Driving, 0.1)]
        Yellow -> Distribution [(Stopped, 0.1), (Driving, 0.9)] 
        Green -> Distribution [(Driving, 1.0)]


inverseLight light = case light of
        Green -> return Red
        Yellow -> return Red
        Red -> Distribution [(Green, 0.45 / 0.55), (Yellow, 0.1 / 0.55)]

data CarResult = Crash | Pass deriving (Show, Eq, Ord)
crash d1 d2 light = clean $ do
    l1 <- light
    l2 <- inverseLight l1
    d1Action <- d1 l1
    d2Action <- d2 l2
    if (d1Action == Driving && d2Action == Driving)
        then Distribution [(Crash, 0.9), (Pass, 0.1)]
        else return Pass


lights = do
    l1 <- light1
    l2 <- inverseLight l1
    return (l1, l2)


-- Die
dice = Distribution { dist = map (\x -> (x, 1 / 6)) [1..6] }

onlyEvens = do
    d <- dice
    if d `mod` 2 == 0 then return $ Just d
    else return Nothing