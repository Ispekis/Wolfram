{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-wolfram-vincent.shao
-- File description:
-- Utils
-}

module Utils where

myAbs :: Int -> Int
myAbs x
    | x < 0 = -x
    | otherwise = x

myIsMaybeNeg :: Maybe Int -> Maybe Int
myIsMaybeNeg Nothing = Nothing
myIsMaybeNeg (Just x) = if (x < 0) then Nothing else (Just x)

myIsNeg :: Int -> Bool
myIsNeg x = x < 0

removeNthList :: Int -> [a] -> [a]
removeNthList _ [] = []
removeNthList 0 xs = xs
removeNthList n (_:xs) = removeNthList (n - 1) xs