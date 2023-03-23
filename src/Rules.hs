{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-wolfram-vincent.shao
-- File description:
-- Rules
-}

module Rules where

checkRule :: Maybe Int -> Bool
checkRule Nothing = False
checkRule (Just r) = if r < 0 || r > 255 then False else True

-- rule nbr -> max byte -> result increment
getRule :: Int -> Int -> Int -> [Bool]
getRule v 1 res = if 1 + res > v then [False] else [True]
getRule v inc res
    | v < inc = [False] ++ (getRule v (inc `div` 2) res)
    | inc + res > v = [False] ++ (getRule v (inc `div` 2) res)
    | otherwise = [True] ++ (getRule v (inc `div` 2) (res + inc))

doRules :: Int -> [Bool] -> [Bool] -> [Bool]
doRules _ [] y = [False] ++ [False] ++ y ++ [False] ++ [False]
doRules c (True:True:True:xs) y =
    doRules c(True:True:xs) (y ++ [getRule c 128 0!!0])
doRules c (True:True:False:xs) y =
    doRules c(True:False:xs) (y ++ [getRule c 128 0!!1])
doRules c (True:False:True:xs) y =
    doRules c(False:True:xs) (y ++ [getRule c 128 0!!2])
doRules c (True:False:False:xs) y =
    doRules c(False:False:xs) (y ++ [getRule c 128 0!!3])
doRules c (False:True:True:xs) y =
    doRules c(True:True:xs) (y ++ [getRule c 128 0!!4])
doRules c (False:True:False:xs) y =
    doRules c(True:False:xs) (y ++ [getRule c 128 0!!5])
doRules c (False:False:True:xs) y =
    doRules c(False:True:xs) (y ++ [getRule c 128 0!!6])
doRules c (False:False:False:xs) y =
    doRules c(False:False:xs) (y ++ [getRule c 128 0!!7])
doRules c (_:xs) y = doRules c xs y