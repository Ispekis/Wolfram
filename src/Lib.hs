{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-wolfram-vincent.shao
-- File description:
-- Lib
-}

module Lib  where

import System.IO (hPutStrLn, stderr)
import System.Exit
import Rules
import Utils
import Data.Maybe

data Conf = Conf {
    rule :: Maybe Int,
    start :: Maybe Int,
    line :: Maybe Int,
    window :: Maybe Int,
    move :: Maybe Int
} deriving (Show)

defaultConf :: Conf
defaultConf = Conf {
    rule=Nothing, start=Just 0, line=Nothing, window=Just 80, move=Just 0
    }

checkAllInt :: [Char] -> Bool
checkAllInt [] = True
checkAllInt (c:cs)
    | ((c < '0' || c > '9') && c /= '-') = False
    | otherwise = checkAllInt cs

readInt :: [Char] -> Maybe Int
readInt c
    | checkAllInt c = Just (read c :: Int)
    | otherwise = Nothing

getOpts :: Conf -> [String] -> Maybe Conf
getOpts c [] = Just c
getOpts (Conf _ s l w m) ("--rule":x:xs) =
    if isNothing (readInt x)
        then Nothing else getOpts (Conf (readInt x) s l w m) xs
getOpts (Conf r _ l w m) ("--start":x:xs) =
    if isNothing (myIsMaybeNeg (readInt x))
        then Nothing else getOpts (Conf r (readInt x) l w m) xs
getOpts (Conf r s _ w m) ("--lines":x:xs) =
    if isNothing (myIsMaybeNeg (readInt x))
        then Nothing else getOpts (Conf r s (readInt x) w m) xs
getOpts (Conf r s l _ m) ("--window":x:xs) =
    if isNothing (myIsMaybeNeg (readInt x))
        then Nothing else getOpts (Conf r s l (readInt x) m) xs
getOpts (Conf r s l w _) ("--move":x:xs) =
    if isNothing (readInt x)
        then Nothing else getOpts (Conf r s l w (readInt x)) xs
getOpts _ _ = Nothing

exitWithMsg :: String -> ExitCode -> IO ()
exitWithMsg str code = hPutStrLn stderr str >> exitWith code

getMiddle :: Int -> Int
getMiddle i = if (i `mod` 2 == 0) then (i `div` 2) + 1 else i `div` 2

startPattern :: Int -> [Bool]
startPattern w
    | w < 5 = [False,False,True,False,False]
    | w `mod` 2 == 0 = replicate (getMiddle w - 1) False ++ [True] ++
    replicate (w - getMiddle w) False
    | otherwise = replicate (getMiddle w) False ++ [True] ++
    replicate (w - getMiddle w - 1) False

display :: Conf -> String -> Int -> IO ()
display _ [] _ = return ()
display (Conf r s l w m) (x:xs) inc
    | inc >= fromJust w = display (Conf r s l w m) xs (inc + 1)
    | otherwise = putChar x >> display (Conf r s l w m) xs (inc + 1)

doShift :: Int -> [Bool] -> [Bool]
doShift m xs
    |myIsNeg m = removeNthList (myAbs m) xs ++ replicate (myAbs m) False
    | otherwise = replicate m False ++ xs

doMove :: Int -> Int -> Int -> [Bool] -> String
doMove 3 m pos xs =
    removeNthList 1 (removeNthList pos (convertToString (doShift m xs)))
doMove 2 m pos xs =
    removeNthList 1 (removeNthList pos (convertToString (doShift m xs)))
doMove 1 m pos xs =
    removeNthList 2 (removeNthList pos (convertToString (doShift m xs)))
doMove _ m pos xs = removeNthList pos (convertToString (doShift m xs))

exec :: Conf -> [Bool] -> Int-> IO ()
exec (Conf r (Just s) Nothing w m) xs inc
    | inc < s =
        exec (Conf r (Just s) Nothing w m) (doRules (fromJust r) xs []) (inc+1)
    | otherwise =
        display (Conf r (Just s) Nothing w m)
        (doMove (fromJust w) (fromJust m) inc xs) 0 >>
        putStrLn "" >>
        exec (Conf r (Just s) Nothing w m) (doRules (fromJust r) xs []) (inc+1)
exec (Conf r (Just s) (Just l) w m) xs inc
    | inc >= l + s = putStr ""
    | inc < s =
        exec (Conf r (Just s) (Just l) w m) (doRules (fromJust r) xs [])(inc+1)
    | otherwise =
        display (Conf r (Just s) (Just l) w m)
        (doMove (fromJust w) (fromJust m) inc xs) 0>>
        putStrLn "" >>
        exec (Conf r (Just s) (Just l) w m) (doRules (fromJust r) xs [])(inc+1)
exec _ _ _ = return ()

convertToString :: [Bool] -> String
convertToString [] = ""
convertToString (False:xs) = " " ++ convertToString xs
convertToString (True:xs) = "*" ++ convertToString xs

doWolfram :: Conf -> IO ()
doWolfram (Conf r _ _ _ _) | not (checkRule r) =
    exitWithMsg "rule option is missing or incorrect" (ExitFailure 84)
doWolfram (Conf r s l (Just w) m) =
    exec (Conf r s l (Just w) m) (startPattern w) 0
doWolfram _ = return ()
