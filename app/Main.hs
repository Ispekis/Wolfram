{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-wolfram-vincent.shao
-- File description:
-- Main
-}

module Main (main) where
import System.Environment
import System.Exit

import Lib

main :: IO()
main = do
    args <- getArgs
    let res = getOpts defaultConf args
    case res of
        Just c -> doWolfram c
        Nothing -> (exitWithMsg "Error on arguments" (ExitFailure 84))