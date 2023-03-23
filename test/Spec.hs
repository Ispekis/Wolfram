{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-PAR-4-1-wolfram-vincent.shao
-- File description:
-- Spec
-}

import Lib
import Rules
import Utils
import Test.HUnit
import qualified System.Exit as Exit

test1 :: Test
test1 = TestCase (assertEqual "myAbs neg" 1 (myAbs (-1)))
test2 :: Test
test2 = TestCase (assertEqual "myAbs pos" 1 (myAbs (1)))

test3 :: Test
test3 = TestCase (assertEqual "myIsMaybeNeg Nothing" Nothing (myIsMaybeNeg Nothing))
test4 :: Test
test4 = TestCase (assertEqual "myIsMaybeNeg Neg" Nothing (myIsMaybeNeg (Just (-1))))
test5 :: Test
test5 = TestCase (assertEqual "myIsMaybeNeg Pos" (Just 1) (myIsMaybeNeg (Just 1)))

test6 :: Test
test6 = TestCase (assertEqual "getRule 30" [False,False,False,True,True,True,True,False] (getRule 30 128 0))
test7 :: Test
test7 = TestCase (assertEqual "getRule 31" [False,False,False,True,True,True,True,True] (getRule 31 128 0))

test8 :: Test
test8 = TestCase (assertEqual "checkRule small" False (checkRule (Just (-1))))
test9 :: Test
test10 :: Test
test9 = TestCase (assertEqual "checkRule bigger" False (checkRule (Just 256)))
test10 = TestCase (assertEqual "checkRule correct" True (checkRule (Just 128)))

test11 :: Test
test11 = TestCase (assertEqual "doRule 30" [False,False,True,True,True,False,False] (doRules 30 [False,False,True,False,False] []))

test12 :: Test
test12 = TestCase (assertEqual "myIsNeg Neg" True (myIsNeg (-1)))

test13 :: Test
test13 = TestCase (assertEqual "myIsNeg Pos" False (myIsNeg 1))

-- test14 :: Test
-- test14 = TestCase (assertEqual "defaultConf" (Conf Nothing (Just 0) Nothing (Just 80) (Just 0)) (defaultConf))

-- test15 :: Test
-- test15 = TestCase (assertEqual "ReadInt valide" False (readInt "8"))

-- test16 :: Test
-- test16 = TestCase (assertEqual "myIsNeg" False (checkRule (Just 256)))

-- test17 :: Test
-- test17 = TestCase (assertEqual "myIsNeg" False (checkRule (Just 256)))


tests :: Test
tests = TestList [
    TestLabel "myAbs" test1,
    TestLabel "myAbs" test2,
    TestLabel "myIsMaybeNeg" test3,
    TestLabel "myIsMaybeNeg" test4,
    TestLabel "myIsMaybeNeg" test5,
    TestLabel "getRule" test6,
    TestLabel "getRule" test7,
    TestLabel "checkRule" test8,
    TestLabel "checkRule" test9,
    TestLabel "checkRule" test10,
    TestLabel "doRule" test11,
    TestLabel "myIsNeg" test12,
    TestLabel "myIsNeg" test13
    ]

main :: IO ()
main = do
    res <- runTestTT tests
    if failures res > 0 then Exit.exitFailure else Exit.exitSuccess
