#!/usr/bin/env runghc
module NumberPrograms where

import System.Environment

data InstrCode = Null | EvenFunc Int Int | OddFunc Int Int Int

instance Show InstrCode where
  show Null 
    = "Halt"
  show (EvenFunc i j)
    = "R" ++ show (i `div` 2) ++ "[+] -> L" ++ show j
  show (OddFunc i a b)
    = "R" ++ show (i `div` 2) ++ "[-] -> " ++ "L" ++ show a ++ ", L" ++ show b


main :: IO()
-- main entry to the command line tool
-- Only argument represents the number to translate into a program
main = do
  args <- getArgs
  (putStr . concat . lineBreaks . transProgram . read . head) args
  where
    lineBreaks = flip  (zipWith (++)) (repeat "\n")


transProgram :: Int -> [String]
-- Translates the program into a list of readable instructions
transProgram
  = (zipWith (++) instrLines) . (map func . instrListFromBinary . toBin)
    where
      func            = show . toInstrCode
      instrLines      = ["L" ++ show i ++ " : " | i <- [0..]]

toInstrCode :: Int -> InstrCode
-- Translates a given instruction number to an instruction
toInstrCode 0 = Null
toInstrCode n
  | even x = EvenFunc (x `div` 2) y
  | otherwise = OddFunc x i j
    where
      (x, y) = splitNumberInfo 1 (toBin n)
      (i, j) = splitNumberInfo 0 (toBin y)


splitNumberInfo :: Int -> [Int] -> (Int, Int)
-- Computes the number pair from its binary representation
splitNumberInfo delim
  = (splitNumberInfo' 0) . reverse
    where
      splitNumberInfo' x [] = (x,0)
      splitNumberInfo' x (b:bs)
        | b /= delim = splitNumberInfo' (x+1) bs
        | otherwise  = (x, foldr (\c n -> 2*n + c) 0 bs)


instrListFromBinary :: [Int] -> [Int]
-- Computes the list of instructions that the program has to execute
instrListFromBinary
  = (instrListFromBinary' 0) . reverse
    where
      instrListFromBinary' n [] = []
      instrListFromBinary' n (0:xs) = instrListFromBinary' (n+1) xs
      instrListFromBinary' n (1:xs) = n : instrListFromBinary' 0 xs


toBin :: Int -> [Int]
-- converts a decimal number into a binary list representation
toBin n
  | n < 2     = [n]
  | otherwise = toBin (n `div` 2) ++ [n `mod` 2]
