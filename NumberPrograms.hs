#!/usr/bin/env runghc
module NumberPrograms where

import System.Environment

data NumberCode = Code Int Int
data InstrCode = Null | EvenPair Int Int | OddPair Int NumberCode

instance Show NumberCode where
  show (Code i j)
    = "L" ++ show i ++ ", L" ++ show j

instance Show InstrCode where
  show Null 
    = "Halt"
  show (EvenPair i j)
    = "R" ++ show (i `div` 2) ++ "[+] -> L" ++ show j
  show (OddPair i c)
    = "R" ++ show (i `div` 2) ++ "[-] -> " ++ show c


main :: IO()
-- main entry to the command line tool
-- Only argument represents the number to translate into a program
main = do
  args <- getArgs
  (putStr . transProgram . read . head) args
  


transProgram :: Int -> String
-- Prints the result of the program for the given number
transProgram number
  = concat $ zipWith (++) instrLines (map func (instrListFromBinary (toBin number)))
    where
      func            = addLineBreak . show . toInstrCode
      addLineBreak s  = s ++ "\n"
      instrLines      = ["L" ++ show i ++ " : " | i <- [0..]]

toInstrCode :: Int -> InstrCode
-- Translates a given instruction number to an instruction
toInstrCode 0 = Null
toInstrCode n
  | even x = EvenPair (x `div` 2) y
  | otherwise = OddPair x (Code i j)
    where
      (x, y) = splitNumberInfo 1 (toBin n)
      (i, j) = splitNumberInfo 0 (toBin x)


splitNumberInfo :: Int -> [Int] -> (Int, Int)
-- Computes the number pair from its binary representation
splitNumberInfo delim
  = (splitNumberInfo' 0) . reverse
    where
      splitNumberInfo' x [] = (x,0)
      splitNumberInfo' x (b:bs)
        | b /= delim = splitNumberInfo' (x+1) bs
        | otherwise  = (x, toDecimal (reverse bs))
      toDecimal = foldl (\n c -> 2*n + c) 0


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
