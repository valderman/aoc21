{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
import Common

instance Input [Int] where
    readInput = map read . words

main :: IO ()
main = both part1 part2

part1 :: [Int] -> String
part1 = show . countIncreases

part2 :: [Int] -> String
part2 = show . countIncreases . map sum . window 3

window :: Int -> [a] -> [[a]]
window n xs
  | n <= length xs = take n xs : window n (tail xs)
  | otherwise      = []

countIncreases :: Ord a => [a] -> Int
countIncreases xs = length $ filter (uncurry (<)) $ zip xs (tail xs)
