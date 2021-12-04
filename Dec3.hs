{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
import Common
import Data.List (transpose, foldl')

instance Input [[Bool]] where
    readInput = map (map (== '1')) . lines

main :: IO ()
main = both part1 part2

part1 :: [[Bool]] -> String
part1 inp = show $ gamma * epsilon
    where
        gamma = toDecimal $ mostCommonBits inp
        epsilon = toDecimal $ map not $ mostCommonBits inp

part2 :: [[Bool]] -> String
part2 inp = show $ oxygen * co2
    where
        oxygen = toDecimal $ findNumber True inp
        co2 = toDecimal $ findNumber False inp

-- | Find the most common bit: -1 means 0, 1 means 1,
--   and 0 means both bits are equally common.
mostCommonBit :: [Bool] -> Int
mostCommonBit = signum . foldl' (\a x -> if x then a+1 else a-1) 0

mostCommonBits :: [[Bool]] -> [Bool]
mostCommonBits = map ((>= 0) . mostCommonBit) . transpose

toDecimal :: [Bool] -> Int
toDecimal = foldl' (\n b -> n*2 + if b then 1 else 0) 0

findNumber :: Bool -> [[Bool]] -> [Bool]
findNumber oxygen numbers = head $ foldl' step numbers [0..length numbers-1]
    where
        step [number] _ = [number]
        step ns pos = filter (\xs ->  xs !! pos == bit) ns
            where bit = (if oxygen then id else not) $ mostCommonBitInPosition ns pos

mostCommonBitInPosition :: [[Bool]] -> Int -> Bool
mostCommonBitInPosition numbers pos = mostCommonBits numbers !! pos
