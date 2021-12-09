{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
import Common

instance Input [Int] where
    readInput = map (read @Int) . words . map (\x -> if x == ',' then ' ' else x)

minFuelCost :: Ord a => (Int -> [Int] -> a) -> [Int] -> a
minFuelCost fuelCost crabs = minimum $ map (flip fuelCost crabs) [minimum crabs .. maximum crabs]

simpleFuelCost :: Int -> [Int] -> Int
simpleFuelCost pos = sum . map (abs . (pos -))

part1 :: [Int] -> String
part1 = show . minFuelCost simpleFuelCost

crabbyFuelCost :: Int -> [Int] -> Int
crabbyFuelCost pos = sum . map cost
    where
        cost n = f 0 0 (abs $ pos - n)
        f !acc step 0 = acc + step
        f !acc step n = f (acc + step) (step + 1) (n - 1)

part2 :: [Int] -> String
part2 = show . minFuelCost crabbyFuelCost

main :: IO ()
main = both part1 part2

