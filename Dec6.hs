{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
import Common
import Data.List (sort, group)

instance Input [Int] where
    readInput [] = []
    readInput str = denseFishState
        where
            fishes = map (read @Int) . words . map (\x -> if x == ',' then ' ' else x) $ str
            sparseFishState = map (\xs -> (head xs, length xs)) $ group $ sort fishes
            denseFishState = map (\n -> maybe 0 id $ lookup n sparseFishState) [0..8]

step :: [Int] -> [Int]
step fishes = steppedFishes
    where
        (newFishes : oneToSixFishes, [sevenFishes, eightFishes]) = splitAt 7 fishes  
        steppedFishes = concat [oneToSixFishes, [sevenFishes + newFishes, eightFishes, newFishes]]

simulate :: Int -> [Int] -> Int
simulate days inp = sum $ foldl (\fishes _ -> step fishes) inp [1..days]

part1 :: [Int] -> String
part1 = show . simulate 80

part2 :: [Int] -> String
part2 = show . simulate 256

main :: IO ()
main = both part1 part2
