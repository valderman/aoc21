{-# LANGUAGE FlexibleInstances #-}
module Common where
import Data.List (group, sort)

class Input a where
    readInput :: String -> a

both :: Input a => (a -> String) -> (a -> String) -> IO ()
both part1 part2 = interact $ \str -> let input = readInput str in unlines [part1 input, part2 input]

todo :: Input a => a -> String
todo _ = "TODO part2"

-- | Non-stable O(n log n) version of nub.
snub :: (Ord a, Eq a) => [a] -> [a]
snub = map head . group . sort

single :: Show a => [a] -> a
single [x] = x
single xs = error $ "length xs /= 1: " ++ show xs