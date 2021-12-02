{-# LANGUAGE FlexibleInstances #-}
module Common where

class Input a where
    readInput :: String -> a

instance Input [Int] where
    readInput = map read . words

both :: Input a => (a -> String) -> (a -> String) -> IO ()
both part1 part2 = interact $ \str -> let input = readInput str in unlines [part1 input, part2 input]

todo :: Input a => a -> String
todo _ = "TODO part2"
