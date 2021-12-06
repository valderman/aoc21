{-# LANGUAGE FlexibleInstances #-}
import Common
import Data.List (foldl')

data Command = Forward Int | Up Int | Down Int

instance Input [Command] where
    readInput = map (rd . words) . lines
        where
            rd ["up", n] = Up (read n)
            rd ["down", n] = Down (read n)
            rd ["forward", n] = Forward (read n)
            rd s = error $ "bad input: " ++ (unwords s) 

main :: IO ()
main = both part1 part2

part1 :: [Command] -> String
part1 = show . (uncurry (*)) . findDestination

part2 :: [Command] -> String
part2 route = show $ x*y
    where (_, x, y) = findDestinationWithAim route

findDestination :: [Command] -> (Int, Int)
findDestination = foldl' step (0, 0)
    where
        step (x, y) (Up n) = (x, y-n)
        step (x, y) (Down n) = (x, y+n)
        step (x, y) (Forward n) = (x+n, y)

findDestinationWithAim :: [Command] -> (Int, Int, Int)
findDestinationWithAim = foldl' step (0, 0, 0)
    where
        step (aim, x, y) (Up n) = (aim-n, x, y)
        step (aim, x, y) (Down n) = (aim+n, x, y)
        step (aim, x, y) (Forward n) = (aim, x+n, y+aim*n)
