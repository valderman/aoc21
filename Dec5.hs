{-# LANGUAGE FlexibleInstances #-}
import Common
import Data.List

data Point = Point {px :: Int,  py :: Int}
    deriving (Show, Eq, Ord)

data Line = Line Point Point
    deriving (Show, Eq, Ord)

instance Input [Line] where
    readInput = map (readLine . words) . lines
        where
            readLine [p1, "->", p2] = Line (readPoint p1) (readPoint p2)
            readLine xs = error $ "not a line: " ++ show xs
            readPoint s = case break (== ',') s of
                (x, ',':y) -> Point (read x) (read y)
                _ -> error $ "not a point: " ++ s

-- | Super broken for any line with slope not in [0, 45, 90] degrees
pointsOnLine :: Line -> [Point]
pointsOnLine (Line p1 p2) =
    [ Point x y
    | (x, y) <- zip (pad (length yvals) (px p1) xvals)
                    (pad (length xvals) (py p1) yvals)
    ]
    where
        pad n val xs = xs ++ replicate (n - length xs) val
        step f = case f p2 - f p1 of
            0 -> 1
            n -> signum n
        xvals = [ px p1, px p1 + step px .. px p2 ]
        yvals = [ py p1, py p1 + step py .. py p2 ]

isHorizontalOrVertical :: Line -> Bool
isHorizontalOrVertical (Line (Point x1 y1) (Point x2 y2)) = x1 == x2 || y1 == y2

duplicates :: Ord a => [a] -> [a]
duplicates = map head . filter((> 1) . length) . group . sort

countOverlappingPoints :: [Line] -> Int
countOverlappingPoints = length . duplicates . concatMap pointsOnLine

part1 :: [Line] -> String
part1 = show . countOverlappingPoints . filter isHorizontalOrVertical

part2 :: [Line] -> String
part2 = show . countOverlappingPoints

main :: IO ()
main = both part1 part2
