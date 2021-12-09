{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
import Common
import Data.Array
import Data.Char (digitToInt)
import Data.List (sort, (\\))
import qualified Data.Set as S

import Debug.Trace

type Point = (Int, Int)
type Heightmap = Array Point Int

instance Input Heightmap where
    readInput str = array ((0, 0), (width - 1, height - 1)) elements
        where
            inputLines = lines str
            width = length (head inputLines)
            height = length inputLines
            elements =
                [ ((x, y), digitToInt e)
                | (y, ln) <- zip [0..] inputLines
                , (x, e) <- zip [0..] ln
                ]

findLocalMinima :: Heightmap -> [(Point, Int)]
findLocalMinima m =
    [ ((x, y), e)
    | x <- [0..(fst $ snd $ bounds m)]
    , y <- [0..(snd $ snd $ bounds m)]
    , let e = m ! (x, y)
    , all ((> e) . (m !)) (neighbors m (x, y))
    ]

neighbors :: Heightmap -> Point -> [Point]
neighbors m (cx, cy) =
    [ (x, y)
    | (x, y) <-
        [ (cx-1, cy)
        , (cx+1, cy)
        , (cx, cy-1)
        , (cx, cy+1)
        ]
    , inRange (bounds m) (x, y)
    ]

basin :: Heightmap -> Point -> S.Set Point
basin m start = go S.empty [start]
    where
        go visited [] = visited
        go visited (pt:pts)
            | m ! pt == 9 = visited
            | null toVisit = go (S.insert pt visited) pts
            | otherwise = go (S.insert pt visited) (pts ++ toVisit)
            where
                toVisit = [ p | p <- neighbors m pt, m ! p < 9, not (p `S.member` visited || p `elem` pts) ]

basins :: Heightmap -> [S.Set Point]
basins m = go (S.fromList [p | p <- indices m, m ! p < 9])
    where
        go pts
            | S.null pts = []
            | null b = go (pts S.\\ b)
            | otherwise = b : go (pts S.\\ b)
            where
                b = basin m (S.findMin pts)

part1 :: Heightmap -> String
part1 = show . sum . map ((+1) . snd) . findLocalMinima

part2 :: Heightmap -> String
part2 = show . product . take 3 . reverse . sort . map length . basins

main :: IO ()
main = both part1 part2
