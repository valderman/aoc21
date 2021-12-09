{-# LANGUAGE FlexibleInstances #-}
import Common
import Data.Function (on)
import Data.List (sort, sortBy, (\\), intersect)
import Data.Maybe (fromJust)

data Configuration = Configuration
  { patterns :: [String]
  , output :: [String]
  }

instance Input Configuration where
    readInput str = Configuration
        { patterns = map sort $ words pat
        , output = map sort $ words out
        }
        where
            (pat, '|':out) = break (== '|') str

instance Input [Configuration] where
    readInput = map readInput . lines

isUnique :: String -> Bool
isUnique = flip elem [2, 3, 4, 7] . length

mapConnections :: Configuration -> [(Char, Char)]
mapConnections (Configuration ps _) =
        [ (a, 'a')
        , (b, 'b')
        , (c, 'c')
        , (d, 'd')
        , (e, 'e')
        , (f, 'f')
        , (g, 'g')
        ]
    where
        -- Lord, forgive me, for I don't know what I'm doing
        nonUnique = filter (not . isUnique) ps
        [one, seven, four, eight] = sortBy (compare `on` length) $ filter isUnique ps
        three = single $ filter ((== 2) . length . (\\ seven)) nonUnique
        bottomRightVertical = single $ filter (\x -> length x == 1 && not (x `elem` [[e],[d]])) $ map (eight \\) nonUnique
        a = single $ seven \\ four
        b = single $ four \\ (seven ++ three)
        c = single $ one `intersect` bottomRightVertical
        d = single $ (intersect three four) \\ one
        e = single $ eight \\ (three ++ four)
        f = single $ one \\ bottomRightVertical
        g = single $ three \\ (seven ++ four)

decode :: Configuration -> [(Char, Char)] -> Configuration
decode (Configuration ps out) connections = Configuration ps [ sort $ map (fromJust . flip lookup connections) signal | signal <- out]

outputNumber :: Configuration -> Int
outputNumber (Configuration _ out) = read $ map lookupDigit out

lookupDigit :: String -> Char
lookupDigit s = maybe (error $ "digit not found: " ++ s) id $ lookup s digitMap

digitMap :: [(String, Char)]
digitMap =
    [ ("abcefg", '0')
    , ("cf", '1')
    , ("acdeg", '2')
    , ("acdfg", '3')
    , ("bcdf", '4')
    , ("abdfg", '5')
    , ("abdefg", '6')
    , ("acf", '7')
    , ("abcdefg", '8')
    , ("abcdfg", '9')
    ]

solve :: Configuration -> Int
solve c = outputNumber $ decode c (mapConnections c)

part1 :: [Configuration] -> String
part1 = show . length . concatMap (filter isUnique . output)

part2 :: [Configuration] -> String
part2 = show . sum . map solve

main :: IO ()
main = both part1 part2
