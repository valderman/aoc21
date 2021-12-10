{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
import Common
import Data.List (sort, foldl')
import Data.Maybe (catMaybes)

instance Input [String] where
    readInput = lines

data Status = OK | Corrupted String String | Incomplete String
    deriving (Eq, Show)

closer :: Char -> Char
closer '(' = ')'
closer '{' = '}'
closer '[' = ']'
closer '<' = '>'
closer _ = error "¯\\_(ツ)_/¯"

repairString :: String -> Maybe String
repairString s = case diagnose s of
    Incomplete s' -> Just $ map closer s'
    _ -> Nothing

diagnose :: String -> Status
diagnose = go []
    where
        go ('(':ps) (')':is) = go ps is
        go ('{':ps) ('}':is) = go ps is
        go ('[':ps) (']':is) = go ps is
        go ('<':ps) ('>':is) = go ps is
        go ps (i:is)
            | i `elem` "({[<" = go (i:ps) is
            | otherwise = Corrupted ps (i:is)
        go [] [] = OK
        go ps [] = Incomplete ps

part1 :: [String] -> String
part1 = show . sum . catMaybes . map (flip lookup valueTable) . badChars
    where
        isCorrupted (Corrupted _ _) = True
        isCorrupted _ = False
        badChars = map (\(Corrupted _ (i:_)) -> i) . filter isCorrupted . map diagnose
        valueTable =  [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

part2 :: [String] -> String
part2 = show . takeMiddle . sort . map score . catMaybes . map repairString 
    where
        score = foldl' (\a x -> a*5 + x) 0 . catMaybes . map (flip lookup valueTable)
        valueTable =  [(')', 1), (']', 2), ('}', 3), ('>', 4)]
        takeMiddle xs = head $ drop (length xs `quot` 2) xs

main :: IO ()
main = both part1 part2
