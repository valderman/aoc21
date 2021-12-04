{-# LANGUAGE RankNTypes #-}
import Common
import Data.List (foldl', transpose, partition)
import Data.Maybe (catMaybes, isNothing)

newtype Card = Card [[Maybe Int]]

instance Show Card where
    show (Card c) = "Bingo Card\n" ++ unlines (bar : map (unwords . showLine) c ++ [bar])
        where
            bar = replicate 18 '-'
            showLine ln = "|" : map (pad 2 . showSquare) ln ++ ["|"]

showSquare :: Maybe Int -> String
showSquare = maybe "  " show

data Bingo = Bingo
  { numbers :: [Int]
  , cards :: [Card]
  } deriving Show

instance Input Bingo where
    readInput str = Bingo
      { numbers = map read $ splitAll ',' $ head lns
      , cards = map Card $ chunk 5 $ map (map (Just . read) . words) $ filter (not . null) $ tail lns
      }
        where
            lns = lines str

pad :: Int -> String -> String
pad n s = replicate (n - length s) ' ' ++ s

splitAll :: Eq a => a -> [a] -> [[a]]
splitAll _ [] = []
splitAll c s = case break (== c) s of
    (x, xs) -> x : splitAll c (drop 1 xs)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

isWinningCard :: Card -> Bool
isWinningCard (Card c) = any (all isNothing) c || any (all isNothing) (transpose c)

mark :: Int -> Card -> Card
mark n (Card c) = Card $ map (map (\x -> if x == Just n then Nothing else x)) c

score :: Int -> Card -> Int
score finalNumber (Card c) = sum (catMaybes $ concat c) * finalNumber

-- | Play a game of bingo, returning all winning cards and their winning numbers,
--   sorted in the order in which they won.
play :: Bingo -> [([Card], Int)]
play bingo = reverse $ snd $ foldl' playRound (cards bingo, []) (numbers bingo)

playAndScore :: (forall a. [a] -> a) -> Bingo -> Int
playAndScore pick bingo = case play bingo of
    xs@(_:_) -> let (winningCard, winningNumber) = pick xs in maximum $ map (score winningNumber) winningCard
    _ -> 0

playRound :: ([Card], [([Card], Int)]) -> Int -> ([Card], [([Card], Int)])
playRound (unmarkedCards, finishedCards) number = (unfinished, updatedFinishedCards)
    where
        markedCards = map (mark number) unmarkedCards
        (finished, unfinished) = partition isWinningCard markedCards
        updatedFinishedCards
            | null finished = finishedCards
            | otherwise = (finished, number) : finishedCards

main :: IO ()
main = both part1 part2

part1 :: Bingo -> String
part1 = show . playAndScore head

part2 :: Bingo -> String
part2 = show . playAndScore last