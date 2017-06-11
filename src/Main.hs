{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens (makeLenses)
import Data.Maybe
import Data.List
import Control.Monad.Random
import System.Random.Shuffle
import Rainbow

data Color = Red | Green | Black deriving (Eq)
instance Show Color where
  show Red = "R"
  show Green = "G"
  show Black = "B"

data Rank = Number Int | Dragon deriving (Eq)
instance Show Rank where
  show (Number n) = show n
  show Dragon = "D"

data Card = Card Color Rank deriving (Eq)

data Slot = Empty | Filled | Has Card deriving (Show)

instance Show Card where
  show (Card c r) = show c ++ show r

data Board = Board { _free :: [Slot],
                     _foundation :: [Maybe Card],
                     _tableau :: [[Card]] }
makeLenses ''Board

showMaybeCard :: Maybe Card -> String
showMaybeCard c = fromMaybe "__" (show <$> c)

showSlot Empty = "__"
showSlot Filled = "++"
showSlot (Has c) = show c

joinWith s = foldl1 (\a b -> a ++ s ++ b)
joinWithSpaces = joinWith " "

colors = [(Red, red), (Green, green)]

slotToEither Empty = Right "__"
slotToEither Filled = Right "++"
slotToEither (Has c) = Left c

maybeCardToEither _ (Just card) = Left card
maybeCardToEither alt Nothing = Right alt

printCard (Left card@(Card color _)) = putChunk $ chunk (show card) & fore outputColor
  where outputColor = fromMaybe mempty $ lookup color colors
printCard (Right s) = putStr s

printBoard b = let
            showCells shower = joinWithSpaces . map shower
            printFree = do
              mapM_ (\c -> printCard (slotToEither c) >> putStr " ") $ _free b
            printFoundation = do
              mapM_ (\c -> printCard (maybeCardToEither "__" c) >> putStr " ") $ _foundation b
            maxHeight = maximum $ map length (_tableau b)
            padRow r = take maxHeight (map Just r ++ repeat Nothing)
            paddedRows = map padRow (_tableau b)
            tableau = zip [1..] (transpose paddedRows)
            printRow (n, r) = do
              putStr $ show n ++ " "
              mapM_ (\c -> printCard (maybeCardToEither "  " c) >> putStr " ") r
              putStr "\n"
            topNumbers =  "1  2  3     4  5  6  7"
            bottomNumbers = "8  9  10 12 13 14 15 16"
         in do
              putStr $ "  " ++ topNumbers ++ "\n" ++ "  "
              printFree
              putStr "   "
              printFoundation
              putStr "\n\n"
              mapM_ printRow tableau
              putStrLn $ "\n" ++ "  " ++ bottomNumbers

newDeck = do
    deck <- shuffleM cards
    return Board { _free = replicate 3 Empty,
                        _foundation = replicate 4 Nothing,
                        _tableau = dealCards deck}

cards = [Card c r | c <- [Red, Green, Black], r <- map Number [1..9] ++ replicate 4 Dragon]

dealCards :: [a] -> [[a]]
dealCards cards = if length cards <= 5 then
                    [cards]
                  else
                    take 5 cards : dealCards (drop 5 cards)

toSlot :: Maybe Card -> Slot
toSlot Nothing = Empty
toSlot (Just c) = Has c

getCard :: Board -> Int -> Slot
getCard b n
  | n <= 0 || n > 16 = undefined
  | n <= 3 = _free b !! (n - 1)
  | n <= 7 = toSlot $ _foundation b !! (n - 4)
  | n <= 14 =  let col = _tableau b !! (n - 8) in
                  if col == [] then Empty else Has $ last col

canPlace :: Card -> Card -> Int -> Bool
canPlace (Card _ Dragon) _ _ = False
canPlace _ (Card _ Dragon) _ = False
canPlace (Card sc (Number sr)) (Card dc (Number dr)) di = sc /= dc && stacks
  where stacks = if 4 <= di && di <= 7 then dr + 1 == sr else dr - 1 == sr

move :: Board -> Int -> Int -> Maybe Board
move b si di = case (source, dest) of
                (Has s, Has d) -> if canPlace s d di then Just b else Nothing
                (Has s, Empty) -> Just b
                (Empty, _) -> Nothing
                (Filled, _) -> Nothing
                (_, Filled) -> Nothing
  where
    source = getCard b si
    dest = getCard b di

main :: IO ()
main = do
  deck <- newDeck
  printBoard deck
