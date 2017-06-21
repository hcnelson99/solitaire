{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

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
instance Show Card where
  show (Card c r) = show c ++ show r

data Slot = Slot { _holdsOne :: Bool,
                   _stacksDown :: Bool,
                   _matchSuit :: Bool,
                   _cards :: [Card] } | Filled deriving (Show)
makeLenses ''Slot

freeSlot = Slot { _holdsOne = True, _stacksDown = True, _matchSuit = False, _cards = [] }
foundationSlot = Slot { _holdsOne = False, _stacksDown = True, _matchSuit = True, _cards = [] }
tableauSlot cards = Slot { _holdsOne = False, _stacksDown = True, _matchSuit = False, _cards = cards }


newtype Board = Board { _slots :: [Slot] } deriving (Show)
makeLenses ''Board

colors = [(Red, red), (Green, green)]

chunkCard card@(Card color _) = chunk (show card) & fore outputColor
  where outputColor = fromMaybe mempty $ lookup color colors

showSlot Filled = [chunk "++"]
showSlot Slot {_cards = [], ..} = [chunk "__"]
showSlot Slot {_cards = cards, ..} = map chunkCard cards

slice begin end = take (end - begin) . drop begin


printBoard b = let
            slots = _slots b
            free = slice 0 3 slots
            foundation = slice 3 7 slots
            printChunks = mapM_ (\c -> putChunk c >> putStr " ")
            printSingleSlots = printChunks . map (head . showSlot)
            _tableau = map _cards . drop 7 $ _slots b
            maxHeight = maximum $ map length _tableau
            padRow r = take maxHeight (map Just r ++ repeat Nothing)
            paddedRows = map padRow _tableau
            tableau = zip [1..] (transpose paddedRows)
            printRow (n, r) = do
              putStr $ show n ++ " "
              printChunks $ map (\card -> fromMaybe (chunk "  ") (chunkCard <$> card)) r
              putStr "\n"
            topNumbers =  "1  2  3     4  5  6  7"
            bottomNumbers = "8  9  10 12 13 14 15 16"
         in do
              putStr $ "  " ++ topNumbers ++ "\n" ++ "  "
              printSingleSlots free
              putStr "   "
              printSingleSlots foundation
              putStr "\n\n"
              mapM_ printRow tableau
              putStrLn $ "\n" ++ "  " ++ bottomNumbers

newBoard = do
    sDeck <- shuffleM deck
    return Board { _slots = replicate 3 freeSlot ++ replicate 4 foundationSlot ++ dealCards sDeck }

deck = [Card c r | c <- [Red, Green, Black], r <- map Number [1..9] ++ replicate 4 Dragon]

dealCards cards = if length cards <= 5 then
                    [tableauSlot cards]
                  else
                    tableauSlot (take 5 cards) : dealCards (drop 5 cards)


main :: IO ()
main = do
  board <- newBoard
  printBoard board
