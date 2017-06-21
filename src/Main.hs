{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.List
import Text.Read (readMaybe)
import Control.Applicative
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
foundationSlots = map (\c -> Slot { _holdsOne = False, _stacksDown = True, _matchSuit = True, _cards = [Card c (Number 0)] }) [Red, Green, Black]
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
            foundation = slice 3 6 slots
            printChunks = mapM_ (\c -> putChunk c >> putStr " ")
            printSingleSlots = printChunks . map (last . showSlot)
            _tableau = map _cards . drop 6 $ _slots b
            maxHeight = maximum $ map length _tableau
            padRow r = take maxHeight (map Just r ++ repeat Nothing)
            paddedRows = map padRow _tableau
            tableau = zip [1..] (transpose paddedRows)
            printRow (n, r) = do
              putStr $ show (maxHeight - n + 1) ++ " "
              printChunks $ map (\card -> fromMaybe (chunk "  ") (chunkCard <$> card)) r
              putStr "\n"
            topNumbers =  "1  2  3        4  5  6"
            bottomNumbers = "7  8  9  10 11 12 13 14"
         in do
              putStr $ "  " ++ topNumbers ++ "\n" ++ "  "
              printSingleSlots free
              putStr "      "
              printSingleSlots foundation
              putStr "\n\n"
              mapM_ printRow tableau
              putStrLn $ "\n" ++ "  " ++ bottomNumbers

newBoard = do
    sDeck <- shuffleM deck
    return Board { _slots = replicate 3 freeSlot ++ foundationSlots ++ dealCards sDeck }

deck = [Card c r | c <- [Red, Green, Black], r <- map Number [1..9] ++ replicate 4 Dragon]

dealCards cards = if length cards <= 5 then
                    [tableauSlot cards]
                  else
                    tableauSlot (take 5 cards) : dealCards (drop 5 cards)

color (Card c _) = c

maybeRank (Card _ (Number n)) = Just n
maybeRank (Card _ Dragon) = Nothing

canBeAfter c1 c2 stacksDown matchSuit = properColor && fromMaybe False properRank
  where
    colorTest = if matchSuit then (==) else (/=)
    properColor = colorTest (color c1) (color c2)
    rankTest = if stacksDown then (\a b -> a + 1 == b) else (\a b -> b + 1 == a)
    properRank = liftA2 rankTest (maybeRank c1) (maybeRank c2)

validStacking :: Bool -> Bool -> [Card] -> Bool
validStacking _ _ [] = error "no cards passed"
validStacking _ _ [c] = True
validStacking stacksDown matchSuit (x:y:xs) = canBeAfter x y stacksDown matchSuit && validStacking stacksDown matchSuit (y:xs)

safeHead [] = Nothing
safeHead l = Just $ head l

safeLast [] = Nothing
safeLast l = Just $ last l

canPlace :: [Card] -> Slot -> Bool
canPlace [] _ = error "No cards passed"
canPlace cards slot = correctSize && validStack
  where
    stacksDown = _stacksDown slot
    matchSuit = _matchSuit slot
    slotCards = _cards slot
    validStack = validStacking stacksDown matchSuit $ maybe cards (: cards) (safeLast slotCards)
    correctSize = not (_holdsOne slot) || (length slotCards + length cards == 1)


move sb n se b = if canPlace movingCards endSlot then over (nthSlotCards se) (++ movingCards) (over (nthSlotCards sb) (take (length sbCards - n)) b) else b
  where
    nthSlotCards i = slots . ix i . cards
    sbCards =  _cards $ _slots b !! sb
    endSlot =  _slots b !! se
    movingCards = drop (length sbCards - n) sbCards


go board = do
  printBoard board
  putStrLn "[from] [n] [to]"
  input <- getLine
  let parsed = mapM (\x -> readMaybe x :: Maybe Int) $ words input
  case parsed of
    Just (from:n:[to]) -> go $ move (from - 1) n (to - 1) board
    _ -> go board

main :: IO ()
main = do
  board <- newBoard
  go board
