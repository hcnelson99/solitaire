{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.List
import Control.Monad.Random
import System.Random.Shuffle



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

data Filled = Filled

instance Show Card where
  show (Card c r) = show c ++ show r

data Board = Board { _free :: [Either (Maybe Card) Filled],
                     _foundation :: [Maybe Card],
                     _tableau :: [[Card]] }
makeLenses ''Board

showMaybeCard :: Maybe Card -> String
showMaybeCard c = fromMaybe "__" (show <$> c)

showFreeCell :: Either (Maybe Card) Filled -> String
showFreeCell (Left m) = showMaybeCard m
showFreeCell (Right Filled) = "++"

joinWith s = foldl1 (\a b -> a ++ s ++ b)
joinWithSpaces = joinWith " "

instance Show Board where
  show b = let
              showCells shower = joinWithSpaces . map shower
              free = showCells showFreeCell $ _free b
              foundation = showCells showMaybeCard $ _foundation b
              maxHeight = maximum $ map length (_tableau b)
              padRow r = take maxHeight (map Just r ++ repeat Nothing)
              paddedRows = map padRow (_tableau b)
              shownRows = map (map (maybe "  " show)) paddedRows
              tableau = joinWith "\n" $ map joinWithSpaces $ transpose shownRows
              numberLabel s e = joinWith "  " $ map show [s..e]
              topNumbers =  numberLabel 1 3 ++ "     " ++ numberLabel 4 7
           in topNumbers ++ "\n" ++
              free ++  "    " ++ foundation ++ "\n\n" ++
              tableau ++ "\n\n" ++
              numberLabel 8 14

newDeck = do
    deck <- shuffleM cards
    return Board { _free = replicate 3 (Left Nothing),
                        _foundation = replicate 4 Nothing,
                        _tableau = dealCards deck}

cards = [Card c r | c <- [Red, Green, Black], r <- map Number [1..9] ++ replicate 4 Dragon]

dealCards :: [a] -> [[a]]
dealCards cards = if length cards <= 5 then
                    [cards]
                  else
                    take 5 cards : dealCards (drop 5 cards)


main :: IO ()
main = do
  deck <- newDeck
  print deck
