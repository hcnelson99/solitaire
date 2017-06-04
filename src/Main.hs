{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe


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

instance Show Board where
  show b = let
              free = foldl1 (++) $ map (\c -> showFreeCell c ++ " ") $ _free b
              foundation = foldl1 (++) $ map (\c -> showMaybeCard c ++ " ") $ _foundation b
           in free ++ "   " ++ foundation ++ "\n\n" ++ test

startingBoard = Board { _free = replicate 3 (Left Nothing),
                        _foundation = replicate 4 Nothing,
                        _tableau = replicate 8 (replicate 7 (Card Red Dragon)) }


main :: IO ()
main = do
  putStrLn $ show startingBoard
