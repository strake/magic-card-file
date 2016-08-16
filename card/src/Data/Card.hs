{-# LANGUAGE OverloadedStrings #-}

module Data.Card (Rarity (..), Frame (..), Code (..), Card (..), codeRE, cardRE, natRE) where

import Data.Char
import Numeric.Natural
import Text.Read
import Text.Regex.Applicative

data Rarity = Common | Uncommon | Rare deriving (Eq, Ord, Show)

data Frame = W | U | B | R | G | A | L deriving (Eq, Ord, Read, Show, Enum)

data Code = Code
    { codeRarity :: Rarity,
      codeFrame  :: Frame,
      codeNumber :: Natural }
  deriving (Eq, Ord)

instance Show Code where
    show (Code rarity frame m) = take 1 (show rarity) ++ show frame ++ pad 2 '0' (show m)

data Card = Card
    { cardCode :: Code,
      cardName :: [Char],
      cardType :: [Char],
      cardCost :: Maybe [Char],
      cardPT :: Maybe [Char],
      cardText :: [Char] }

codeRE :: RE Char Code
codeRE = Code <$> (Common <$ "C" <|> Uncommon <$ "U" <|> Rare <$ "R")
              <*> msym (readMaybe . pure) <*> natRE

cardRE :: RE Char Card
cardRE = Card <$   "Code:\t" <*> codeRE
              <*> ("\nName:\t" *> lnRE <|> pure "")
              <* "\nType:\t" <*> lnRE
              <*> optional ("\nCost:\t" *> lnRE)
              <*> optional ("\nP/T:\t" *> lnRE)
              <*> (dropWhile (== '\n') <$> many anySym)
  where lnRE = many (psym (/= '\n'))

natRE :: RE Char Natural
natRE = read <$> many (psym isDigit)

pad :: Int -> a -> [a] -> [a]
pad n x xs | length xs < n = replicate (n - length xs) x ++ xs
           | otherwise = xs
