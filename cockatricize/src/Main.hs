{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Main where

import Data.Bool (bool)
import Data.Card
import Data.List.Split
import Data.Maybe
import Data.Sgml
import Numeric.Natural
import Text.Read
import Text.Regex.Applicative

main :: IO ()
main = interact $
       splitOn "\n\n\n" & fmap (\ xs -> fromMaybe (error ("Ill-formed card:\n\n" ++ xs)) $ match cardRE xs) &
       filter (cardCode & \ (Code _ _ n) -> n <= 50) & zipWith toSgml [1..] & Tag "cards" [] & toChars
  where (&) = flip (.)

toSgml :: Natural -> Card -> Sgml
toSgml n (Card {..}) =
    Tag "card" [] $ concat
    [[Tag "name" [] [CData (bool cardName (show cardCode) (null cardName))],
      Tag "set" [("rarity", show rarity), ("num", show n)] [CData setName]],
     bool [] (pure . Tag "color" [] . pure . CData . show $ frame) (frame ∈ [W, U, B, R, G]),
     maybe [] (pure . Tag "manacost" [] . pure . CData) cardCost,
     maybe [] (pure . Tag "cmc" [] . pure . CData . show) (convert =<< cardCost),
     [Tag "type" [] [CData cardType]],
      maybe [] (pure . Tag "pt" [] . pure . CData) cardPT,
     [Tag "tableRow" [] [CData (show $ tableRow cardType)],
      Tag "text" [] [CData cardText]]]
  where Code rarity frame _ = cardCode

convert :: [Char] -> Maybe Natural
convert = match (sum <$> many (natRE <|> 1 <$ msym (readMaybe . pure :: Char -> Maybe Frame)))

tableRow :: [Char] -> Natural
tableRow (splitOn " " -> ts)
  | "Land" ∈ ts = 0
  | "Creature" ∈ ts = 2
  | "Sorcery" ∈ ts = 3
  | otherwise = 1

setName :: [Char]
setName = "S0"

(∈) :: (Eq a, Foldable f) => a -> f a -> Bool
(∈) = elem
