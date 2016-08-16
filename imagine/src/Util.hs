module Util where

import Control.Arrow
import Data.Either
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map

bisections :: [a] -> [([a], [a])]
bisections xs = flip List.splitAt xs <$> [0..length xs]

groupEithers :: [Either a b] -> [Either [a] [b]]
groupEithers = fmap (\ xs@(x:_) -> either (pure (Left . lefts)) (pure (Right . rights)) x xs) .
               List.groupBy ((==) `on` either (pure False) (pure True))

spanFoldl :: (b -> Bool) -> (b -> a -> b) -> b -> [a] -> ([a], [a])
spanFoldl _ _ _  [] = ([], [])
spanFoldl p f z0 (x:xs) | False <- p z0 && p (f z0 x) = ([], x:xs)
                        | otherwise = (x:) *** id $ spanFoldl p f (f z0 x) xs

ordNubOn :: Ord k => (a -> k) -> [a] -> [a]
ordNubOn f = fmap (f &&& id) & Map.fromList & Map.elems

{-# INLINE (&) #-}
(&) :: (a -> b) -> (b -> c) -> a -> c
(f & g) x = g (f x)
