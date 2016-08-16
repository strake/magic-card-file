module Data.Sgml where

import Data.Foldable

data Sgml = Tag [Char] [([Char], [Char])] [Sgml] | CData [Char]

toChars :: Sgml -> [Char]
toChars (Tag tag attrs children) =
    (fold . concat) [["<", tag], (\ (k, v) -> " " ++ k ++ "=\"" ++ v ++ "\"") <$> attrs,
                     ">" : fmap toChars children ++ ["</", tag, ">"]]
toChars (CData x) = x
