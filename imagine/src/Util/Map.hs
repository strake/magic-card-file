module Util.Map where

import Data.Map as Map

{-# INLINE traverseEither #-}
traverseEither :: Applicative p => (a -> p (Either b c)) -> Map k a -> p (Map k b, Map k c)
traverseEither f = fmap (Map.mapEither id) . traverse f
