
module Util where

import Data.List
import Data.Maybe
import Numeric

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C


bin8 x = pad 8 '0' $ showIntAtBase 2 ("01"!!) x ""

pad n x xs | l <- length xs = replicate (n-l) x ++ xs

bytes = concatMap bin8 . B.unpack . C.pack

x ?> y = mapMaybe (\c-> (y!!) <$> elemIndex c x)

split c = go "" where
  go xs (x:cs) | x == c    = xs : go [] cs
               | otherwise = go (xs++[x]) cs
  go xs _ = [xs]
