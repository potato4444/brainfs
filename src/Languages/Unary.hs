
module Languages.Unary ( unaryDesc ) where

import Numeric

import Core
import Languages.Brainfuck
import Types
import Util

{-!
  name: Unary
  argName: unary
  desc: unaryDesc
!-}


unaryDesc = bfDesc
  { run = core u2bf [] ""
  , bf2x = Just bf2u
  , x2bf = Just u2bf
  }


bf2u = zeroes
     . fromBase 2 "01"
     . concat
     . ("1":)
     . (bf ?> unary) where

  zeroes n = '0' <$ [1..n]


u2bf = (unary ?> bf)
     . filter ((3==) . length)
     . chunksOf 3
     . drop 1
     . bin
     . len

(unary,bf) = unzip
  [ ("000", '>')
  , ("001", '<')
  , ("010", '+')
  , ("011", '-')
  , ("100", '.')
  , ("101", ',')
  , ("110", '[')
  , ("111", ']')
  ]

bin :: Integer -> String
bin n = showIntAtBase 2 ("01"!!) n ""

len (_:xs) = 1+len xs
len _ = 0
