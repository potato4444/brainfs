
module Languages.Spoon ( spoonDesc ) where

{-!
  name: Spoon
  argName: spoon
  desc: spoonDesc
!-}

import System.Console.GetOpt
import System.IO

import Core
import Languages.Brainfuck
import Types


spoonDesc = bfDesc
  { run = run'
  , options =
    [ Option "0" ["zero"] (ReqArg ((_1.~).head) "T") "set token for 0"
    , Option "1" ["one"] (ReqArg ((_2.~).head) "T") "set token for 1"
    ]
  , dflts = ('0','1')
  , bf2x = Just fromBF
  }

run' (z,o) = core trans [dbg,quit] "x?" () where

  quit = tok 'x' >> exit .= True
  dbg  = tok '?' >> get >>= io . hPrint stderr

  trans = go . map bin . filter (`elem` [z,o])

  bin c | c == z = 0
        | otherwise = 1

  go (1:cs) = '+' : go cs
  go (0:0:0:cs) = '-' : go cs
  go (0:1:0:cs) = '>' : go cs
  go (0:1:1:cs) = '<' : go cs
  go (0:0:1:1:cs) = ']' : go cs
  go (0:0:1:0:0:cs) = '[' : go cs
  go (0:0:1:0:1:0:cs) = '.' : go cs
  go (0:0:1:0:1:1:0:cs) = ',' : go cs
  go (0:0:1:0:1:1:1:0:cs) = '?' : go cs
  go (0:0:1:0:1:1:1:1:cs) = 'x' : go cs
  go _ = []


fromBF = concatMap c2str where
  c2str c = case c of
    '+' -> "1"
    '-' -> "000"
    '>' -> "010"
    '<' -> "011"
    ']' -> "0011"
    '[' -> "00100"
    '.' -> "001010"
    ',' -> "0010110"
    _ -> ""
