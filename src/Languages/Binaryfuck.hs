
module Languages.Binaryfuck ( binaryfuckDesc ) where

{-!
  name: Binaryfuck
  argName: binaryfuck
  desc: binaryfuckDesc
!-}

import Data.Char
import Data.List
import Data.Maybe
import Numeric
import qualified Data.Map as M

import Core
import Languages.Brainfuck
import Types


binaryfuckDesc = bfDesc
  { run = core bin2bf [] ""
  , bf2x = Just bf2bin
  , x2bf = Just bin2bf
  }


alph = ['0'..'9']++['a'..]

bin2bf = go
       . drop 1
       . dropWhile (=='0')
       . concat
       . mapMaybe ((`M.lookup` b32) . toLower) where

  b32 = M.fromList . zip alph $ mapM (pure "01") [1..5]

  go ('0':'0':'0':cs) = '+' : go cs
  go ('0':'0':'1':cs) = '-' : go cs
  go ('0':'1':'0':cs) = '>' : go cs
  go ('0':'1':'1':cs) = '<' : go cs
  go ('1':'0':'0':cs) = '.' : go cs
  go ('1':'0':'1':cs) = ',' : go cs
  go ('1':'1':'0':cs) = '[' : go cs
  go ('1':'1':'1' :cs) = ']' : go cs
  go (_:cs) = go cs
  go _ = []


bf2bin :: String->String
bf2bin = (\x-> showIntAtBase 32 (alph!!) x "")
       . fst
       . head
       . readInt 2 (`elem`"01") (idx "01")
       . ('1':)
       . concatMap c2bin where
  c2bin c = case c of
    '+' -> "000"
    '-' -> "001"
    '>' -> "010"
    '<' -> "011"
    '.' -> "100"
    ',' -> "101"
    '[' -> "110"
    ']' -> "111"
    _ -> ""

  fromBase b alph = fst . head . readInt b (`elem` alph) (idx alph)
  idx cs c = let Just x = elemIndex c cs in x
