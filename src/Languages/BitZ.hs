
module Languages.BitZ ( bitzDesc ) where

{-!
  name: BitZ
  argName: bitz
  desc: bitzDesc
!-}

import Data.List
import Data.Maybe
import System.Console.GetOpt

import Core
import Languages.Brainfuck
import Types
import Util


bitzDesc = bfDesc
  { run = run'
  , options = [ Option "b" ["bytes"] (NoArg (const True)) "read source as bytestring" ]
  , dflts = False
  , bf2x = Just fromBF
  , x2bf = Just toBF
  }

run' bs = core (toBF . t) [] "" () where
  t = if bs then bytes else id

alph = "><+-.,[]"

toBF = map (alph!!)
     . init
     . tail
     . foldr (\a (b:bs) -> if a=='0' then mod(b+1)8:bs else 0:b:bs) [0]
     . dropWhileEnd (=='0')
     . dropWhile (=='0')
     . filter (`elem` "01")

fromBF = foldr (\a b->'1' : replicate a '0'++b) "1"
       . mapMaybe (`elemIndex` alph)
