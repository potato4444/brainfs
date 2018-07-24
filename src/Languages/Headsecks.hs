
module Languages.Headsecks ( headsecksDesc ) where

{-!
  name: Headsecks
  argName: headsecks
  desc: headsecksDesc
!-}

import Data.Char as C

import Core
import Languages.Brainfuck
import Types
import Util


headsecksDesc = bfDesc
  { run = core toBF [] ""
  , bf2x = Just (alph ?> "01234567")
  , x2bf = Just toBF
  }

alph = "+-<>.,[]"

toBF = map (\c-> alph !! mod (C.ord c) 8)
