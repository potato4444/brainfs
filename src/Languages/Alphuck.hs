
module Languages.Alphuck ( alphuckDesc ) where

{-!
  name: Alphuck
  argName: alphuck
  desc: alphuckDesc
!-}

import Core
import Languages.Brainfuck
import Types
import Util


alphuckDesc = bfDesc
  { run = core (alph ?> bf) [] ""
  , bf2x = Just (bf ?> alph)
  , x2bf = Just (alph ?> bf)
  }

alph = "eoijcaps"
bf   = "+,-.<>[]"
