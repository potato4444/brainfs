
module Languages.ReverseFuck ( reverseDesc ) where

{-!
  name: ReverseFuck
  argName: reverse
  desc: reverseDesc
!-}

import Core
import Languages.Brainfuck
import Types
import Util


reverseDesc = bfDesc
  { run = core trans [] ""
  , bf2x = Just trans
  , x2bf = Just trans
  }

trans = rbf ?> bf

rbf  = "-+.,><]["
bf   = "+-,.<>[]"
