
module Languages.Brainfuck ( bfDesc ) where

{-!
  name: brainfuck
  argName: brainfuck
  desc: bfDesc
!-}

import Core
import Types

bfDesc = Desc
  { run = core (++" ") [] ""
  , initCtx = ()
  , cellOpts = True
  , tapeOpts = True
  , eofOpts = True
  , cTyp = u8
  , tTyp = CTape 30000
  , eofAction = Set 0
  , options = []
  , dflts = ()
  , bf2x = Nothing
  , x2bf = Nothing
  }
