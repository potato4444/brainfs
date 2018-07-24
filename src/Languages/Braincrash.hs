
module Languages.Braincrash ( braincrashDesc ) where


import Languages.Brainfuck
import Types

{-!
  name: Braincrash
  argName: braincrash
  desc: braincrashDesc
!-}


import Core

braincrashDesc = bfDesc
  { run = core toBF [] ""
  , bf2x = Just fromBF
  , x2bf = Just toBF
  }

alph = concat (repeat "+-><.,[]")


toBF = concat . zipWith pick alph . filter (`elem` " !") where
  pick c '!' = [c]
  pick _ _ = []


fromBF = go alph where
  go (a:as) (b:bs) | a == b    = '!' : go as bs
                   | otherwise = ' ' : go as (b:bs)
  go _ _ = []
