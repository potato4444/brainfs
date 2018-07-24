
module Languages.Bitfuck ( bitfuckDesc ) where

{-!
  name: Bitfuck
  argName: bitfuck
  desc: bitfuckDesc
!-}

import Languages.Boolfuck
import Types

bitfuckDesc = boolfuckDesc
  { run = run' , bf2x = Just (bool2bit . bf2bool) }

run' f src = run boolfuckDesc f (bit2bool src)

Just bf2bool = bf2x boolfuckDesc

bit2bool = go . filter (`elem` "*<>!") where
  go ('*':cs) = '+' : go cs
  go ('<':cs) = '<' : go cs
  go ('>':cs) = '>' : go cs
  go ('!':'!':cs) = ';' : go cs
  go ('!':'*':cs) = ',' : go cs
  go ('!':'<':cs) = '[' : go cs
  go ('!':'>':cs) = ']' : go cs
  go _ = []

bool2bit = concatMap c2str . filter (`elem` "+<>;,[]") where
  c2str c = case c of
    '+' -> "*"
    '<' -> "<"
    '>' -> ">"
    ';' -> "!!"
    ',' -> "!*"
    '[' -> "!<"
    ']' -> "!>"
    _ -> ""
