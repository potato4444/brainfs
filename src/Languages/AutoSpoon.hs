
module Languages.AutoSpoon ( autospoonDesc ) where

{-!
  name: AutoSpoon
  argName: autospoon
  desc: autospoonDesc
!-}

import Data.Char
import Data.List
import System.Console.GetOpt

import Languages.Spoon
import Types


autospoonDesc = spoonDesc
  { run = run'
  , options = [ Option "s" ["spaces"] (NoArg (const True)) "consider spaces" ]
  , dflts = False
  , bf2x = Nothing
  }

run' spaces src = run spoonDesc (z,o) src where
  sorted = map head . sortOn (negate . length) . group . sort
         $ (if spaces then id else filter (not . isSpace)) src
  z | null sorted = '0'
    | otherwise   = head sorted
  o | (_:c:_) <- sorted = c
    | otherwise         = succ z

Just bf2spoon = bf2x spoonDesc

bf2autospoon src = go where
  spoon = bf2spoon src

  (z,o) = foldr count (0,0) spoon

  count c (z,o) = case c of
    '0'-> (z+1,o)
    '1'-> (z,o+1)
    _-> (z,o)

  go | null spoon = ""
     | z > o      = spoon
     | otherwise  = spoon ++ concat (replicate (div(o-z+1)2) "1000")
