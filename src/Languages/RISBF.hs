{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Languages.RISBF ( risbfDesc ) where

{-!
  name: RISBF
  argName: risbf
  desc: risbfDesc
!-}

import Core
import Languages.Brainfuck
import Types

risbfDesc = bfDesc
  { run = core trans [star,add,sub] "*+-"
  , initCtx = Mem
  , tTyp = RTape Nothing
  , eofAction = Set 0
  }

data Sel = Mem | Ptr deriving (Eq,Show)

next Mem = Ptr
next Ptr = Mem

star = tok '*' >> ctx %= next
add  = tok '+' >> tape (cell %~ inc) // tapeM (move "right" fwd)
sub  = tok '-' >> tape (cell %~ dec) // tapeM (move "left" bwd)

mem // ptr = use ctx >>= \case { Mem -> mem; Ptr -> ptr }


trans = go . filter (`elem`"?*+-/") where
  go ('*':cs) = '*' : go cs
  go ('+':cs) = '+' : go cs
  go ('-':cs) = '-' : go cs
  go ('/':'/':cs) = '.' : go cs
  go ('/':'*':cs) = ',' : go cs
  go ('/':'+':cs) = '[' : go cs
  go ('/':'-':cs) = ']' : go cs
  go (c:cs) = c : go cs
  go e = e

bf2risbf = go Mem where
  go m (c:cs) = case c of
    '+' -> decide m "+" "*+" Mem cs
    '>' -> decide m "*+" "+" Ptr cs
    '-' -> decide m "-" "*-" Mem cs
    '<' -> decide m "*-" "-" Ptr cs
    '.' -> "//" ++ go m cs
    ',' -> "/*" ++ go m cs
    '[' -> decide m "*/+" "/+" Ptr cs
    ']' -> decide m "*/-" "/-" Ptr cs
    _   -> go m cs
  go _ "" = ""


  decide m mem ptr m' cs
    | m == Mem  = mem ++ go m' cs
    | otherwise = ptr ++ go m' cs
