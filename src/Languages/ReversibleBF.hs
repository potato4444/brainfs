{-# LANGUAGE LambdaCase #-}

module Languages.ReversibleBF ( reversibleDesc ) where

{-!
  name: Reversible brainfuck
  argName: reversible
  desc: reversibleDesc
!-}

import Core
import Languages.Brainfuck
import Types

reversibleDesc = bfDesc
  { run = core' (/=zero) (/=zero) (++" ") [inp] ","
  , tTyp = RTape Nothing
  , eofAction = Set 0
  }

inp _ = do
  tok ','
  withCell $ \c->
    if c == zero then
      getChr >>= \case
        Just c -> tape $ cell .~ fi (ord c)
        Nothing -> use eof >>= \case
          Nop -> pure ()
          Set n -> tape $ cell .~ fi n
    else
      exit .= True
