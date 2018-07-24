{-# LANGUAGE TemplateHaskell, TupleSections #-}

module Languages.Brainfunct ( brainfunctDesc ) where


import Languages.Brainfuck
import Types

{-!
  name: Brainfunct
  argName: brainfunct
  desc: brainfunctDesc
!-}

import Data.Array
import Lens.Micro.TH

import Core
import Util

data Ctx = Ctx { _functs :: Array Int String, _callStack :: [(Int,Zip Char)] }
instance Show Ctx where show (Ctx _ cs) = "callstack: " ++ show (fst <$> cs)
makeLenses ''Ctx

brainfunctDesc = bfDesc
  { run = run'
  , initCtx = error "overridden"
  , bf2x = Just id
  , x2bf = Nothing
  }

run' () src _ = core id [check,app] "@" () mainFct (Ctx fctArray [])
  where fs = (++" ") <$> split '/' src
        mainFct | null fs   = ""
                | otherwise = last fs
        fctArray = listArray (1,length fs-1) fs

check =
  ifM id exit (
    ifM (not.null) (ctx . callStack) (do
      exit .= False
      (_,f):fs <- use (ctx . callStack)
      src .= f
      ctx . callStack .= fs
    ) $ fail "callstack null"
  ) $ fail "exit"

app = do
  tok '@'
  withCell $ \c-> do
    fcts <- use (ctx . functs)
    let ix = fromIntegral (fv c)
    when (bounds fcts `inRange` ix) $
      case fcts ! ix of
        "" -> pure ()
        c:cs -> do
          use src >>= (ctx . callStack %=) . (:) . (ix,)
          src .= Zip [] c cs 0
