{-# LANGUAGE LambdaCase #-}

module Languages.Brainhype ( brainhypeDesc ) where

{-!
  name: Brainhype
  argName: brainhype
  desc: brainhypeDesc
!-}

import Control.Applicative
import System.IO

import Core
import Languages.Brainfuck
import Types


brainhypeDesc = bfDesc
  { run = core' (==zero) (/=zero) id [hype] "{"
  , bf2x = Just id
  , x2bf = Just (errorWithoutStackTrace "one does not simply solve the halting problem")
  }

hype stmt = do
  satisfy' Just (=='{') "expected '}'"
  Zip _ _ cs _ <- use src
  let sub = go 0 cs
  unless (null sub) $ do
    get >>= io . execStateT (runBF $ many stmt) . (src .~ listTape sub)
    balancedMatch fwd '}' '{' <$> use src >>= \case
      Nothing -> io (hPutStrLn stderr "no matching '}'") >> exit .= True
      Just s' -> src .= s'
  tape $ cell .~ zero


  where
    go n (c:cs) | n == 0, c == '}' = []
                | c == '}'  = c : go (n-1) cs
                | c == '{'  = c : go (n+1) cs
                | otherwise = c : go n cs
    go _ _ = []
