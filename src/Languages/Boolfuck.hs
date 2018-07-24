{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Languages.Boolfuck ( boolfuckDesc ) where

{-!
  name: Boolfuck
  argName: boolfuck
  desc: boolfuckDesc
!-}

import Lens.Micro.TH

import Core
import Types
import Util

data Ctx = Ctx { _inStream  :: [Bool], _outStream :: [Bool] }

instance Show Ctx where
  show (Ctx os is) = "{ in: " ++ show' os ++ ", out: " ++ show' is ++ " }"
    where show' bs = show (fromEnum <$> bs)

makeLenses ''Ctx


boolfuckDesc = Desc
  { run = core id [ignore, inStmt, outStmt] "-,.;"
  , initCtx = Ctx [] []
  , cellOpts = False
  , tapeOpts = True
  , eofOpts = False
  , cTyp = bit
  , tTyp = ITape
  , eofAction = Set 0
  , options = []
  , dflts = ()
  , bf2x = Just bf2bool
  , x2bf = Nothing
  }

ignore = () <$ oneOf "-."

inStmt = tok ',' >> go where
  go = use (ctx . inStream) >>= \case
         (b:bs) -> do
           tape $ cell .~ fi (fromIntegral $ fromEnum b)
           ctx . inStream .= bs
         _ -> getChr >>= \case
           Just c  -> ctx . inStream .= bin (ord c) >> go
           Nothing -> use eof >>= \case
             Nop -> pure ()
             Set n -> ctx . inStream .= bin n >> go

  bin = reverse . map (=='1') . bin8

outStmt = do
  tok ';'
  withCell $ \c-> do
    ctx . outStream %= (fi (fv c):)

    use (ctx . outStream) >>= \case
      bs | 8 > length bs -> pure ()
         | otherwise -> do
            let (w,rs) = splitAt 8 bs
            ctx . outStream .= rs
            io . putChar . chr . foldl ((+).(2*)) 0 $ map fv w


bf2bool :: String -> String
bf2bool = concatMap c2cmds where
  c2cmds c = case c of
    '+' -> ">[>]+<[+<]>>>>>>>>>[+]<<<<<<<<<"
    '-' -> ">>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+]<<<<<<<<<"
    '<' -> "<<<<<<<<<"
    '>' -> ">>>>>>>>>"
    ',' -> ">,>,>,>,>,>,>,>,<<<<<<<<"
    '.' -> ">;>;>;>;>;>;>;>;<<<<<<<<"
    '[' -> ">>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>[+<<<<<<<<[>]+<[+<]"
    ']' -> ">>>>>>>>>+<<<<<<<<+[>+]<[<]>>>>>>>>>]<[+<]"
    _ -> ""
