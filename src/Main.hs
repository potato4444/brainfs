{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module Main where

import System.IO
import System.Environment

import Args
import Languages
import MainTH
import Types


main :: IO ()
main = getArgs >>= $pickDesc

runDesc an nm Desc{..} as = case getOpt Permute opts as of
  (args,rest,[]) ->
    case foldr ($) defaults args of
      Opts _ True _ _ _ _ _ _ _ _ -> putStrLn usage
      opts -> case rest of
        s:_ -> do
          src <- if opts ^. expr then pure s else readFile s
          case opts ^. mode of
            Trans trans -> putStrLn (trans src)
            Run ->
              with "can't decode cell type" (opts ^. cellType) $ \ct ->
                with "can't decode tape type" (opts ^. tapeType) $ \tt ->
                  with "can't decode eof action" (opts ^. eofOpt) $ \eo ->
                    inputHandle (opts ^. inputFile) >>=
                      run (opts ^. flags) src initCtx eo ct tt (opts ^. dbg) (opts ^. vbs)
        _ -> die $ "please provide " ++ if opts ^. expr then "expression" else "file"
  (_,_,err) -> die $ concat err

  where
    opts = mkOptions an nm cellOpts tapeOpts eofOpts bf2x x2bf options
    defaults = Opts False False False False Nothing Run (Just cTyp) (Just tTyp) (Just eofAction) dflts
    inputHandle (Just "-") = pure stdin
    inputHandle (Just fname) = openFile fname ReadMode
    inputHandle Nothing = pure stdin
    usage = usageInfo (usage' an) opts
    die = die' usage
    with s m f = maybe (die s) f m


die' u m = ioError . userError $ concat [m," ",u]

usage' l = "\n usage: brainfs " ++ l ++ " [-h] (-e expr | file) [OPTIONS]\n"
