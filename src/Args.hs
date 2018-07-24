{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction, TemplateHaskell #-}

module Args
  ( module System.Console.GetOpt
  , mkOptions, Mode(..), Opts(..)
  , expr, help, dbg, vbs, inputFile, mode, cellType, tapeType, eofOpt, flags
  ) where

import Data.Char
import Data.Maybe
import Lens.Micro.TH
import System.Console.GetOpt

import Types


data Mode = Run | Trans (String -> String)

data Opts val flags = Opts
  { _expr, _help
  , _dbg, _vbs   :: Bool
  , _inputFile   :: Maybe String
  , _mode        :: Mode
  , _cellType    :: Maybe CTyp
  , _tapeType    :: Maybe TTyp
  , _eofOpt      :: Maybe Eof
  , _flags       :: flags
  }

makeLenses ''Opts


mkOptions ::
     String -- ^ argument name
  -> String -- ^ name
  -> Bool -- ^ add cell-type options
  -> Bool -- ^ add tape-type options
  -> Bool -- ^ add eof option
  -> Maybe (String -> String) -- ^ BF -> x transpiler
  -> Maybe (String -> String) -- ^ x -> BF transpiler
  -> Options flags -- ^ options for additional flags
  -> Options (Opts val flags)
mkOptions an nm co to eo bx xb aos =
  dfltOptions ++ catMaybes trns ++ cts ++ map (fmap (flags%~)) aos ++ helpOption

  where cts | to        = [typeOption|co] ++ tapeOptions ++ [eofOption|eo]
            | otherwise = [typeOption|co] ++ [eofOption|eo]

        trns = zipWith3 opt ["transpile brainfuck to" ++ nm
                            , "transpile " ++ nm ++ " to brainfuck"
                            ] ["bf2" ++ [head an],head an : "2bf"] [bx,xb]

        opt d f t' = opt' d f <$> t'
        opt' d f t = Option "" [f] (NoArg (mode .~ Trans t)) d


helpOption = [ Option "h" ["help"] (NoArg (help .~ True)) "print this help" ]

dfltOptions =
  [ Option "e" ["expr"] (NoArg (expr .~ True)) "supply source via command"
  , Option "f" ["file"] (ReqArg (inputFile ?~) "file") "read inputs from file"
  , Option "d" ["debug"] (NoArg (dbg .~ True)) "use ? to dump the state"
  , Option "v" ["verbose"] (NoArg (vbs .~ True)) "dump state at every tick"
  ]

tapeOptions =
  [ Option "l" ["left"] (OptArg (to LTape) "N") "tape in left direction"
  , Option "r" ["right"] (OptArg (to RTape) "N") "tape in right direction"
  , Option "c" ["circular"] (ReqArg ((tapeType .~) . fmap CTape . readNum) "N") "finite circular tape"
  , Option "i" ["infinite"] (NoArg (tapeType ?~ ITape)) "infinite tape in both directions"
  ] where to c = (tapeType .~) . fmap c . mapM readNum

typeOption =
  Option "t" ["type"] (ReqArg ((cellType .~) . toProxy) "TYPE") "set integer type"

eofOption =
  Option "" ["eof"] (ReqArg ((eofOpt .~) . readEof) "(N|nop)") "what to do on eof (set N or nop)"


readNum s | all isDigit s = Just (read s)
          | otherwise = Nothing

toProxy str = case str of
  "u8"  -> Just u8
  "s8"  -> Just s8
  "u16" -> Just u16
  "s16" -> Just s16
  "u32" -> Just u32
  "s32" -> Just s32
  "u64" -> Just u64
  "s64" -> Just s64
  "b"   -> Just bit
  "u"   -> Just inf
  _ -> Nothing

readEof s | map toLower s == "nop" = Just Nop
          | '-':cs <- s = Set . negate <$> readNum cs
          | otherwise = Set <$> readNum s
