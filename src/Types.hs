{-# LANGUAGE DeriveFunctor
      , ExistentialQuantification
      , FlexibleContexts
      , FlexibleInstances
      , GeneralizedNewtypeDeriving
      , LambdaCase
      , NoMonomorphismRestriction
      , Rank2Types
      , UndecidableInstances
      , TemplateHaskell #-}

module Types
  ( module Lens.Micro, module Lens.Micro.Mtl
  , unless, when, get, modify, put, execStateT, (<|>)
  , module Types
  , stderr, Proxy(..)
  ) where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Array
import Data.Proxy
import Data.Word
import Data.Int
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import System.Console.GetOpt
import System.IO


type Options a = [OptDescr (a -> a)]
type Runner flags ctx =
     flags
  -> String -- ^ source
  -> ctx -- ^ additional flags
  -> Eof -- ^ what to do on eof
  -> CTyp -- ^ cell type
  -> TTyp -- ^ tape type
  -> Bool -- ^ debug
  -> Bool -- ^ verbose
  -> Handle -- ^ input handle
  -> IO ()

data Desc ctx flags = Desc
  { run       :: Runner flags ctx
  , initCtx   :: ctx
  , cellOpts
  , tapeOpts
  , eofOpts   :: Bool
  , cTyp      :: CTyp
  , tTyp      :: TTyp
  , eofAction :: Eof
  , options   :: Options flags
  , dflts     :: flags
  , bf2x
  , x2bf      :: Maybe (String -> String)
  }

data Eof = Nop | Set Integer
data TTyp = CTape Int | LTape (Maybe Int) | RTape (Maybe Int) | ITape
data CTyp = forall val . (Val val) => CTyp (Proxy val)


class (Eq val) => Val val where
  pretty :: val -> String
  fi     :: Integer -> val
  fv     :: val -> Integer
  zero   :: val
  inc    :: val -> val
  dec    :: val -> val

instance {-#OVERLAPS#-} (Val val)=> Show val where show = pretty

instance {-# OVERLAPS #-} Val Bool where
  pretty False = "0"
  pretty True  = "1"
  fi = toEnum . fromIntegral . (`mod`2)
  fv = fromIntegral . fromEnum
  zero = False
  inc = not
  dec = not

instance (Eq val, Integral val, Num val, Show val) => Val val where
  pretty = show
  fi = fromIntegral
  fv = fromIntegral
  zero = 0
  inc = (+1)
  dec = subtract 1

u8, s8, u16, s16, u32, s32, u64, s64, bit :: CTyp
u8  = CTyp (Proxy :: Proxy Word8)
s8  = CTyp (Proxy :: Proxy Int8)
u16 = CTyp (Proxy :: Proxy Word16)
s16 = CTyp (Proxy :: Proxy Int16)
u32 = CTyp (Proxy :: Proxy Word32)
s32 = CTyp (Proxy :: Proxy Int32)
u64 = CTyp (Proxy :: Proxy Word64)
s64 = CTyp (Proxy :: Proxy Int64)
bit = CTyp (Proxy :: Proxy Bool)
inf = CTyp (Proxy :: Proxy Integer)


class Tape tape where
  cell :: Lens' (tape val) val
  bwd  :: tape val -> Maybe (tape val)
  fwd  :: tape val -> Maybe (tape val)
  goto :: Integer -> tape val -> Maybe (tape val)


data Zip val = Zip [val] val [val] Integer
  deriving Functor

{-
instance {-#OVERLAPS#-} Show (Zip Char) where
  show (Zip xs y zs i) = concat [show i, ": ", show ls, " ", show y, " ", show rs]
    where ls = reverse xs
          rs = zs
listTP i str = Zip (reverse $ take (i) str) (str!!i) (drop(i+1)str) (fromIntegral i)
-}

instance Val v => Show (Zip v) where
  show (Zip xs y zs i) = concat [show i, ": ", show ls, " ", show y, " ", show rs]
    where ls = reverse $ take 5 xs
          rs = take 5 zs

instance Tape Zip where
  cell = lens getter setter where
    getter (Zip _ v _ _) = v
    setter (Zip x _ y i) u = Zip x u y i
  bwd (Zip (x:xs) v y i) = Just $ Zip xs x (v:y) (i-1)
  bwd _ = Nothing
  fwd (Zip x v (y:ys) i) = Just $ Zip (v:x) y ys (i+1)
  fwd _ = Nothing
  goto n z@(Zip _ _ _ i)
    | i > fromIntegral n = goto n =<< bwd z
    | i < fromIntegral n = goto n =<< fwd z
    | otherwise = Just z


data Circular val = Circular (Array Int val) Int
  deriving Functor

instance Val val => Show (Circular val) where
  show (Circular a i) = concat
    [show i, ": ", show $ slice(-4)(-3)(-1), " ", show (a!i), " ", show $ slice 1 2 5]
    where slice x y z = [a!j| j' <- [x,y..z], let j = (i+j') `mod` sz]
          sz = snd (bounds a)

instance Tape Circular where
  cell = lens getter setter where
    getter (Circular a i) = a ! i
    setter (Circular a i) v = Circular (a//[(i,v)]) i
  bwd (Circular a i) = Just . Circular a $ mod (i-1) sz
    where sz = snd $ bounds a
  fwd (Circular a i) = Just . Circular a $ mod (i+1) sz
    where sz = snd $ bounds a
  goto n (Circular a _) = Just $ Circular a (fromIntegral n`mod`sz)
    where sz = snd $ bounds a


data Env tok ctx = forall tape val
  . (Functor tape, Show (tape val), Tape tape, Val val) => Env
  { _exit
  , _debug
  , _verbose :: Bool
  , _input   :: Handle
  , _src     :: Zip tok
  , __tape__ :: tape val
  , _eof     :: Eof
  , _ctx     :: ctx
  }

instance Show ctx => Show (Env tok ctx) where
  show (Env _ _ _ _ _ t _ c) = concat ["{ ", show t, ", ", show c, " }"]

mkEnv dbg vbs h src (CTape n) val eof ctx =
  Env False dbg vbs h (listTape src) (Circular (listArray (0,n-1) $ repeat val) 0) eof ctx
mkEnv dbg vbs h src ttyp val eof ctx = Env False dbg vbs h (listTape src) tape eof ctx where
  tape = case ttyp of
      (LTape (Just n)) -> Zip (nvs n) val [] 0
      (LTape Nothing)  -> Zip  vs val [] 0
      (RTape (Just n)) -> Zip [] val (nvs n) 0
      (RTape Nothing)  -> Zip [] val  vs 0
      ITape     -> Zip vs val vs 0
      _ -> error "won't happen"

  nvs n = replicate n val
  vs = repeat val

listTape (x:xs) = Zip [] x xs 0
listTape [] = error "empty src"

makeLenses ''Env

newtype BF tok ctx a = BH { runBF :: StateT (Env tok ctx) IO a }
  deriving (Functor, Applicative, Alternative, Monad, MonadState (Env tok ctx), MonadIO)
