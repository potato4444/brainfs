{-# LANGUAGE FlexibleContexts
      , PartialTypeSignatures
      , LambdaCase
      , Rank2Types
      , ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Core where

import Data.Foldable
import Control.Applicative
import Control.Monad.IO.Class
import System.IO

import qualified Control.Exception as E
import qualified Data.Char         as C

import Types


satisfy = satisfy' fwd

satisfy' :: (Zip t -> Maybe (Zip t))->(t -> Bool) -> String -> BF t _ t
satisfy' dir p m =
  ifM id exit (fail "unexpected eof") $
   ifM p (src . cell) go (fail m)

  where go = do tok <- use (src . cell)
                dir <$> use src >>= \case
                  Nothing -> exit .= True
                  Just st -> src  .= st
                pure tok

tok c = satisfy (==c) ("tok " ++ show c)

noneOf cs = satisfy (`notElem`cs) ("noneOf '" ++ cs ++ "'")

oneOf cs = satisfy (`elem`cs) ("oneOf '" ++ cs ++ "'")

anyTok = satisfy (const True) "anyTok"

io :: MonadIO m => IO a -> m a
io = liftIO

dbg x = io (print x) >> pure x

chr :: Integer -> Char
chr = C.chr . (`mod`128) . fromIntegral

ord :: Char -> Integer
ord = fromIntegral . C.ord

getChr = use input >>= safeIO Nothing . fmap Just . hGetChar

getStr = use input >>= safeIO "" . hGetLine

safeIO :: a -> IO a -> BF _ _ a
safeIO def action = io (E.try action) >>= \case
  Left e  -> io (hPrint stderr (e :: E.SomeException)) >> pure def
  Right c -> pure c

tape :: (forall tape val . (Tape tape, Val val) => tape val -> tape val)
  -> BF _ _ ()
tape f = do
  (Env f0 f1 f2 f3 f4 t f5 f6) <- get
  put (Env f0 f1 f2 f3 f4 (f t) f5 f6)

tapeM :: (forall tape val . (Tape tape, Val val) => tape val -> BF _ _ (tape val))
  -> BF _ _ ()
tapeM f = do
  (Env f0 f1 f2 f3 f4 t f5 f6) <- get
  t' <- f t
  put (Env f0 f1 f2 f3 f4 t' f5 f6)

move d f t = case f t of
  Just t' -> pure t'
  Nothing -> do
    io . hPutStrLn stderr $ "can't move " ++ d
    exit .= True
    pure t

core :: Show ctx
  => (String -> String) -- ^ modify the source
  -> [BF Char ctx ()] -- ^ additional statements
  -> String  -- ^ ignore these characters
  -> Runner () ctx
core t bs = core' (==zero) (/=zero) t (const <$> bs)

core' :: Show ctx
  => (forall val . (Val val) => val -> Bool) -- ^ open predicate (take jump if true)
  -> (forall val . (Val val) => val -> Bool) -- ^ close predicate (take jump if true)
  -> (String -> String) -- ^ modify the source
  -> [BF Char ctx () -> BF Char ctx ()] -- ^ additional statements
  -> String  -- ^ ignore these characters
  -> Runner () ctx
core' op cl t bs es = run where
  run _ "" _ _ _ _ _ _ _ = pure ()
  run _ src ctx eof (CTyp (_ :: Proxy v)) tt dbg vbs h =
    execStateT (runBF $ many stmt) (mkEnv dbg vbs h (t src) tt (zero :: v) eof ctx)
      >>= hPrint stderr
  stmt = do
    whenM id verbose $
      satisfy' Just (const True) "peek" >>= io . putStr . (++":  ") . show
    asum (($ stmt) <$> bs) <|> loop <|> atom <|> () <$ anyTok
    whenM id verbose $
      get >>= io . print

  loop = satisfy' Just (`elem` "[]") "expected '[' or ']' " >>= \case
    '[' -> withCell $ \c-> if op c -- c == zero
             then jumpFwd <$> use src >>= \case
               Just s' -> src .= s'
               Nothing -> do
                 io (hPutStrLn stderr "no matching ']'")
                 exit .= True
             else
               () <$ tok '['
    ']' -> withCell $ \c -> if cl c -- c /= zero
             then jumpBwd <$> use src >>= \case
               Just s' -> src .= s'
               Nothing -> do
                 io (hPutStrLn stderr "no matching '['")
                 exit .= True
             else
               () <$ tok ']'
    _ -> fail "notgonnahappen"

  atom = noneOf ('[':']':es) >>= \case
    '<' -> tapeM $ move "left" bwd
    '>' -> tapeM $ move "right" fwd
    '+' -> tape $ cell %~ inc
    '-' -> tape $ cell %~ dec
    ',' -> getChr >>= \case
             Just c -> tape $ cell .~ fi (ord c)
             Nothing -> use eof >>= \case
               Nop -> pure ()
               Set n -> tape $ cell .~ fi n
    '.' -> do
      Env _ _ _ _ _ t _ _ <- get
      io . putChar . chr . fv $ t ^. cell
    '?' -> whenM id debug $ get >>= io . hPrint stderr
    _ -> pure ()

withCell :: (forall val . (Val val) => val -> BF _ _ a) -> BF _ _ a
withCell f = do
  (Env _ _ _ _ _ t _ _) <- get
  f (t ^. cell)

jumpBwd, jumpFwd :: Tape tape => tape Char -> Maybe (tape Char)
jumpBwd = (fwd =<<) . balancedMatch bwd '[' ']'
jumpFwd = (fwd =<<) . balancedMatch fwd ']' '['

balancedMatch :: (Eq tok, Tape tape)
  => (tape tok -> Maybe (tape tok)) -> tok -> tok -> tape tok -> Maybe (tape tok)
balancedMatch mv a b t = go 0 =<< mv t where
  go n t | n == 0, t ^. cell == a = Just t
         | t ^. cell == a = go (n-1) =<< mv t
         | t ^. cell == b = go (n+1) =<< mv t
         | otherwise = go n =<< mv t


ifM pred lens mIf mElse = use lens >>= \case
  v | pred v    -> mIf
    | otherwise -> mElse

whenM pred lens ma = ifM pred lens (() <$ ma) (pure ())
