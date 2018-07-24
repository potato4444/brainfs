{-# LANGUAGE DeriveFunctor
      , DeriveTraversable
      , FlexibleContexts
      , LambdaCase
      , TemplateHaskell #-}

module MainTH where

import Data.Char
import Data.List
import Data.Maybe
import Language.Haskell.TH
import Lens.Micro
import Lens.Micro.TH
import System.Directory
import Text.Parsec


type Hint = Hint' String

data Hint' a = H { _name, _argName, _desc :: a }
  deriving (Show, Foldable, Functor, Traversable)

makeLenses ''Hint'


pickDesc = do
  hs <- runIO (collectHints "src/Languages")

  let langs  = listE $ map (\h-> tupE $ stringE . ($ h) <$> [_name,_argName]) hs
      watLang = match wildP (normalB [|const $(die langs "unknown language")|]) []
      matchLang = lamCaseE (map fromHint hs ++ [watLang])

  [| \case (l:as) -> $matchLang (toLower <$> l) as
           _      -> $(die langs "please provide a language")
   |]

fromHint :: Hint -> MatchQ
fromHint hints = do
  Just dsc <- fmap varE <$> lookupValueName (hints ^. desc)
  let nm = litE . stringL $ hints ^. name
      an = litE . stringL $ hints ^. argName
  match (litP . stringL $ hints ^. argName) (normalB [| runDesc $an $nm $dsc |]) []

die :: ExpQ -> String -> ExpQ
die ls msg = [| die' (concat [ usage' "LANG"
                             , "\n  LANG must be one of:"
                             , foldr (\(n,a)b->"\n    "++a++"\t-\t"++n++b) "" $ls
                             , "\n"
                             ]) msg |]

collectHints :: FilePath -> IO [Hint]
collectHints p = fmap catMaybes . mapM go . sort =<< listDirectory p where
  go f = parseFile f <$> readFile (p ++ '/' : f) >>= \case
    Left e  -> putStrLn e >> pure Nothing
    Right h -> pure (Just h)

parseFile :: FilePath -> String -> Either String Hint
parseFile f = either (Left . show)
                     (maybe (Left $ f ++ ": fields missing") Right . sequence)
            . parse hintP f
            . go
  where
    go ('{':'-':'!':cs) = pick (dropWhile (`elem`"\n ") cs)
    go (_:cs) = go cs
    go e = e

    pick ('!':'-':'}':_) = []
    pick (c:cs) = c : pick cs
    pick e = e

    hintP = foldr ($) (H n n n) <$> many (keyP name "name" <|>
                                          keyP argName "argName" <|>
                                          keyP desc "desc")
    n = Nothing

    keyP l k = (l ?~) <$> (string k *> spaces *> char ':' *> spaces *>
                           many (noneOf "\n") <* spaces)
