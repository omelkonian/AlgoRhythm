module Grammar.RandomUtils where

import System.Random

oneOf :: [a] -> IO a
oneOf = choose . fmap (\a -> (1, a))

choose :: [(Double, a)] -> IO a
choose items = do
  let totalWeight = sum $ fst <$> items
  index <- getStdRandom $ randomR (0, totalWeight)
  return $ pick index items

pick :: Double -> [(Double, a)] -> a
pick n ((w, a):es) =
  if n <= w then
    a
  else
    pick (n-w) es
pick _ _ = error "pick: empty list"
