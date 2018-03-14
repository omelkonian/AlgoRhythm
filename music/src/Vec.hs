{-# language GADTs                #-}
{-# language DataKinds            #-}
{-# language TypeFamilies         #-}
{-# language TypeOperators        #-}
{-# language StandaloneDeriving   #-}
{-# language UndecidableInstances #-}
{-# language PolyKinds            #-}
{-# language ScopedTypeVariables  #-}

module Vec (
    Vec (..)
  , toList
  , map'
) where

import Data.TypeLevel.Num hiding ((-), (+), (*))

data Vec a n where
  Nil  :: Vec a D0
  (:.) :: a -> Vec a n -> Vec a (Succ n)
infixr 5 :.

-- deriving instance Eq a => Eq (Vec a n)

instance Show a => Show (Vec a n) where
  showsPrec d = showsPrec d . toList

toList :: Vec a n -> [a]
toList Nil = []
toList (x :. xs) = x : toList xs

map' :: (a -> b) -> Vec a n -> Vec b n
map' _  Nil      = Nil
map' f (x :. xs) = f x :. map' f xs
