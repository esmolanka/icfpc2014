
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.RecursionSchemes where

import Control.Monad
import Data.Traversable (Traversable)
import qualified Data.Traversable as Traversable

newtype Fix f = Fix { unFix :: f (Fix f) }

deriving instance (Show (f (Fix f))) => Show (Fix f)
deriving instance (Eq (f (Fix f))) => Eq (Fix f)
deriving instance (Ord (f (Fix f))) => Ord (Fix f)

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

cataM :: (Traversable f, Monad m) => (f a -> m a) -> Fix f -> m a
cataM alg = alg <=< Traversable.mapM (cataM alg) . unFix

ana :: (Functor f) => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

