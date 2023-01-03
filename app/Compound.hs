-- TODO release this as a library? look in to how it relates to free monads
module Compound (Compound, single, run) where

import Control.Monad (ap)

data Compound f a where
    Compound :: Compound f a -> (a -> Compound f b) -> Compound f b
    One :: f a -> Compound f a
    Simple :: a -> Compound f a
instance Functor (Compound f) where
    fmap = (<*>) . pure
instance Applicative (Compound f) where
    pure = Simple
    (<*>) = ap
instance Monad (Compound f) where
    (>>=) = Compound

single :: f a -> Compound f a
single = One

run :: Monad m => (forall r. f r -> m r) -> Compound f a -> m a
run go = \case
    Compound m f -> run go m >>= (run go . f)
    One a -> go a
    Simple x -> pure x
