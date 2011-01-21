----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.Trans.Tagged
-- Copyright  : 2011 Edward Kmett
-- License    : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-------------------------------------------------------------------------------

module Control.Monad.Trans.Tagged
    ( 
    -- * Tagged values
      Tagged
    , TaggedT(..)
    , retag
    , tag, tagSelf
    , untag, untagSelf
    , asTaggedTypeOf
    ) where

import Prelude hiding (foldr, foldl, mapM, sequence, foldr1, foldl1)
import Control.Applicative
import Control.Monad (liftM, MonadPlus(..))
import Control.Monad.Fix
import Data.Traversable (Traversable(..))
import Data.Foldable (Foldable(..))
import Data.Functor.Identity

-- | A @'Tagged' s b@ value is a value @b@ with an attached phantom type @s@.
-- This can be used in place of the more traditional but less safe idiom of
-- passing in an undefined value with the type, because unlike an @(s -> b)@, 
-- a @'Tagged' s b@ can't try to use the argument @s@ as a real value.
--
-- Moreover, you don't have to rely on the compiler to inline away the extra
-- argument, because the newtype is "free"

newtype TaggedT s m b = TagT { untagT :: m b } 
  deriving ( Eq, Ord, Read, Show )

type Tagged s = TaggedT s Identity

instance Functor m => Functor (TaggedT s m) where 
    fmap f (TagT x) = TagT (fmap f x)
    b <$ (TagT x) = TagT (b <$ x)
    {-# INLINE fmap #-}

instance Applicative m => Applicative (TaggedT s m) where
    pure = TagT . pure
    {-# INLINE pure #-}
    TagT f <*> TagT x = TagT (f <*> x)
    {-# INLINE (<*>) #-}
    TagT f  *> TagT x = TagT (f  *> x)
    {-# INLINE ( *>) #-}
    TagT f <*  TagT x = TagT (f <*  x)
    {-# INLINE (<* ) #-}

instance Alternative m => Alternative (TaggedT s m) where
    empty = TagT empty
    TagT a <|> TagT b = TagT (a <|> b)

instance Monad m => Monad (TaggedT s m) where
    return = TagT . return
    {-# INLINE return #-}
    TagT m >>= k = TagT (m >>= untagT . k)
    {-# INLINE (>>=) #-}
    TagT m >> TagT n = TagT (m >> n)
    {-# INLINE (>>) #-}

instance MonadPlus m => MonadPlus (TaggedT s m) where
    mzero = TagT mzero
    mplus (TagT a) (TagT b) = TagT (mplus a b)

instance MonadFix m => MonadFix (TaggedT s m) where
    mfix f = TagT $ mfix (untagT . f) 

instance Foldable f => Foldable (TaggedT s f) where
    foldMap f (TagT x) = foldMap f x
    {-# INLINE foldMap #-}
    fold (TagT x) = fold x
    {-# INLINE fold #-}
    foldr f z (TagT x) = foldr f z x
    {-# INLINE foldr #-}
    foldl f z (TagT x) = foldl f z x
    {-# INLINE foldl #-}
    foldl1 f (TagT x) = foldl1 f x 
    {-# INLINE foldl1 #-}
    foldr1 f (TagT x) = foldr1 f x
    {-# INLINE foldr1 #-}

instance Traversable f => Traversable (TaggedT s f) where
    traverse f (TagT x) = TagT <$> traverse f x
    {-# INLINE traverse #-}
    sequenceA (TagT x) = TagT <$> sequenceA x
    {-# INLINE sequenceA #-}
    mapM f (TagT x) = liftM TagT (mapM f x)
    {-# INLINE mapM #-}
    sequence (TagT x) = liftM TagT (sequence x)
    {-# INLINE sequence #-}

-- | Some times you need to change the tag you have lying around.
-- Idiomatic usage is to make a new combinator for the relationship between the
-- tags that you want to enforce, and define that combinator using 'retag'.
--
-- > data Succ n
-- > retagSucc :: Tagged n a -> Tagged (Succ n) a
-- > retagSucc = retag
retag :: TaggedT s m b -> TaggedT t m b
retag = TagT . untagT
{-# INLINE retag #-}

-- | 'asTaggedTypeOf' is a type-restricted version of 'const'. It is usually used as an infix operator, and its typing forces its first argument (which is usually overloaded) to have the same type as the tag of the second.
asTaggedTypeOf :: s -> TaggedT s m b -> s
asTaggedTypeOf = const
{-# INLINE asTaggedTypeOf #-}

tag :: b -> Tagged s b
tag = TagT . Identity
{-# INLINE tag #-}

untag :: Tagged s b -> b
untag = runIdentity . untagT
{-# INLINE untag #-}

-- | Tag a value with its own type.
tagSelf :: a -> Tagged a a
tagSelf = tag
{-# INLINE tagSelf #-}

-- | 'untagSelf' is a type-restricted version of 'untag'.
untagSelf :: Tagged a a -> a
untagSelf = untag
{-# INLINE untagSelf #-}

