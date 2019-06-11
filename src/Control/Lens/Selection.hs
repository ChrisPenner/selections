{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
module Control.Lens.Selection
  ( selected
  , unselected
  , inverting
  , unwrapping
  ) where

import Data.Profunctor (Profunctor(..))
import Data.Functor.Selection
import Data.Bitraversable

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a
type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type Iso' s a = Iso s s a a
type LensLike f s t a b = (a -> f b) -> s -> f t

-- | Traversal over selected elements
--
-- @'selected' = 'traverse'@
-- See also 'firsting'
selected :: (Traversable f) => Traversal' (Selection f b a) a
selected = traverse

-- | Traversal over unselected elements
--
-- @'unselected' = 'inverting' . 'selected'@
-- See also 'seconding'
unselected :: (Traversable f) => Traversal' (Selection f b a) b
unselected = inverting . selected


-- | Iso which inverts the current selection
--
-- @'inverting' = iso 'invertSelection' 'invertSelection'@
inverting :: (Functor f) => Iso' (Selection f b a) (Selection f a b)
inverting = dimap invertSelection (fmap invertSelection)
-- Note to self; write `Swapped` instance from 'lens'

-- | Iso which exposes the underlying functor representation
--
-- @'unwrapping' = iso 'unwrapSelection' 'wrapSelection'@
unwrapping :: (Functor f) => Iso' (Selection f b a)  (f (Either b a))
unwrapping = dimap unwrapSelection (fmap Selection)

-- | Given a functor as input
selecting :: (Functor f) => (a -> Bool) -> Iso (f a) (f b) (Selection f a a) (Selection f b b)
selecting predicate = dimap (select predicate . newSelection) (fmap forgetSelection)
