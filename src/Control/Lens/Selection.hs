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

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a
type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type Iso' s a = Iso s s a a

-- | Traversal over selected elements
--
-- @'selected' = 'traverse'@
selected :: (Traversable f) => Traversal (Selection f b a) (Selection f b c) a c
selected = traverse

-- | Traversal over unselected elements
--
-- @'unselected' = 'inverting' . 'selected'@
unselected :: (Traversable f) => Traversal (Selection f b a) (Selection f c a) b c
unselected = inverting . selected


-- | Iso which inverts the current selection
--
-- @'inverting' = iso 'invertSelection' 'invertSelection'@
inverting :: (Functor f, Functor g) => Iso (Selection f b a) (Selection g d c) (Selection f a b) (Selection g c d)
inverting = dimap invertSelection (fmap invertSelection)

-- | Iso which exposes the underlying functor representation
--
-- @'unwrapping' = iso 'unwrapSelection' 'wrapSelection'@
unwrapping :: (Functor f) => Iso (Selection f b a) (Selection g d c) (f (Either b a)) (g (Either d c))
unwrapping = dimap unwrapSelection (fmap Selection)
