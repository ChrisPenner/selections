{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
module Control.Lens.Selection
  ( selected
  , unselected
  , inverted
  , unwrapped
  ) where

import Data.Functor.Selection
import Data.Bitraversable

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

-- | Traversal over selected elements
--
-- @'selected' = 'traverse'@
selected :: (Traversable f) => Traversal (Selection f b a) (Selection f b a') a a'
selected = traverse

-- | Traversal over unselected elements
--
-- @'unselected' = 'inverting' . 'selected'@
unselected :: (Traversable f) => Traversal (Selection f b a) (Selection f b' a) b b'
unselected = inverted . selected

-- | Traversal which inverts the current selection
--
-- This is provided as a Traversal to avoid adding a 'profunctors' dependency.
-- You can write the corresponding iso if you wish:
--
-- @
-- 'inverting' = iso 'invertSelection' 'invertSelection'
-- @
inverted :: (Functor f) => Traversal (Selection f b a) (Selection f b' a') (Selection f a b) (Selection f a' b')
inverted f s = invertSelection <$> f (invertSelection s)

-- | Traversal which exposes the underlying functor representation
--
-- This is provided as a Traversal to avoid adding a 'profunctors' dependency.
-- You can write the corresponding iso if you wish:
--
-- @'unwrapping' = iso 'unwrapSelection' 'wrapSelection'@
unwrapped :: Traversal (Selection f b a) (Selection f b' a') (f (Either b a)) (f (Either b' a'))
unwrapped f s = Selection <$> f (unwrapSelection s)
