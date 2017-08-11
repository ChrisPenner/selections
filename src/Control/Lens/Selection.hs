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
selected :: (Selectable s t, Traversable (s b)) => Traversal (s b a) (s b c) a c
selected = traverse

-- | Traversal over unselected elements
--
-- @'unselected' = 'inverting' . 'selected'@
unselected :: (Selectable s g, Traversable (s a)) => Traversal (s b a) (s c a) b c
unselected = inverting . selected


-- | Iso which inverts the current selection
--
-- @'inverting' = iso 'invertSelection' 'invertSelection'@
inverting :: (Selectable s f, Selectable t g) => Iso (s b a) (t d c) (s a b) (t c d)
inverting = dimap invertSelection (fmap invertSelection)

-- | Iso which exposes the underlying functor representation
--
-- @'unwrapping' = iso 'unwrapSelection' 'wrapSelection'@
unwrapping :: (Functor f, Selectable s f, Selectable t g) => Iso (s b a) (t d c) (f (Either b a)) (g (Either d c))
unwrapping = dimap unwrapSelection (fmap wrapSelection)
