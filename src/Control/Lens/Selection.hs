{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
module Control.Lens.Selection
  ( selected
  -- , unselected
  , inverting
  , unwrapping
  ) where

import Data.Bitraversable (bitraverse)
import Data.Profunctor (Profunctor(..))
import Data.Functor.Selection

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a
type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type Iso' s a = Iso s s a a

-- | Traversal over selected elements
--
-- @'selected' = 'traverse'@
selected :: (Selectable s f b, Traversable s, Selectable t f b, Traversable f) => Traversal (s a) (t c) a c
selected f = fmap wrapSelection . traverse (bitraverse pure f) . unwrapSelection

-- -- | Traversal over unselected elements
-- --
-- -- @'unselected' = 'inverting' . 'selected'@
-- unselected :: (Selectable s f b, Selectable t f c) => Traversal (s a) (s a) b c
-- unselected = inverting . selected
--   where
--     inv :: Iso' (s a) (t b)
--     inv = inverting
--     sel :: Traversal (t b) (s a) b a
--     sel = selected


-- | Iso which inverts the current selection

-- @'inverting' = iso 'invertSelection' 'invertSelection'@
inverting :: forall s t a b f. (Selectable s f b, Selectable t f a) => Iso' (s a) (t b)
inverting = dimap (invertSelection :: s a -> t b) (fmap invertSelection)

-- | Iso which exposes the underlying functor representation
--
-- @'unwrapping' = iso 'unwrapSelection' 'wrapSelection'@
unwrapping :: (Functor f, Selectable s f b, Selectable t g d) => Iso (s a) (t c) (f (Either b a)) (g (Either d c))
unwrapping = dimap unwrapSelection (fmap wrapSelection)
