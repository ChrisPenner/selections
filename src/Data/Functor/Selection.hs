{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language UndecidableInstances #-}
{-# language StandaloneDeriving #-}
module Data.Functor.Selection
  ( -- * SelectionT
  SelectionT(..)
  -- ** Selecting/Deselecting
  -- | Most selection combinators require that both the selected and unselected types
  -- be equal (i.e. SelectionT f a a); this is necessary since items will switch
  -- their selection status. Your selected and unselected types may diverge, but 
  -- you'll need to unify them in order to extract your underlying functor.
  , newSelection
  , forgetSelection
  , select
  , include
  , exclude
  , selectAll
  , deselectAll
  , invertSelection
  , onSelected
  , onUnselected
  , getSelected
  , getUnselected

  -- * Comonad Combinators
  , selectWithContext
  ) where

import Control.Comonad (Comonad(..))
import Control.Monad.Trans
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))

-- | A monad transformer for performing actions over selected
-- values. Combinators are provided to select specific values within
-- the underlying Functor. This transformer is isomorphic to EitherT,
-- but interprets semantics differently, thus providing a different
-- understanding and different combinators.
newtype SelectionT f b a = SelectionT {
  -- | Expose the underlying representation of a selection, this is
  -- isomorphic to EitherT.
  runSelectionT :: f (Either b a)
  } deriving (Functor, Foldable)

deriving instance (Eq (f (Either b a))) => Eq (SelectionT f b a)
deriving instance (Show (f (Either b a))) => Show (SelectionT f b a)

instance (Applicative f) => Applicative (SelectionT f b) where
  pure = SelectionT . pure . pure
  SelectionT fa <*> SelectionT ga = SelectionT ((<*>) <$> fa <*> ga)

-- | SelectionT is a monad over selected items when the underlying m is a Monad
instance (Monad f) => Monad (SelectionT f b) where
  return = pure
  SelectionT m >>= k =
    SelectionT $ m >>= either (return . Left) (runSelectionT . k)

-- | Bifunctor over unselected ('first') and selected ('second') values
instance (Functor f) => Bifunctor (SelectionT f) where
  first f = SelectionT . fmap (first f) . runSelectionT
  second = fmap

-- | Bifoldable over unselected and selected values respectively
instance (Foldable f) => Bifoldable (SelectionT f) where
  bifoldMap l r = foldMap (bifoldMap l r) . runSelectionT

-- | Bitraversable over unselected and selected values respectively
instance (Traversable f) => Bitraversable (SelectionT f) where
  bitraverse l r = fmap SelectionT . traverse (bitraverse l r) . runSelectionT

-- | Create a selection from a functor by selecting all values
newSelection :: (Functor f) => f a -> SelectionT f () a
newSelection = SelectionT . fmap Right

-- | Drops selection from your functor returning all values (selected or not).
--
-- @forgetSelection . newSelection = id@
forgetSelection :: Functor f => SelectionT f a a -> f a
forgetSelection = fmap (either id id) . runSelectionT

-- | Clear the selection then select only items which match a predicate.
-- 
-- @select f = include f . deselectAll@
select :: Functor f => (a -> Bool) -> SelectionT f a a -> SelectionT f a a
select f = include f . deselectAll

-- | Add items which match a predicate to the current selection
-- 
-- @include f . select g = select (\a -> f a || g a)@
include :: Functor f => (a -> Bool) -> SelectionT f a a -> SelectionT f a a
include f = SelectionT . fmap (either (choose f) Right) . runSelectionT

-- | Remove items which match a predicate to the current selection
-- 
-- @exclude f . select g = select (\a -> f a && not (g a))@
exclude :: Functor f => (a -> Bool) -> SelectionT f a a -> SelectionT f a a
exclude f = SelectionT . fmap (either Left (switch . choose f)) . runSelectionT

-- | Select all items in the container
-- 
-- @selectAll = include (const True)@
selectAll :: (Functor f) => SelectionT f a a -> SelectionT f a a
selectAll = include (const True)

-- | Deselect all items in the container
-- 
-- @deselectAll = exclude (const True)@
deselectAll :: (Functor f) => SelectionT f a a -> SelectionT f a a
deselectAll = exclude (const True)

-- | Flip the selection, all selected are now unselected and vice versa.
invertSelection :: Functor f => SelectionT f b a -> SelectionT f a b
invertSelection = SelectionT . fmap switch . runSelectionT

-- | Map over selected values
-- 
-- @onSelected = fmap@
onSelected :: Functor f => (a -> c) -> SelectionT f b a -> SelectionT f b c
onSelected = fmap

-- | Map over unselected values
-- 
-- @onSelected = first@
onUnselected :: Functor f => (b -> c) -> SelectionT f b a -> SelectionT f c a
onUnselected = first

-- | Collect all selected values into a list. For more complex operations use
-- foldMap.
-- 
-- @getSelected = foldMap (:[])@
getSelected :: Foldable f => SelectionT f b a -> [a]
getSelected = foldMap (:[])

-- | Collect all unselected values into a list. For more complex operations use
-- operations from Bifoldable.
-- 
-- @getUnselected = foldMap (:[]) . invertSelection@
getUnselected :: (Functor f, Foldable f) => SelectionT f b a -> [b]
getUnselected = foldMap (:[]) . invertSelection


-- Comonad combinators

-- | Select values based on their context within a comonad. This combinator makes
-- its selection by running the predicate using extend.
selectWithContext :: Comonad w => (w a -> Bool) -> SelectionT w a a -> SelectionT w a a
selectWithContext f = SelectionT . extend (choose' extract f) . forgetSelection


-- Helpers
choose' :: (a -> b) -> (a -> Bool) -> a -> Either b b
choose' f p a = if p a then Right (f a)
                      else Left (f a)

choose :: (a -> Bool) -> a -> Either a a
choose = choose' id

switch :: Either a b -> Either b a
switch = either Right Left

