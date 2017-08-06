{-# language FlexibleInstances #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language UndecidableInstances #-}
{-# language StandaloneDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
module Data.Functor.Selection
  ( -- * SelectionT
  SelectionT(..)
  -- ** Selecting/Deselecting
  -- | Most selection combinators require that both the selected and unselected types
  -- be equal (i.e. SelectionT a f a); this is necessary since items will switch
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

  -- * Bifunctor, Bifoldable, Bitraversable

  -- * Comonad Combinators
  , selectWithContext
  ) where

import Control.Comonad (Comonad(..))
import Control.Monad.Trans
import Data.Functor.Compose
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))

class Selectable s where
  -- | Lift a functor into the Selectable
  wrapSelection :: f (Either b a) -> s b f a
  -- | extract a functor from the Selectable
  unwrapSelection :: s b f a -> f (Either b a)

-- | A monad transformer for performing actions over selected
-- values. Combinators are provided to select specific values within
-- the underlying Functor. This transformer is isomorphic to EitherT,
-- but interprets semantics differently, thus providing a different
-- understanding and different combinators.
newtype SelectionT b f a = SelectionT {
  -- | Expose the underlying representation of a selection
  runSelectionT :: f (Either b a)
  } deriving (Functor, Foldable)

instance (Applicative f) => Applicative (SelectionT b f) where
  pure = SelectionT . pure . pure
  SelectionT fa <*> SelectionT ga = SelectionT ((<*>) <$> fa <*> ga)

-- | SelectionT is a monad over selected items when the underlying m is a Monad
instance (Monad f) => Monad (SelectionT b f) where
  return = pure
  SelectionT m >>= k =
    SelectionT $ m >>= either (return . Left) (runSelectionT . k)

instance (Selectable s) => MonadTrans (s b) where
  lift = newSelection

bimapSel :: (Selectable s, Functor f) => (b -> d) -> (a -> c) -> s b f a -> s d f c
bimapSel l r = wrapSelection . fmap (bimap l r) . unwrapSelection

bifoldMapSel :: (Monoid m, Selectable s, Foldable f) => (b -> m) -> (a -> m) -> s b f a -> m
bifoldMapSel l r = foldMap (bifoldMap l r) . unwrapSelection

bitraverseSel :: (Selectable s, Traversable f, Applicative f) => (b -> f d) -> (a -> f c) -> s b f a -> f (s d f c)
bitraverseSel  l r = fmap wrapSelection . traverse (bitraverse l r) . unwrapSelection

instance Selectable SelectionT where
  wrapSelection = SelectionT
  unwrapSelection = runSelectionT

-- | Create a selection from a functor by selecting all values
newSelection :: (Selectable s, Functor f) => f a -> s b f a
newSelection = wrapSelection . fmap Right

-- | Drops selection from your functor returning all values (selected or not).
--
-- @forgetSelection . newSelection = id@
forgetSelection :: (Selectable s, Functor f) => s a f a -> f a
forgetSelection = fmap (either id id) . unwrapSelection

-- | Clear the selection then select only items which match a predicate.
--
-- @select f = include f . deselectAll@
select :: (Selectable s, Functor f) => (a -> Bool) -> s a f a -> s a f a
select f = include f . deselectAll

-- | Add items which match a predicate to the current selection
--
-- @include f . select g = select (\a -> f a || g a)@
include :: (Selectable s, Functor f) => (a -> Bool) -> s a f a -> s a f a
include f = wrapSelection . fmap (either (choose f) Right) . unwrapSelection

-- | Remove items which match a predicate to the current selection
--
-- @exclude f . select g = select (\a -> f a && not (g a))@
exclude :: (Selectable s, Functor f) => (a -> Bool) -> s a f a -> s a f a
exclude f = wrapSelection . fmap (either Left (switch . choose f)) . unwrapSelection

-- | Select all items in the container
--
-- @selectAll = include (const True)@
selectAll :: (Selectable s, Functor f) => s a f a -> s a f a
selectAll = include (const True)

-- | Deselect all items in the container
--
-- @deselectAll = exclude (const True)@
deselectAll :: (Selectable s, Functor f) => s a f a -> s a f a
deselectAll = exclude (const True)

-- | Flip the selection, all selected are now unselected and vice versa.
invertSelection :: (Selectable s, Functor f) => s b f a -> s a f b
invertSelection = wrapSelection . fmap switch . unwrapSelection

-- | Map over selected values
--
-- @onSelected = fmap@
onSelected :: (Selectable s, Functor f) => (a -> c) -> s b f a -> s b f c
onSelected f = wrapSelection . fmap (second f) . unwrapSelection

-- | Map over unselected values
--
-- @onSelected = wrapSelection . fmap (first f) . unwrapSelection@
onUnselected :: (Selectable s, Functor f) => (b -> c) -> s b f a -> s c f a
onUnselected f = wrapSelection . fmap (first f) . unwrapSelection

-- | Collect all selected values into a list. For more complex operations use
-- foldMap.
--
-- @getSelected = foldMap (:[])@
getSelected :: (Selectable s, Foldable f) => s b f a -> [a]
getSelected = foldMap (bifoldMap (const []) pure) . unwrapSelection

-- | Collect all unselected values into a list. For more complex operations use
-- operations from Bifoldable.
--
-- @getUnselected = getSelected . invertSelection@
getUnselected :: (Selectable s, Functor f, Foldable f) => s b f a -> [b]
getUnselected = getSelected . invertSelection

-- | Unify selected and unselected and forget the selection
--
-- @unify f g == forgetSelection . onUnselected f . onSelected g@
unify :: (Selectable s, Functor f) => (b -> c) -> (a -> c) -> s b f a -> f c
unify l r = fmap (either l r) . unwrapSelection

-- | Perform a natural transformation over the underlying container of a selectable
trans :: (Selectable s) => (forall c. f c -> g c) -> s b f a -> s b g a
trans t = wrapSelection . t . unwrapSelection

-- Comonad combinators

-- | Select values based on their context within a comonad. This combinator makes
-- its selection by running the predicate using extend.
selectWithContext :: (Selectable s, Comonad w) => (w a -> Bool) -> s a w a -> s a w a
selectWithContext f = wrapSelection . extend (choose' extract f) . forgetSelection


-- Helpers
choose' :: (a -> b) -> (a -> Bool) -> a -> Either b b
choose' f p a = if p a then Right (f a)
                      else Left (f a)

choose :: (a -> Bool) -> a -> Either a a
choose = choose' id

switch :: Either a b -> Either b a
switch = either Right Left

(&) :: a -> (a -> c) -> c
(&) = flip ($)

-- x :: [String]
-- x = newSelection @SelectionT [1..5] & fmap (+10) & select (>12) & unify show show
