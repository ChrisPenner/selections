{-# language FlexibleInstances #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language UndecidableInstances #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
module Data.Functor.Selection
  ( -- * Selection
  Selection(..)
  , modifySelection
  , Selection'
  -- ** Selecting/Deselecting
  -- | Most selection combinators require that both the selected and unselected types
  -- be equal (i.e. Selection f a a); this is necessary since items will switch
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
  , mapSelected
  , mapUnselected
  , getSelected
  , getUnselected
  , unify
  , trans

  -- * Comonad Combinators
  , selectWithContext
  ) where

import Control.Monad (ap)
import Control.Comonad (Comonad(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))

-- | A selection wraps a Functor @f@ and has an unselected type @b@ and a selected type @a@
newtype Selection f b a = Selection
  { -- | Expose the underlying representation of a 'Selection'
    unwrapSelection :: f (Either b a)
  } deriving (Functor, Foldable, Traversable)

-- | A type alias for selections with the same unselected/selected types
type Selection' f a = Selection f a a

deriving instance (Show (f (Either b a))) => Show (Selection f b a)
deriving instance (Eq (f (Either b a))) => Eq (Selection f b a)

instance Monad m => Applicative (Selection m b) where
  pure = Selection . pure . Right
  (<*>) = ap

-- | Selection is a monad over selected items when the underlying m is a Monad
instance (Monad m) => Monad (Selection m b) where
  return = pure
  Selection m >>= k =
    Selection $ m >>= either (return . Left) (unwrapSelection . k)

-- | Bifunctor over unselected ('first') and selected ('second') values
instance (Functor f) => Bifunctor (Selection f) where
  first f = Selection . fmap (first f) . unwrapSelection
  second = fmap

-- | Bifoldable over unselected and selected values respectively
instance (Foldable f) => Bifoldable (Selection f) where
  bifoldMap l r = foldMap (bifoldMap l r) . unwrapSelection

-- | Bitraversable over unselected and selected values respectively
instance (Traversable f) => Bitraversable (Selection f) where
  bitraverse l r = fmap Selection . traverse (bitraverse l r) . unwrapSelection

-- Modify the underlying representation of a selection
modifySelection :: (Functor f) => (f (Either b a) -> g (Either d c)) -> Selection f b a -> Selection g d c
modifySelection f = Selection . f . unwrapSelection

-- | Create a selection from a functor by selecting all values
newSelection :: (Functor f) => f a -> Selection f b a
newSelection = Selection . fmap Right

-- | Drops selection from your functor returning all values (selected or not).
--
-- @'forgetSelection' . 'newSelection' = id@
--
-- @'forgetSelection' = 'unify' id id@
forgetSelection :: (Functor f) => Selection f a a -> f a
forgetSelection = unify id id

-- | Clear the selection then select only items which match a predicate.
--
-- @'select' f = 'include' f . 'deselectAll'@
select :: (Functor f) => (a -> Bool) -> Selection f a a -> Selection f a a
select f = include f . deselectAll


-- | Add items which match a predicate to the current selection
--
-- @'include' f . 'select' g = 'select' (\a -> f a || g a)@
include :: (Functor f) => (a -> Bool) -> Selection f a a -> Selection f a a
include f = modifySelection (fmap (either (choose f) Right))

-- | Remove items which match a predicate to the current selection
--
-- @'exclude' f . 'select' g = 'select' (\a -> f a && not (g a))@
exclude :: (Functor f) => (a -> Bool) -> Selection f a a -> Selection f a a
exclude f = modifySelection (fmap (either Left (switch . choose f)))

-- | Select all items in the container
--
-- @'selectAll' = 'include' (const True)@
selectAll :: (Functor f) => Selection f a a -> Selection f a a
selectAll = include (const True)

-- | Deselect all items in the container
--
-- @'deselectAll' = 'exclude' (const True)@
deselectAll :: (Functor f) => Selection f a a -> Selection f a a
deselectAll = exclude (const True)

-- | Flip the selection, all selected are now unselected and vice versa.
invertSelection :: (Functor f) => Selection f b a -> Selection f a b
invertSelection = modifySelection (fmap switch)

-- | Map over selected values
--
-- @'mapSelected' = fmap@
mapSelected :: (Functor f) => (a -> c) -> Selection f b a -> Selection f b c
mapSelected = fmap

-- | Map over unselected values
--
-- @'mapUnselected' = 'first'@
mapUnselected :: (Functor f) => (b -> c) -> Selection f b a -> Selection f c a
mapUnselected = first

-- | Collect all selected values into a list. For more complex operations use
-- foldMap.
--
-- @'getSelected' = foldMap (:[])@
getSelected :: (Foldable f) => Selection f b a -> [a]
getSelected = foldMap (:[])

-- | Collect all unselected values into a list. For more complex operations use
-- operations from Bifoldable.
--
-- @'getUnselected' = 'getSelected' . 'invertSelection'@
getUnselected :: (Foldable f, Functor f) => Selection f b a -> [b]
getUnselected = bifoldMap (:[]) (const [])

-- | Unify selected and unselected and forget the selection
--
-- @'unify' f g == 'forgetSelection' . 'onUnselected' f . 'onSelected' g@
unify :: (Functor f) => (b -> c) -> (a -> c) -> Selection f b a -> f c
unify l r = fmap (either l r) . unwrapSelection

-- | Perform a natural transformation over the underlying container of a selectable
trans :: (Functor f) => (forall c. f c -> g c) -> Selection f b a -> Selection g b a
trans f = modifySelection f

-- Comonad combinators

-- | Select values based on their context within a comonad. This combinator makes
-- its selection by running the predicate using extend.
selectWithContext :: (Comonad w) => (w a -> Bool) -> Selection w a a -> Selection w a a
selectWithContext f = modifySelection (extend (choose' extract f) .  fmap (either id id))


-- Helpers
choose' :: (a -> b) -> (a -> Bool) -> a -> Either b b
choose' f p a = if p a then Right (f a)
                      else Left (f a)

choose :: (a -> Bool) -> a -> Either a a
choose = choose' id

switch :: Either a b -> Either b a
switch = either Right Left
