{-# language FlexibleInstances #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language UndecidableInstances #-}
{-# language RankNTypes #-}
{-# language FunctionalDependencies #-}
{-# language StandaloneDeriving #-}
module Data.Functor.Selection
  ( -- * Selection
  Selection(..)
  , Selectable(..)
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

-- | You can make any type selectable if it contains a functor of (Either b a)
class Functor f => Selectable s f b | s -> f, s -> b where
  -- Modify the underlying representation of a selection
  modifySelection :: (Selectable t f d) => (f (Either b a) -> f (Either d c)) -> s a -> t c
  modifySelection f = wrapSelection . f . unwrapSelection

  -- | Lift a functor into the Selectable
  wrapSelection :: f (Either b a) -> s a

  -- | Extract the underlying functor from the Selectable
  unwrapSelection :: s a -> f (Either b a)


-- | The simplest selection type
newtype Selection f b a = Selection 
  { -- | Expose the underlying representation of a Selection
    runSelection :: f (Either b a)
  } deriving (Functor, Foldable, Traversable)

deriving instance (Show (f (Either b a))) => Show (Selection f b a)
deriving instance (Eq (f (Either b a))) => Eq (Selection f b a)

instance Monad m => Applicative (Selection m b) where
  pure = Selection . pure . Right
  (<*>) = ap

-- | Selection is a monad over selected items when the underlying m is a Monad
instance (Monad m) => Monad (Selection m b) where
  return = pure
  Selection m >>= k =
    Selection $ m >>= either (return . Left) (runSelection . k)

-- | Bifunctor over unselected ('first') and selected ('second') values
instance (Functor f) => Bifunctor (Selection f) where
  first f = Selection . fmap (first f) . runSelection
  second = fmap

-- | Bifoldable over unselected and selected values respectively
instance (Foldable f) => Bifoldable (Selection f) where
  bifoldMap l r = foldMap (bifoldMap l r) . runSelection

-- | Bitraversable over unselected and selected values respectively
instance (Traversable f) => Bitraversable (Selection f) where
  bitraverse l r = fmap Selection . traverse (bitraverse l r) . runSelection

instance (Functor f) => Selectable (Selection f b) f b where
  -- modifySelection f (Selection s) = Selection $ f s
  wrapSelection = Selection
  unwrapSelection = runSelection

-- | Create a selection from a functor by selecting all values
newSelection :: (Selectable s f b) => f a -> s a
newSelection = wrapSelection . fmap Right

-- | Drops selection from your functor returning all values (selected or not).
--
-- @'forgetSelection' . 'newSelection' = id@
--
-- @'forgetSelection' = 'unify' id id@
forgetSelection :: (Selectable s f a) => s a -> f a
forgetSelection = unify id id

-- | Clear the selection then select only items which match a predicate.
--
-- @'select' f = 'include' f . 'deselectAll'@
select :: (Selectable s f a) => (a -> Bool) -> s a -> s a
select f = include f . deselectAll

-- | Add items which match a predicate to the current selection
--
-- @'include' f . 'select' g = 'select' (\a -> f a || g a)@
include :: (Selectable s f a) => (a -> Bool) -> s a -> s a
include f = modifySelection (fmap (either (choose f) Right))

-- | Remove items which match a predicate to the current selection
--
-- @'exclude' f . 'select' g = 'select' (\a -> f a && not (g a))@
exclude :: (Selectable s f a) => (a -> Bool) -> s a -> s a
exclude f = modifySelection (fmap (either Left (switch . choose f)))

-- | Select all items in the container
--
-- @'selectAll' = 'include' (const True)@
selectAll :: (Selectable s f a) => s a -> s a
selectAll = include (const True)

-- | Deselect all items in the container
--
-- @'deselectAll' = 'exclude' (const True)@
deselectAll :: (Selectable s f a) => s a -> s a
deselectAll = exclude (const True)

-- | Flip the selection, all selected are now unselected and vice versa.
invertSelection :: (Selectable s f b, Selectable t f a) => s a -> t b
invertSelection = modifySelection (fmap switch)

-- | Map over selected values
--
-- @'onSelected' = fmap@
mapSelected :: (Selectable s f b) => (a -> c) -> s a -> s c
mapSelected f = modifySelection (fmap (second f))

-- | Map over unselected values
--
-- @'onSelected' f = 'modifySelection' (fmap ('first' f))@
mapUnselected :: (Selectable s f b, Selectable t f c) => (b -> c) -> s a -> t a
mapUnselected f = modifySelection (fmap (first f))

-- | Collect all selected values into a list. For more complex operations use
-- foldMap.
--
-- @'getSelected' = foldMap (:[])@
getSelected :: (Selectable s f b, Foldable f) => s a -> [a]
getSelected = foldMap (bifoldMap (const []) pure) . unwrapSelection

-- | Collect all unselected values into a list. For more complex operations use
-- operations from Bifoldable.
--
-- @'getUnselected' = 'getSelected' . 'invertSelection'@
getUnselected :: (Selectable s f b, Foldable f) => s a -> [b]
getUnselected = foldMap (bifoldMap pure (const [])) . unwrapSelection

-- | Unify selected and unselected and forget the selection
--
-- @'unify' f g == 'forgetSelection' . 'onUnselected' f . 'onSelected' g@
unify :: (Selectable s f b) => (b -> c) -> (a -> c) -> s a -> f c
unify l r = fmap (either l r) . unwrapSelection

-- | Perform a natural transformation over the underlying container of a selectable
trans :: (Selectable s f b, Selectable t g b) => (forall c. f c -> g c) -> s a -> t a
trans f = wrapSelection . f . unwrapSelection

-- Comonad combinators

-- | Select values based on their context within a comonad. This combinator makes
-- its selection by running the predicate using extend.
selectWithContext :: (Selectable s w a, Comonad w) => (w a -> Bool) -> s a -> s a
selectWithContext f = modifySelection (extend (choose' extract f) .  fmap (either id id))


-- Helpers
choose' :: (a -> b) -> (a -> Bool) -> a -> Either b b
choose' f p a = if p a then Right (f a)
                      else Left (f a)

choose :: (a -> Bool) -> a -> Either a a
choose = choose' id

switch :: Either a b -> Either b a
switch = either Right Left
