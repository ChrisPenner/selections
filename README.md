Selections
==========
[![Hackage](https://img.shields.io/badge/hackage-latest-green.svg)](https://hackage.haskell.org/package/selections)

[See it on Hackage](https://hackage.haskell.org/package/selections)

`selections` is a haskell package for transforming subsets of values within a functor using
an intuitive selection-based interface.

Ever wished you could select just a few values within a functor, perform some
operations on them, then flatten them back into the plain old functor again? Now
you can!

`Selection` is a newtype wrapper around Functors which adds several
combinators and interesting instances. Wrapping a functor in `Selection` allows
you to:

-   Select specific values within your functor according to a predicate
-   Expand/Contract selections based on additional predicates using `include`
    and `exclude`
-   Select values based on their context if your functor is also a Comonad
-   Map over unselected and/or selected values using `Bifunctor`
-   Traverse over unselected and/or selected values using `Bitraversable`
-   Fold over unselected and/or selected values using `Bifoldable`
-   Perform monad computations over selected values if your functor is a Monad
-   Extract all unselected or selected elements to a list
-   Deselect and return to your original functor using `unify`

Lenses and traversals coming eventually!

Technically you could use `Selection` as a monad-transformer, but it's a bit
clunky and you'd probably be better off with
[`EitherT`](https://hackage.haskell.org/package/either-4.4.1.1/docs/Control-Monad-Trans-Either.html).
Fun fact, `Selection` is isomorphic to `EitherT`, but the semantics are quite
different and they're suited to different purposes.

## Examples

We'll start off using a simple list as our underlying functor.

You may find it useful to throw in some [Type
Applications](https://ghc.haskell.org/trac/ghc/wiki/TypeApplication) to help
disambiguate type for the compiler. This typically isn't an issue in compiled code,
but the interpreter can get confused from time to time.

```haskell
{-# language TypeApplications #-}
import Data.Functor.Selection

-- This combinator is super handy for chaining selections along
(&) :: a -> (a -> c) -> c
(&) = flip ($)

xs :: [Int]
xs = [1..6]
```

Let's select just the even numbers and see what we get!

```haskell
evens :: Selection [] Int Int
evens = newSelection xs & select even
-- Selection {runSelection = [Left 1,Right 2,Left 3,Right 4,Left 5,Right 6]}
```

Cool, we can see that the underlying representation consists of our original
functor, except it uses Either to show which elements are selected. Let's 
multiply our odd elements by two with `mapUnselected`

```haskell
byTwo :: Selection [] Int Int
byTwo = evens & mapUnselected (*2)
-- Selection {runSelection = [Left 2,Right 2,Left 6,Right 4,Left 10,Right 6]}
```

Notice that even though the numbers became even as a result of the transformation
the same elements remain selected. If you wanted to you could run 'select even'
again to include the new elements.

Let's exclude anything greater than 5 from our selection, then get the numbers
that remain selected as a list

```haskell
excluded :: [Int]
excluded = byTwo & exclude (>5) & getSelected
-- [2, 4]
```
Nice! Looks like it worked! Notice how the order of elements remained 
the same through the whole thing!

The types of the selected and unselected elements are allowed to diverge!
So long as they line up when we decide to get the results then it's all good!
We can use `unify :: Selectable s f => (b -> c) -> (a -> c) -> s b a -> f c` to
help us out with that.

Let's try it out!

```haskell
diverged :: Selection [] String Int
diverged = newSelection @(Selection []) [1..6] & select even & mapUnselected show
-- Selection {runSelection = [Left "1",Right 2,Left "3",Right 4,Left "5",Right 6]}
 
unified :: [Int]
unified = diverged & mapUnselected ("100"++) & unify read id
[1001,2,1003,4,1005,6]
```

`[]` is a great functor to try out because it has Applicative and Monad instances
defined, which both act pretty much as you'd expect, but they only consider selected
values!

```haskell
evens :: Selection [] Int Int
evens = newSelection [1..6] & select even 

plus10 :: Selection [] Int Int
plus10 = do
  x <- evens
  newSelection [x + 10, x + 100]
-- Selection {runSelection = [Left 1,Right 12,Right 102,Left 3,Right 14,Right 104,Left 5,Right 16,Right 106]}
```


