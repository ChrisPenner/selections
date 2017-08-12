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
-   Traverse or fold over selections using `Control.Lens`

Here's how it looks, tutorials are available
[here](https://github.com/ChrisPenner/selections/tree/master/tutorials).

```haskell
xs = [1..6]
λ> newSelection xs & select even & mapSelected (+100) & bimap (("Odd: " ++) . show) (("Even: " ++) . show) & forgetSelection
["Odd: 1","Even: 102","Odd: 3","Even: 104","Odd: 5","Even: 106"]
```

Technically you could use `Selection` as a monad-transformer, but it's a bit
clunky and you'd probably be better off with
[`EitherT`](https://hackage.haskell.org/package/either-4.4.1.1/docs/Control-Monad-Trans-Either.html).

Fun fact, `Selection` is isomorphic to `EitherT`, but the semantics are quite
different and they're suited to different purposes.

## When Should/Shouldn't I Use Selections?

You can use selections whenever you've got a bunch of things and you want to operate over just a few of them at a time.
You can do everything that selections provides by combining a bunch of predicates with fmap, but it gets messy really
quick; selections provides a clean interface for this sort of operation.

You shouldn't use selections when you're looking for a monadic interface for this sort of thing, selections works
at the value level and typically you want to chain selection commands using `(.)` or `(&)`, it technically can
be used as a monad transformer if your underlying functor is also a monad, but at that point you may wish to check
out [`EitherT`](https://hackage.haskell.org/package/either-4.4.1.1/docs/Control-Monad-Trans-Either.html) instead.

## Examples

Check out the [Accounts
tutorial](https://github.com/ChrisPenner/selections/tree/master/tutorials/Basic.md)
first to get your bearings. After that continue to the [Lenses
tutorial](https://github.com/ChrisPenner/selections/tree/master/tutorials/Lenses.md).
