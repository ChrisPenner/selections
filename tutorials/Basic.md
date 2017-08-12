We'll need a few imports to start off:

```haskell
module Examples.Accounts where

import Data.Functor.Selection
import Data.Bifunctor
import Data.Bitraversable
import Control.Monad (void)
```

Let's see how selections can help us simplify some basic tasks; first we'll define
a simple user type so we can contrive some examples.

```haskell
data Country = Canada | USA
  deriving (Show, Eq)

data Account = Account
  { name :: String
  , country :: Country
  , balance :: Float
  } deriving (Show, Eq)
```

Okay, so our accounts are pretty simple, they have a name, country of origin, and an
account balance! Let's make a 'database' of accounts as a simple list:

```haskell
accounts :: [Account]
accounts =
  [ Account "Steve" Canada 34
  , Account "Cindy" USA    10
  , Account "Blake" USA    (-6)
  , Account "Carl"  Canada (-16)
  ]
```

Great! So far so good. Now we see where selections come in handy, let's say
we want to accumulate interest for all accounts with a POSITIVE account balance.
Normally we'd need to map over every user, check their account balance, then
perform the interest calculation. This code is pretty straightforward to write,
but it gets a bit clunky in more complex situations. With selections we can select
the accounts we want to work with, then map over them specifically!

```haskell
type Rate = Float
addInterest :: Rate -> Account -> Account
addInterest rate user = let newBalance = balance user * rate
                        in user{balance=newBalance}
```

One additional complication! USA and Canadian accounts get different interest rates!
No problem though, let's see what we can do!

```haskell
-- This is just a handy combinator, you might recognize it if you've worked with
-- Control.Lens
(&) :: a -> (a -> b) -> b
(&) = flip ($)

usaRate, canRate :: Rate
usaRate = 1.25
canRate = 1.10

adjusted :: [Account]
adjusted =
  newSelection accounts
    & select ((== USA) . country)
    & exclude ((< 0) . balance)
    & mapSelected (addInterest usaRate)

    & select ((== Canada) . country)
    & exclude ((< 0) . balance)
    & mapSelected (addInterest canRate)
    & forgetSelection
```

Here's what we end up with, you can see it made the proper adjustments without
touching the negative accounts!

```haskell
[ Account "Steve", Canada 34.0
, Account  "Cindy" USA 10.0
, Account "Blake" USA -6.0
, Account "Carl" Canada -16.0 ]
```

Okay! A lot just happened, lets break it down into smaller bits!

```haskell
americans :: Selection' [] Account
americans =
  newSelection accounts
    & select ((== USA) . country)
```

Okay so first thing, we need to create a selection around our list of users,
`newSelection` wraps any Functor in a `Selection` type. The Selection type
is as follows: `Selection f b a` where `f` is the underlying functor (in our
case a list), `b` represents the type of unselected data, `a` represents the
selected data. These types can diverge if you like, but in our case they're
both `Account` so we use the `Selected'` type alias instead, which is simply
defined as `type Selection' f a = Selection f a a`

Now that we have a new selection we need to tell it what to select!
`newSelection` selects all elements by default, which isn't all that useful,
so we use `select` with a predicate to determine which elements we want to
be in focus. In this case `select ((== USA) . country)` clears the previous
selection, then selects only american accounts.

If we print out `americans` now, this is what we'll see:

```haskell
Selection {unwrapSelection = 
  [ Left (Account {name = "Steve", country = Canada, balance = 34.0})
  , Right (Account {name = "Cindy", country = USA, balance = 10.0})
  , Right (Account {name = "Blake", country = USA, balance = -6.0})
  , Left (Account {name = "Carl", country = Canada, balance = -16.0})
  ]}
```

A bit messy, but we can see that accounts in the USA are wrapped in a `Right` whereas
the others are wrapped in a `Left`. As users of the selections library we don't
need to worry about all that, the interface takes care of the details, but it's cool
to see that it's working!

If we wanted to select the Canadians we could of course write a predicate for
that, OR since we know we're only tracking two countries right now we could use
`invertSelection` to flip the selection so that Canadians are focused and Americans
are unselected.

We can now use `getSelected` and `getUnselected` to get a list of USA or Canadian
accounts respectively, note how the `Right`s and `Left`s disappear whenever we
stop working within a Selection:

```haskell
λ> getSelected americans
[ Account {name = "Cindy", country = USA, balance = 10.0}
, Account {name = "Blake", country= USA, balance = -6.0}]
```

Back to the task at hand! We've got our americans selected, but Blake has a
negative account balance! Let's `exclude` any accounts with a negative balance.
`exclude` keeps the current selection, but punts out any elements that fail the
predicate. There's also an `include` combinator which will add any unselected
elements which do pass the predicate (if you're into that sort of thing).

```haskell
americanPositive :: Selection' [] Account
americanPositive =
    americans
    & exclude ((< 0) . balance)
```

Now we can finally make our transformation, `mapSelected` is provided if you
want a nicely named combinator, but it's actually just a synonym for fmap.

```haskell
americansAdjusted :: Selection' [] Account
americansAdjusted = americanPositive & mapSelected (addInterest usaRate)
```

All selections are `Bifunctors`, so you can `bimap` over the unselected and
selected values respectively if you like. Let's say there was a banking error
in our user's favour (it happens all the time I swear). All Americans get a $10
credit, all Canadians get a $7 credit!

```haskell
adjustBalance :: (Float -> Float) -> Account -> Account
adjustBalance f user = user{balance = f (balance user)}
withCredit :: [Account]
withCredit = 
  newSelection accounts 
  & select ((== USA) . country)
  & bimap (adjustBalance (+7)) (adjustBalance (+10))
  & forgetSelection
```

They're also Bitraversable and Bifoldable, so we can perform operations with
effects over each segment independently, or perform different effectful
operations over each type. Lets print out a warning to all users with a
negative balance!

```haskell
warnDelinquents :: IO ()
warnDelinquents = void $
  newSelection accounts 
  & select ((<0) . balance)
  & bitraverse congrats warn
  where
    warn user = putStrLn (name user ++ ": get your act together!")
    congrats user = putStrLn (name user ++ ": you're doing great!")
```

Here's how it works: 

```
λ> warnDelinquents
Steve: you're doing great!
Cindy: you're doing great!
Blake: get your act together!
Carl: get your act together!
```

You can use the Bifoldable instance to do similarly interesting things,
getSelected and getUnselected are provided as helpers which return lists of the
selected and unselected items.

That's it for this tutorial! Check out the Lenses tutorial if you want to take
things a bit further!
