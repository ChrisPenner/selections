> {-# language TemplateHaskell #-}
> module Examples.Lens where

> import Data.Functor.Selection
> import Control.Lens.Selection
> import Control.Lens

Here we'll extend the Accounts example to show off the selection lenses a bit,
let's set up our data structures again, but this time we'll also derive some lenses!

> data Country = Canada | USA
>   deriving (Show, Eq)
>
> data Account = Account
>   { _name :: String
>   , _country :: Country
>   , _balance :: Float
>   } deriving (Show, Eq)
> makeLenses 'Account

> accounts :: [Account]
> accounts =
>   [ Account "Steve" Canada 34
>   , Account "Cindy" USA    10
>   , Account "Blake" USA    (-6)
>   , Account "Carl"  Canada (-16)
>   ]

Following from the previous tutorial we can select american users:

> americans :: Selection' [] Account
> americans = newSelection accounts & select ((==USA) . view country)

Now we can use our traversals from `Control.Lens.Selection`! Let's get a list of the 
selected accounts:

```
λ> americans ^.. selected
[ Account {_name = "Cindy", _country = USA, _balance = 10.0}
, Account {_name = "Blake", _country = USA, _balance = -6.0}]
```

`selected` is a traversal, so that means we can compose it with other lenses and 
perform operations!

```
λ> americans & selected . balance +~ 100 & forgetSelection
[ Account {_name = "Steve", _country = Canada, _balance = 34.0}
, Account {_name = "Cindy", _country = USA, _balance = 110.0}
, Account {_name = "Blake", _country = USA, _balance = 94.0}
, Account {_name = "Carl", _country = Canada, _balance = -16.0}]
```

Nifty! We've composed lenses to adjust a value inside only the selected accounts!

We also have the `inverting` iso, let's get the names of our Canadians:

```
λ> americans ^.. inverting . selected . name
["Steve", "Carl"]
```

Of course `unselected = inverting . selected`, so you should probably use that, but
I just wanted to show off.

That's pretty much it for lenses, hope they come in handy!
