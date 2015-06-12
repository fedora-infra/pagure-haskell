# pagure-haskell

A pure Haskell client to the Pagure API. Based on high quality libraries such
as Wreq, Aeson, and Lens.

We encapsulate all operations inside of a `PagureT` monad transformer. That is
to say, you can chain pagure requests together and run them as a (sequential)
batch with `runPagureT`. This allows for composibility of API calls.

We also define lenses for all custom types that we define. We make use of the
`lens` package's ability to generate typeclasses for fields and use type
inference to figure out which type we are trying to pull a field out of. We do
this so that we can name several lenses the same.

We also expose (but don't re-export, in `Web.Pagure`) the internal functions
we've written over Wreq, to make it easy to add or modify any endpoints we may
have missed or messed up. `pagureGetWith` corresponds to Wreq's `getWith`, and
so on. `pagureUrl :: String -> PagureT String ` will generate an appropriate URL
for accessing the API by querying the internal `Reader` monad (in which we store
a `PagureConfig`) for the base URL.

## Example

```haskell
module Main where

import Data.Default
import Web.Pagure

-- | We define (in Web.Pagure.Types) an instance of Default for PagureConfig.
-- It performs unauthenticated requests to <https://pagure.io>.
-- We use this here ('def').
main :: IO ()
main = runPagureT errorCodes def >>= print
```

# License

BSD-2. (c) 2015 Red Hat, Inc. See LICENSE for details.
