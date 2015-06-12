# pagure-haskell

A pure Haskell client to the Pagure API. Based on high quality libraries such
as Wreq, Aeson, and Lens.

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
