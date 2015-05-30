{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure.Extras
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Access to the \"Extras\" endpoints of the Pagure API.
----------------------------------------------------------------------------
module Web.Pagure.Extras where

import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Wreq
import Web.Pagure.Internal.Wreq
import Web.Pagure.Types

-- | Access the @/version@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT version pc
-- Response {responseStatus = Status {statusCode = 200, [...]
-- @
version :: PagureT (Response BL.ByteString)
version = pagureGet "/version"
