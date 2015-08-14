{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure.Users
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Access to the \"Users\" endpoints of the Pagure API.
----------------------------------------------------------------------------
module Web.Pagure.Users where

import Control.Lens
import Data.Aeson.Lens
import qualified Data.Text as T
import Network.Wreq
import Web.Pagure.Internal.Wreq
import Web.Pagure.Types

-- | Access the @/users@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT users pc
-- ["adamwill","alphacc","asamalik","ausil","bkabrda","bochecha",[...]
-- @
users ::
  Maybe T.Text -- ^ Optional pattern to search for
  -> PagureT [Username]
users pattern = do
  opts <- pagureWreqOptions
  let opts' = case pattern of
        Nothing -> opts
        Just p -> opts & param "pattern" .~ [p]
  resp <- pagureGetWith opts' "users"
  return $ resp ^.. responseBody . key "users" . values . _String

-- | Access the @/groups@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT groups pc
-- ["releng","Fedora-Infra"]
-- @
groups ::
  Maybe T.Text -- ^ Optional pattern to search for
  -> PagureT [Group]
groups pattern = do
  opts <- pagureWreqOptions
  let opts' = case pattern of
        Nothing -> opts
        Just p -> opts & param "pattern" .~ [p]
  resp <- pagureGetWith opts' "groups"
  return $ resp ^.. responseBody . key "groups" . values . _String
