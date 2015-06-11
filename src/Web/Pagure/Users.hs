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
users :: PagureT [Username]
users = do
  resp <- pagureGet ("users")
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
groups :: PagureT [Group]
groups = do
  resp <- pagureGet ("groups")
  return $ resp ^.. responseBody . key "groups" . values . _String
