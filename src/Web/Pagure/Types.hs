{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure.Types
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Types used within the Pagure API.
----------------------------------------------------------------------------
module Web.Pagure.Types where

import Control.Lens

-- | Describes how to connect to, and authenticate with, the
-- <https://pagure.io/ Pagure> <https://pagure.io/api/0/ API>.
--
-- API keys are used for authenticating to the API. In API version 0, they are
-- apparently project-specific. This means that, short of keeping a local
-- database of all project keys (which expire every 60 days), there is no way of
-- mass-updating your projects. :(
--
-- Keys are obtained by going to your project's Settings page, and clicking the
-- "Get a new Key" link.
data PagureConfig = PagureConfig {
    _baseUrl :: String
  , _apiKey :: Maybe String
  } deriving (Eq, Show)
makeLenses ''PagureConfig
