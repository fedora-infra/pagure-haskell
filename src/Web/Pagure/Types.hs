{-# LANGUAGE OverloadedStrings #-}
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

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Default
import qualified Data.Text as T
import Prelude

-- | Our 'PagureT' type which is really a 'ReaderT' with 'IO' as its base. For
-- now, at least.
type PagureT a = ReaderT PagureConfig IO a

-- | Run the whole stack.
runPagureT :: PagureT a -> PagureConfig -> IO a
runPagureT = runReaderT

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

-- | Default to <https://pagure.io>, unauthenticated.
instance Default PagureConfig where
  def = PagureConfig "https://pagure.io" Nothing

-- | Used in several API endpoint responses.
data User =
  User { userFullname :: String
       , userName :: String
       } deriving (Eq, Show)

instance FromJSON User where
  parseJSON (Object x) = User <$>
                         x .: "fullname"
                     <*> x .: "name"
  parseJSON _            = mzero

-- | Used in several API endpoint responses.
data Project =
  Project { projectDateCreated :: String
          , projectDescription :: String
          , projectId :: Integer
          , projectName :: String
          , projectParent :: Maybe String -- TODO: is it really a String?
          , projectUser :: User
          } deriving (Eq, Show)

instance FromJSON Project where
  parseJSON (Object x) = Project <$>
                         x .: "date_created"
                     <*> x .: "description"
                     <*> x .: "id"
                     <*> x .: "name"
                     <*> x .: "parent"
                     <*> x .: "user"
  parseJSON _            = mzero

type Comment = T.Text
type Content = String
type Group = T.Text
type IssueId = Integer
type Private = Bool
type PullRequestId = Integer
type Repo = String
type Tag = T.Text
type Title = String
type Username = T.Text
