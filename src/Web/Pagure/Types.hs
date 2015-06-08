{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Control.Applicative
import Control.Lens
import Control.Monad (mzero)
import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.Text as T

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
makeLenses ''PagureConfig

type Content = String
type Private = Bool
type Repo = String
type Tag = T.Text
type Title = String
type Username = String

-- | Response type for the 'Web.Pagure.Projects.issues' function.
data IssueResponse =
  IssueResponse { _issueResponseArgs :: IssueArgs
                , _issueResponseIssueList :: [Issue]
                } deriving (Eq, Show)

data IssueArgs =
  IssueArgs { _issueArgsAssignee :: Maybe String
            , _issueArgsAuthor :: Maybe String
            , _issueArgsStatus :: Maybe String
            , _issueArgsTagList  :: [Tag]
            } deriving (Eq, Show)

data Issue =
  Issue { _issueAssignee :: Maybe IssueUser
        , _issueBlocks :: [String]    -- TODO: is [String] correct?
        , _issueComments :: [IssueComment]
        , _issueContent :: String
        , _issueDateCreated :: String
        , _issueDepends :: [String]   -- TODO: is [String] correct?
        , _issueId :: Integer
        , _issuePrivate :: Bool
        , _issueStatus :: String
        , _issueTagList :: [Tag]
        , _issueTitle :: String
        , _issueUser :: IssueUser
        } deriving (Eq, Show)

data IssueUser =
  IssueUser { _issueUserFullname :: String
            , _issueUserName :: String
            } deriving (Eq, Show)

data IssueComment =
  IssueComment { _issueCommentComment :: String
               , _issueCommentDateCreated :: String
               , _issueCommentId :: Integer
               , _issueCommentParent :: Maybe Integer
               , _issueCommentUser :: IssueUser
               } deriving (Eq, Show)

makeFields ''IssueResponse
makeFields ''IssueArgs
makeFields ''Issue
makeFields ''IssueUser
makeFields ''IssueComment

instance FromJSON IssueResponse where
  parseJSON (Object x) = IssueResponse <$>
                         x .: "args"
                     <*> x .: "issues"
  parseJSON _            = mzero

instance FromJSON IssueArgs where
  parseJSON (Object x) = IssueArgs <$>
                         x .: "assignee"
                     <*> x .: "author"
                     <*> x .: "status"
                     <*> x .: "tags"
  parseJSON _            = mzero

instance FromJSON Issue where
  parseJSON (Object x) = Issue <$>
                         x .: "assignee"
                     <*> x .: "blocks"
                     <*> x .: "comments"
                     <*> x .: "content"
                     <*> x .: "date_created"
                     <*> x .: "depends"
                     <*> x .: "id"
                     <*> x .: "private"
                     <*> x .: "status"
                     <*> x .: "tags"
                     <*> x .: "title"
                     <*> x .: "user"
  parseJSON _            = mzero

instance FromJSON IssueUser where
  parseJSON (Object x) = IssueUser <$>
                         x .: "fullname"
                     <*> x .: "name"
  parseJSON _            = mzero

instance FromJSON IssueComment where
  parseJSON (Object x) = IssueComment <$>
                         x .: "comment"
                     <*> x .: "date_created"
                     <*> x .: "id"
                     <*> x .: "parent"
                     <*> x .: "user"
  parseJSON _            = mzero
