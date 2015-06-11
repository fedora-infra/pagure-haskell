{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure.Types.Issue
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Types for project issues, used within the Pagure API.
----------------------------------------------------------------------------
module Web.Pagure.Types.Issue where

import Control.Applicative
import Control.Lens
import Control.Monad (mzero)
import Data.Aeson
import Data.Default
import qualified Data.Text as T
import Web.Pagure.Types

-- | Response type for the 'Web.Pagure.Projects.issues' function.
data IssueResponse =
  IssueResponse { issueResponseArgs :: IssueArgs
                , issueResponseIssueList :: [Issue]
                } deriving (Eq, Show)

data IssueArgs =
  IssueArgs { issueArgsAssignee :: Maybe String
            , issueArgsAuthor :: Maybe String
            , issueArgsStatus :: Maybe String
            , issueArgsTagList  :: [Tag]
            } deriving (Eq, Show)

data Issue =
  Issue { issueAssignee :: Maybe IssueUser
        , issueBlocks :: [String]    -- TODO: is [String] correct?
        , issueComments :: [IssueComment]
        , issueContent :: String
        , issueDateCreated :: String
        , issueDepends :: [String]   -- TODO: is [String] correct?
        , issueId :: Integer
        , issuePrivate :: Bool
        , issueStatus :: String
        , issueTagList :: [Tag]
        , issueTitle :: String
        , issueUser :: IssueUser
        } deriving (Eq, Show)

data IssueUser =
  IssueUser { issueUserFullname :: String
            , issueUserName :: String
            } deriving (Eq, Show)

data IssueComment =
  IssueComment { issueCommentComment :: String
               , issueCommentDateCreated :: String
               , issueCommentId :: Integer
               , issueCommentParent :: Maybe Integer
               , issueCommentUser :: IssueUser
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

-- | Filters to query issues for a given project.
data IssueFilters =
  IssueFilters { issueFiltersStatus :: T.Text
               , issueFiltersTags :: Maybe [Tag]
               , issueFiltersAssignee :: Maybe Username  -- TODO: Are these correct?
               , issueFiltersAuthor :: Maybe Username
               } deriving (Eq, Show)
makeFields ''IssueFilters

instance Default IssueFilters where
  def = IssueFilters "Open" Nothing Nothing Nothing

data IssueStatus = Open | Invalid | InsufficientData | Fixed deriving (Eq, Show)

issueStatusToAPI :: IssueStatus -> String
issueStatusToAPI Open = "Open"
issueStatusToAPI Invalid = "Invalid"
issueStatusToAPI InsufficientData = "Insufficient data"
issueStatusToAPI Fixed = "Fixed"
