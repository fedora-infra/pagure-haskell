{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad (mzero)
import Data.Aeson
import Data.Default
import qualified Data.Text as T
import Web.Pagure.Types
import Prelude

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
  Issue { issueAssignee :: Maybe User
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
        , issueUser :: User
        } deriving (Eq, Show)

-- | When requesting a particular comment, we get a different structure than
-- what we'd get if we were requesting an issue and looking at its comments.
-- For now, we ignore the extra fields. At some point, it would be good to unify
-- the two responses upstream.
data IssueComment =
  IssueComment { issueCommentComment :: String
               , issueCommentDateCreated :: String
               , issueCommentId :: Integer
               , issueCommentParent :: Maybe Integer
               , issueCommentUser :: User
               } deriving (Eq, Show)

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

instance Default IssueFilters where
  def = IssueFilters "Open" Nothing Nothing Nothing

data IssueStatus = Open | Invalid | InsufficientData | Fixed deriving (Eq, Show)

issueStatusToAPI :: IssueStatus -> String
issueStatusToAPI Open = "Open"
issueStatusToAPI Invalid = "Invalid"
issueStatusToAPI InsufficientData = "Insufficient data"
issueStatusToAPI Fixed = "Fixed"
