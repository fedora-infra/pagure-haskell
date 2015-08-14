{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure.Types.Project
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- The 'Project' type.
----------------------------------------------------------------------------
module Web.Pagure.Types.Project where

import Control.Monad (mzero)
import Data.Aeson
import Web.Pagure.Types

data PullRequest =
  PullRequest { pullRequestAssignee :: Maybe String
              , pullRequestBranch :: String
              , pullRequestBranchFrom :: String
              , pullRequestClosedBy :: User
              , pullRequestComments :: [PullRequestComment]
              , pullRequestCommitStart :: Maybe String
              , pullRequestCommitStop :: Maybe String
              , pullRequestDateCreated :: String -- NOTE: Yes, it's a String...
              , pullRequestId :: Integer
              , pullRequestProject :: Project
              , pullRequestRepoFrom :: Project
              , pullRequestStatus :: String -- TODO: API docs say it's a bool?
              , pullRequestTitle :: String
              , pullRequestUid :: String -- NOTE: Yes, it's a String, not an Integer...
              , pullRequestUser :: User
              } deriving (Eq, Show)

data PullRequestComment =
  PullRequestComment { pullRequestCommentComment :: String
                     , pullRequestCommentCommit :: Maybe String
                     , pullRequestCommentDateCreated :: String -- NOTE: Yes, it's a String...
                     , pullRequestCommentFilename :: Maybe String
                     , pullRequestCommentId :: Integer
                     , pullRequestCommentLine :: Maybe Integer
                     , pullRequestCommentParent :: Maybe PullRequestComment
                     , pullRequestCommentUser :: User
                     } deriving (Eq, Show)

instance FromJSON PullRequest where
  parseJSON (Object x) = PullRequest <$>
                         x .: "assignee"
                     <*> x .: "branch"
                     <*> x .: "branch_from"
                     <*> x .: "closed_by"
                     <*> x .: "comments"
                     <*> x .: "commit_start"
                     <*> x .: "commit_stop"
                     <*> x .: "date_created"
                     <*> x .: "id"
                     <*> x .: "project"
                     <*> x .: "repo_from"
                     <*> x .: "status"
                     <*> x .: "title"
                     <*> x .: "uid"
                     <*> x .: "user"
  parseJSON _            = mzero

instance FromJSON PullRequestComment where
  parseJSON (Object x) = PullRequestComment <$>
                         x .: "comment"
                     <*> x .: "commit"
                     <*> x .: "date_created"
                     <*> x .: "filename"
                     <*> x .: "id"
                     <*> x .: "line"
                     <*> x .: "parent"
                     <*> x .: "user"
  parseJSON _            = mzero


-- TODO: Write this for status above.
--data IssueStatus = Open | Invalid | InsufficientData | Fixed deriving (Eq, Show)
--
--issueStatusToAPI :: IssueStatus -> String
--issueStatusToAPI Open = "Open"
--issueStatusToAPI Invalid = "Invalid"
--issueStatusToAPI InsufficientData = "Insufficient data"
--issueStatusToAPI Fixed = "Fixed"
