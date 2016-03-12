{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure.Types.User
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Types for users
----------------------------------------------------------------------------
module Web.Pagure.Types.User where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Web.Pagure.Types
import Prelude

data UserResponse =
  UserResponse { userResponseForks :: [UserRepo]
               , userResponseRepos :: [UserRepo]
               , userResponseUser  :: User
               } deriving (Eq, Show)

data UserRepo =
  UserRepo { userRepoDateCreated :: String
           , userRepoDescription :: String
           , userRepoId :: Integer
           , userRepoName :: String
           , userRepoParent :: Maybe Project
             -- TODO: Uncomment after upstream python type cast error is fixed...
           --, userRepoSettings :: UserRepoSettings
           , userRepoTags :: [Tag]
           , userRepoUser :: User
           } deriving (Eq, Show)

data UserRepoSettings =

  UserRepoSettings { userRepoSettingsMinimumPRScore :: Integer
                   , userRepoSettingsOnlyAssigneeCanMergePR :: Bool
                   , userRepoSettingsWebhooks :: Maybe String
                   , userRepoSettingsIssueTracker :: Bool
                   , userRepoSettingsProjectDocumentation :: Bool
                   , userRepoSettingsProjectPullRequests :: Bool
                   , userRepoSettingsEnforceSignedOffCommitsInPR :: Bool
                   } deriving (Eq, Show)

instance FromJSON UserResponse where
  parseJSON (Object x) = UserResponse <$>
                         x .: "forks"
                     <*> x .: "repos"
                     <*> x .: "user"
  parseJSON _            = mzero

instance FromJSON UserRepo where
  parseJSON (Object x) = UserRepo <$>
                         x .: "date_created"
                     <*> x .: "description"
                     <*> x .: "id"
                     <*> x .: "name"
                     <*> x .: "parent"
                     -- TODO: Uncomment after upstream python type cast error is fixed...
                     -- <*> x .: "settings"
                     <*> x .: "tags"
                     <*> x .: "user"
  parseJSON _            = mzero

instance FromJSON UserRepoSettings where
  parseJSON (Object x) = UserRepoSettings <$>
                         x .: "Minimum_score_to_merge_pull-request"
                     <*> x .: "Only_assignee_can_merge_pull-request"
                     <*> x .: "Web-hooks"
                     <*> x .: "issue_tracker"
                     <*> x .: "project_documentation"
                     <*> x .: "pull_requests"
                     <*> x .: "Enforce_signed-off_commits_in_pull-request"
  parseJSON _            = mzero
