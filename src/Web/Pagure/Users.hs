{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Pagure.Users where

import Data.Aeson as A
import Data.String (IsString)
import Control.Monad (mzero)
import qualified Data.Text as T
import GHC.Generics
import Servant.API
import Servant.Docs hiding (API)

-- We introduce a convention of having the *response* types end in R.

-- | A pattern to filter out responses in endpoints which permit such filtering.
newtype Pattern = Pattern T.Text deriving (Eq, Generic, IsString, Ord, Show)

instance ToHttpApiData Pattern where toUrlPiece (Pattern a) = a
instance ToParam (QueryParam "pattern" Pattern) where
  toParam _ =
    DocQueryParam "pattern"
                  ["Fed*", "*web*", "*ora", "..."]
                  "An optional pattern to filter by"
                  Normal

------------------------------------------------------------
-- groups
------------------------------------------------------------

-- | A pagure group. The only thing we get about it from the API is its name.
newtype GroupName =
  GroupName { groupNameGroupName :: T.Text }
  deriving (Eq, Generic, IsString, Ord, Show)

instance FromJSON GroupName where
  parseJSON (String x) = return (GroupName x)
  parseJSON _ = mzero

instance ToJSON GroupName where
  toJSON (GroupName x) = A.String x

-- | @/groups@ endpoint response.
data GroupsR = GroupsR {
    groupsRGroups :: [GroupName]
  , groupsRTotalGroups :: Integer
  } deriving (Show)

instance FromJSON GroupsR where
  parseJSON (Object x) =
    GroupsR
    <$> x .: "groups"
    <*> x .: "total_groups"
  parseJSON _ = mzero

instance ToJSON GroupsR where
  toJSON (GroupsR a b) =
    object ["groups" .= a, "total_groups" .= b]

instance ToSample GroupsR where
  toSamples _ = samples [ GroupsR [GroupName "Fedora-Infra"] 1
                        , GroupsR [ GroupName "Fedora-Infra"
                                  , GroupName "fedora-web"
                                  ] 2
                        ]

------------------------------------------------------------
-- users
------------------------------------------------------------

-- | A pagure username.
newtype Username =
  Username { usernameUsername :: T.Text }
  deriving (Eq, Generic, IsString, Ord, Show)

instance FromJSON Username where
  parseJSON (String x) = return (Username x)
  parseJSON _ = mzero

instance ToJSON Username where
  toJSON (Username x) = A.String x

instance ToHttpApiData Username where toUrlPiece (Username a) = a

instance ToCapture (Capture "username" Username) where
  toCapture _ =
    DocCapture "username"
               "The username of the user"

-- | @/users@ endpoint response.
data UsersR = UsersR {
    usersRUsers :: [Username]
  , usersRTotalUsers :: Integer
  } deriving (Show)

instance FromJSON UsersR where
  parseJSON (Object x) =
    UsersR
    <$> x .: "users"
    <*> x .: "total_users"
  parseJSON _ = mzero

instance ToJSON UsersR where
  toJSON (UsersR a b) =
    object ["users" .= a, "total_users" .= b]

instance ToSample UsersR where
  toSamples _ = samples [ UsersR [Username "codeblock"] 1
                        , UsersR [ Username "codelock"
                                 , Username "janedoe"] 2
                        ]

------------------------------------------------------------
-- user
------------------------------------------------------------

-- | @/user/*username*@ endpoint response.
data UserR = UserR {
    userRForks :: [Repo]
  , userRRepos :: [Repo]
  , userRUser  :: User
  } deriving (Show)

instance FromJSON UserR where
  parseJSON (Object x) =
    UserR
    <$> x .: "forks"
    <*> x .: "repos"
    <*> x .: "user"
  parseJSON _ = mzero

instance ToJSON UserR where
  toJSON (UserR a b c) =
    object ["forks" .= a, "repos" .= b, "user" .= c]

newtype UserFullname =
  UserFullname { userFullnameFullname :: T.Text }
  deriving (Eq, Generic, IsString, Ord, Show)

instance FromJSON UserFullname where
  parseJSON (String x) = return (UserFullname x)
  parseJSON _ = mzero

instance ToJSON UserFullname where
  toJSON (UserFullname x) = A.String x

-- | A 'User' in pagure consists of the user\'s fullname and username.
data User = User {
    userFullname :: UserFullname
  , userUsername :: Username
  } deriving (Show)

instance FromJSON User where
  parseJSON (Object x) =
    User
    <$> x .: "fullname"
    <*> x .: "name"
  parseJSON _ = mzero

instance ToJSON User where
  toJSON (User a b) =
    object ["fullname" .= a, "name" .= b]

-- | A repository object used by various endpoints in the API.
--
-- We return types that come directly from the API for now, making no effort to
-- convert them into nicer, more Haskelly types.
data Repo = Repo {
    repoDateCreated :: T.Text
  , repoDescription :: T.Text
  , repoId :: Integer
  , repoName :: T.Text
  , repoParent :: Maybe Repo
  , repoSettings :: RepoSettings
  , repoUser :: User
  } deriving (Show)

instance FromJSON Repo where
  parseJSON (Object x) =
    Repo
    <$> x .: "date_created"
    <*> x .: "description"
    <*> x .: "id"
    <*> x .: "name"
    <*> x .: "parent"
    <*> x .: "settings"
    <*> x .: "user"
  parseJSON _ = mzero

instance ToJSON Repo where
  toJSON (Repo a b c d e f g) =
    object [ "date_created" .= a, "description" .= b, "id" .= c, "name" .= d
           , "parent" .= e, "settings" .= f, "user" .= g ]

-- | Encodes the "settings" dictionary for a repository.
data RepoSettings = RepoSettings {
    repoSettingsMinimumScoreToMergePR :: Integer
  , repoSettingsOnlyAssigneeCanMergePR :: Bool
  , repoSettingsWebHooks :: Maybe T.Text
  , repoSettingsIssueTracker :: Bool
  , repoSettingsProjectDocumentation :: Bool
  , repoSettingsPullRequests :: Bool
  } deriving (Show)

instance FromJSON RepoSettings where
  parseJSON (Object x) =
    RepoSettings
    <$> x .: "Minimum_score_to_merge_pull-request"
    <*> x .: "Only_assignee_can_merge_pull-request"
    <*> x .: "Web-hooks"
    <*> x .: "issue_tracker"
    <*> x .: "project_documentation"
    <*> x .: "pull_requests"
  parseJSON _ = mzero

instance ToJSON RepoSettings where
  toJSON (RepoSettings a b c d e f) =
    object [ "Minimum_score_to_merge_pull-request" .= a
           , "Only_assignee_can_merge_pull-request" .= b, "Web-hooks" .= c
           , "issue_tracker" .= d , "project_documentation" .= e
           , "pull_requests" .= f ]

instance ToSample UserR where
  toSamples _ = singleSample s
    where
      s = UserR []
                [Repo "1426595173"
                      "test description"
                      4
                      "testrepo"
                      Nothing
                      (RepoSettings (-1) False Nothing True False True)
                      (User "Ricky Elrod" "codeblock")]
                (User "Ricky Elrod" "codeblock")
