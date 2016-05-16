{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Pagure.Users where

import Data.Aeson
import Control.Monad (mzero)
import qualified Data.Text as T
import GHC.Generics
import Servant.API
import Servant.Docs hiding (API)

type UsersAPI =
  "api" :> "0" :> "groups" :> QueryParam "pattern" T.Text :> Get '[JSON] GroupsR
  :<|> "api" :> "0" :> "users" :> QueryParam "pattern" T.Text :> Get '[JSON] UsersR

-- | A pagure group. The only thing we get about it from the API is its name.
newtype GroupName = GroupName T.Text deriving (Eq, Generic, Ord, Show)
instance FromJSON GroupName
instance ToJSON GroupName

-- We introduce a convention of having the *response* types end in R.

------------------------------------------------------------
-- groups
------------------------------------------------------------

-- | @/groups@ endpoint response.
data GroupsR = GroupsR {
    groupsRGroups :: [GroupName]
  , groupsRTotalGroups :: Integer
  } deriving (Show, Generic)

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

instance ToParam (QueryParam "pattern" T.Text) where
  toParam _ =
    DocQueryParam "pattern"
                  ["Fed*", "*web*", "*ora", "..."]
                  "An optional pattern to filter by"
                  Normal


------------------------------------------------------------
-- users
------------------------------------------------------------

-- | A pagure username.
newtype Username = Username T.Text deriving (Eq, Generic, Ord, Show)
instance FromJSON Username
instance ToJSON Username

-- | @/users@ endpoint response.
data UsersR = UsersR {
    usersRUsers :: [Username]
  , usersRTotalUsers :: Integer
  } deriving (Show, Generic)

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
