{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Pagure.Common where

import Control.Monad (mzero)
import Data.Aeson as A
import Data.String (IsString)
import qualified Data.Text as T
import GHC.Generics
import Servant.API
import Servant.Docs hiding (API)

-- | A pattern to filter out responses in endpoints which permit such filtering.
newtype Pattern = Pattern T.Text deriving (Eq, Generic, IsString, Ord, Show)

instance ToHttpApiData Pattern where toUrlPiece (Pattern a) = a
instance ToParam (QueryParam "pattern" Pattern) where
  toParam _ =
    DocQueryParam "pattern"
                  ["Fed*", "*web*", "*ora", "..."]
                  "An optional pattern to filter by"
                  Normal

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

-- | A pagure repository name.
newtype RepoName =
  RepoName { repoNameRepoName :: T.Text }
  deriving (Eq, Generic, IsString, Ord, Show)

instance FromJSON RepoName where
  parseJSON (String x) = return (RepoName x)
  parseJSON _ = mzero

instance ToJSON RepoName where
  toJSON (RepoName x) = A.String x

instance ToHttpApiData RepoName where toUrlPiece (RepoName a) = a

instance ToCapture (Capture "repo" RepoName) where
  toCapture _ =
    DocCapture "repo"
               "The name of the repository"
