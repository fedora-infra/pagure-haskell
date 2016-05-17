{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Web.Pagure where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Proxy
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import Web.Pagure.Common
import Web.Pagure.Extras
import Web.Pagure.Users

-- | A description of pagure API endpoints
type API =
  -- Users
  "api" :> "0" :> "groups" :> QueryParam "pattern" Pattern :> Get '[JSON] GroupsR
  :<|> "api" :> "0" :> "users" :> QueryParam "pattern" Pattern :> Get '[JSON] UsersR
  :<|> "api" :> "0" :> "user" :> Capture "username" Username :> Get '[JSON] UserR
  -- Extras
  :<|> "api" :> "0" :> "version" :> Get '[JSON] VersionR
  :<|> "api" :> "0" :> "error_codes" :> Get '[JSON] ErrorCodesR
  :<|> "api" :> "0" :> Capture "repo" RepoName :> "tags" :> Get '[JSON] TagsR
  :<|> "api" :> "0" :> "fork" :> Capture "username" Username :> Capture "repo" RepoName :> "tags" :> Get '[JSON] TagsR


api :: Proxy API
api = Proxy

pagureGroups
  :: Maybe Pattern -- ^ An optional pattern to filter by
  -> Manager
  -> BaseUrl
  -> ClientM GroupsR

pagureUsers
  :: Maybe Pattern -- ^ An optional pattern to filter by
  -> Manager
  -> BaseUrl
  -> ClientM UsersR

pagureUser
  :: Username -- ^ The user to get information for
  -> Manager
  -> BaseUrl
  -> ClientM UserR

pagureVersion
  :: Manager
  -> BaseUrl
  -> ClientM VersionR

pagureErrorCodes
  :: Manager
  -> BaseUrl
  -> ClientM ErrorCodesR

pagureTags
  :: RepoName -- ^ The repository to get information for
  -> Manager
  -> BaseUrl
  -> ClientM TagsR

pagureTagsFork
  :: Username -- ^ The owner of the repository
  -> RepoName -- ^ The repository to get information for
  -> Manager
  -> BaseUrl
  -> ClientM TagsR

pagureGroups
  :<|> pagureUsers
  :<|> pagureUser
  :<|> pagureVersion
  :<|> pagureErrorCodes
  :<|> pagureTags
  :<|> pagureTagsFork = client api

-- | Run a query against the PRODUCTION pagure instance.
prod :: (Manager -> BaseUrl -> ExceptT e IO a) -> IO (Either e a)
prod endpoint = do
  manager <- newManager tlsManagerSettings
  runExceptT (endpoint manager (BaseUrl Https "pagure.io" 443 ""))

-- | Run a query against the STAGING pagure instance.
staging :: (Manager -> BaseUrl -> ExceptT e IO a) -> IO (Either e a)
staging endpoint = do
  manager <- newManager tlsManagerSettings
  runExceptT (endpoint manager (BaseUrl Https "stg.pagure.io" 443 ""))
