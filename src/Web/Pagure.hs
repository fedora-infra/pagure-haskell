{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Web.Pagure where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Proxy
import qualified Data.Text as T
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import Web.Pagure.Users

-- | A description of pagure API endpoints
type API = UsersAPI

api :: Proxy API
api = Proxy

pagureGroups
  :: Maybe T.Text -- ^ An optional pattern to filter by
  -> Manager
  -> BaseUrl
  -> ClientM GroupsR

pagureUsers
  :: Maybe T.Text -- ^ An optional pattern to filter by
  -> Manager
  -> BaseUrl
  -> ClientM UsersR

pagureGroups :<|> pagureUsers = client api

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
