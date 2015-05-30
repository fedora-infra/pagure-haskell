{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure.Internal.Wreq
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Low-level access to the Pagure API
----------------------------------------------------------------------------
module Web.Pagure.Internal.Wreq where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
--import Data.Aeson (toJSON)
--import Data.Aeson.Lens (key, nth)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (dropWhileEnd)
import Network.Wreq
import Network.Wreq.Types (Postable)
import Web.Pagure.Types

-- | The version of the API we are targetting.
apiVersion :: Int
apiVersion = 0

-- | Construct an API URL path. Strips any preceeding slashes from the given
-- 'String' parameter as well as the '_baseUrl' of the 'PagureConfig'.
pagureUrl :: String -> PagureT String
pagureUrl s = do
  (PagureConfig url _) <- ask
  return $ dropWhileEnd (=='/') url ++ "/api/" ++ show apiVersion ++
    "/" ++ dropWhile (== '/') s

-- | Set up a (possibly authenticated) request to the Pagure API.
pagureWreqOptions :: PagureT Options
pagureWreqOptions  = do
  pc <- ask
  return $ case pc of
    PagureConfig _ (Just k) ->
      defaults & header "Authorization" .~ [BS.pack k]
    _ -> defaults

-- | Perform a @GET@ request to the API.
pagureGet :: String -> PagureT (Response BL.ByteString)
pagureGet path = do
  opts <- pagureWreqOptions
  path' <- pagureUrl path
  liftIO $ getWith opts path'

-- | Perform a @POST@ request to the API.
pagurePost :: Postable a => String -> a -> PagureT (Response BL.ByteString)
pagurePost path a = do
  opts <- pagureWreqOptions
  path' <- pagureUrl path
  liftIO $ postWith opts path' a
