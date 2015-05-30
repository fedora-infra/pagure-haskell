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
import Data.Aeson (toJSON)
import Data.Aeson.Lens (key, nth)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (dropWhile, dropWhileEnd)
import Network.Wreq
import Network.Wreq.Types (Postable)
import Web.Pagure.Types

-- | The version of the API we are targetting.
apiVersion :: Int
apiVersion = 0

-- | Construct an API URL path. Strips any preceeding slashes from the given
-- 'String' parameter as well as the '_baseUrl' of the 'PagureConfig'.
pagureUrl :: PagureConfig -> String -> String
pagureUrl (PagureConfig url _) s =
  dropWhileEnd (=='/') url ++ "/api/" ++ show apiVersion ++
  "/" ++ dropWhile (== '/') s

-- | Set up a (possibly authenticated) request to the Pagure API.
pagureWreqOptions :: PagureConfig -> Options
pagureWreqOptions (PagureConfig _ (Just k)) =
  defaults & header "Authorization" .~ [BS.pack k]
pagureWreqOptions _ = defaults

-- | Perform a @GET@ request to the API.
pagureGet :: PagureConfig -> String -> IO (Response BL.ByteString)
pagureGet pc path = getWith (pagureWreqOptions pc) (pagureUrl pc path)

-- | Perform a @POST@ request to the API.
pagurePost
  :: Postable a =>
     PagureConfig -> String -> a -> IO (Response BL.ByteString)
pagurePost pc path = postWith (pagureWreqOptions pc) (pagureUrl pc path)
