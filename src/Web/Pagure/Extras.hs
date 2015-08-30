{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure.Extras
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Access to the \"Extras\" endpoints of the Pagure API.
----------------------------------------------------------------------------
module Web.Pagure.Extras where

import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Network.Wreq
import Web.Pagure.Internal.Wreq
import Web.Pagure.Types

-- | Access the @/version@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT version pc
-- Response {responseStatus = Status {statusCode = 200, [...]
-- @
version :: PagureT BL.ByteString
version = do
  resp <- pagureGet "/version"
  return (resp ^. responseBody)

-- | Access the @/[repo]/tags@ endpoint.
--
-- Takes an optional @pattern@ that (according to the API documentation)
-- "filters the starting letters of the tags."
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (tags "pagure" Nothing) pc
-- ["0.1","0.2","Artwork","doc","easyfix","email","wishful"]
-- @
tags ::
  Repo
  -> Maybe T.Text -- ^ An optional @pattern@.
  -> PagureT [Tag]
tags r pattern = do
  opts <- pagureWreqOptions
  let opts' = case pattern of
        Just p  -> opts & param "pattern" .~ [p]
        Nothing -> opts
  resp <- pagureGetWith opts' (r ++ "/tags")
  return $ resp ^.. responseBody . key "tags" . values . _String

-- | Access the @/error_codes@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT errorCodes pc
-- fromList [("ENOREQ","Pull-Request not found"),("ENOISSUE[...]
-- @
--
-- Of course, you can uses lenses to traverse the 'HM.HashMap' as usual.
errorCodes :: PagureT (HM.HashMap T.Text T.Text)
errorCodes = do
  resp <- pagureGet "/error_codes"
  return $ resp ^. responseBody . _Object . below _String
