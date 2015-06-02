{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure.Projects
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Access to the \"Projects\" endpoints of the Pagure API.
----------------------------------------------------------------------------
module Web.Pagure.Projects where

import Control.Lens
import Data.Aeson.Lens
import Network.Wreq
import Web.Pagure.Internal.Wreq
import Web.Pagure.Types

-- | Access the @/[repo]/git/tags@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (gitTags "pagure") pc
-- ["0.1","0.1.1","0.1.10","0.1.2","0.1.3","0.1.4","0.1.5",[...]
-- @
gitTags :: Repo -> PagureT [Tag]
gitTags r = do
  resp <- pagureGet (r ++ "/git/tags")
  return $ resp ^.. responseBody . key "tags" . values . _String

-- | Access the @/[repo]/git/tags@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (gitTagsFork "codeblock" "pagure") pc
-- ["0.1","0.1.1","0.1.10","0.1.2","0.1.3","0.1.4","0.1.5",[...]
-- @
gitTagsFork :: Username -> Repo -> PagureT [Tag]
gitTagsFork u r = do
  resp <- pagureGet ("fork/" ++ u ++ "/" ++ r ++ "/git/tags")
  return $ resp ^.. responseBody . key "tags" . values . _String
