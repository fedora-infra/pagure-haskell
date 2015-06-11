{-# LANGUAGE OverloadedStrings #-}
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
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Network.Wreq
import Web.Pagure.Internal.Wreq
import Web.Pagure.Types
import Web.Pagure.Types.Issue

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
  resp <- pagureGet ("fork/" ++ T.unpack u ++ "/" ++ r ++ "/git/tags")
  return $ resp ^.. responseBody . key "tags" . values . _String

-- | Access the @/[repo]/issues@ endpoint.
--
-- Traversing the response is particularly nice because we define lenses for all
-- of the fields you get back. See the example below.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> issues <- runPagureT (issues "pagure") pc
-- >>> λ> issues ^. issueList . to head . comments . to head . comment
-- "wat?"
-- @
issues :: Repo -> IssueFilters -> PagureT IssueResponse
issues r filters = do
  opts <- pagureWreqOptions
  let opts' = opts & param "status" .~ [filters ^. status]
                   & param "tags" .~ filters ^. tags . non []
                   & param "assignee" .~ filters ^. assignee . to maybeToList
                   & param "author" .~ filters ^. author . to maybeToList
  resp <- asJSON =<< pagureGetWith opts' (r ++ "/issues")
  return (resp ^. responseBody)

-- | Access the @/fork/[username]/[repo]/issues@ endpoint.
--
-- Traversing the response is particularly nice because we define lenses for all
-- of the fields you get back. See the example below.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> issues <- runPagureT (issuesFork "codeblock" "pagure") pc
-- >>> λ> issues ^. issueList . to head . comments . to head . comment
-- "wat?"
-- @
issuesFork :: Username -> Repo -> IssueFilters -> PagureT IssueResponse
issuesFork u r filters = do
  opts <- pagureWreqOptions
  let opts' = opts & param "status" .~ [filters ^. status]
                   & param "tags" .~ filters ^. tags . non []
                   & param "assignee" .~ filters ^. assignee . to maybeToList
                   & param "author" .~ filters ^. author . to maybeToList
  resp <- asJSON =<<
          pagureGetWith opts' ("fork/" ++ T.unpack u ++ "/" ++ r ++ "/issues")
  return (resp ^. responseBody)

-- | Access the @/[repo]/new_issue@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> issues <- runPagureT (newIssue "pagure-haskell" "Test" "ignore" Open False) pc
-- @
newIssue ::
  Repo
  -> Title
  -> Content
  -> IssueStatus
  -> Private
  -> PagureT (Maybe T.Text)
newIssue r t c s p = do
  -- < pingou> so if the issue is private just put private=foo else don't
  --           provide it to the request
  resp <- pagurePost (r ++ "/new_issue") (["title" := t
                                          ,"issue_content" := c
                                          ,"status" := issueStatusToAPI s] ++
                                          if p
                                          then ["private" := T.pack "true"]
                                          else [])
  return (resp ^? responseBody . key "message" . _String)
