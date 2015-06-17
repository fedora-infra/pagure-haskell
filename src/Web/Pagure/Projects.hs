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
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Maybe (maybeToList)
import Data.Monoid
import qualified Data.Text as T
import Network.Wreq
import Web.Pagure.Internal.Wreq
import Web.Pagure.Lens
import Web.Pagure.Types
import Web.Pagure.Types.Issue
import Web.Pagure.Types.Project

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

-- | Access the @/fork/[username]/[repo]/git/tags@ endpoint.
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
gitTagsFork u r = gitTags ("fork/" ++ T.unpack u ++ "/" ++ r)

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
issuesFork u r = issues ("fork/" ++ T.unpack u ++ "/" ++ r)

-- | Access the @/[repo]/new_issue@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (newIssue "pagure-haskell" "Test" "ignore" Open False) pc
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

-- | Access the @/fork/[user]/[repo]/new_issue@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (newIssueFork "codeblock" "pagure" "Test" "ignore" Open False) pc
-- @
newIssueFork ::
  Username
  -> Repo
  -> Title
  -> Content
  -> IssueStatus
  -> Private
  -> PagureT (Maybe T.Text)
newIssueFork u r = newIssue ("fork/" ++ T.unpack u ++ "/" ++ r)

-- | Access the @/[repo]/pull-request/[request id]/merge@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (mergePullRequest "pagure-haskell" 123) pc
-- @
mergePullRequest :: Repo -> PullRequestId -> PagureT (Maybe T.Text)
mergePullRequest r pr = do
  resp <- pagurePost (r ++ "/pull-request/" ++ show pr ++ "/merge")
          (mempty :: C8.ByteString)
  return (resp ^? responseBody . key "message" . _String)

-- | Access the @/fork/[user]/[repo]/pull-request/[request id]/merge@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (mergePullRequestFork "relrod" "pagure-haskell" 123) pc
-- @
mergePullRequestFork :: Username -> Repo -> PullRequestId -> PagureT (Maybe T.Text)
mergePullRequestFork u r = mergePullRequest ("fork/" ++ T.unpack u ++ "/" ++ r)

-- | Access the @/[repo]/pull-request/[request id]/close@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (closePullRequest "pagure-haskell" 123) pc
-- @
closePullRequest :: Repo -> PullRequestId -> PagureT (Maybe T.Text)
closePullRequest r pr = do
  resp <- pagurePost (r ++ "/pull-request/" ++ show pr ++ "/close")
          (mempty :: C8.ByteString)
  return (resp ^? responseBody . key "message" . _String)

-- | Access the @/fork/[user]/[repo]/pull-request/[request id]/close@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (mergePullRequest "pagure-haskell" 123) pc
-- @
closePullRequestFork :: Username -> Repo -> PullRequestId -> PagureT (Maybe T.Text)
closePullRequestFork u r = closePullRequest ("fork/" ++ T.unpack u ++ "/" ++ r)

-- | Access the @/[repo]/pull-request/[request id]@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (pullRequest "pagure" 244) pc
-- Just (PullRequest {pullRequestAssignee = Nothing, pullRequestBranch = [..]
-- @
pullRequest :: Repo -> PullRequestId -> PagureT (Maybe PullRequest)
pullRequest r pr = do
  resp <- asJSON =<< pagureGet (r ++ "/pull-request/" ++ show pr)
  return (resp ^. responseBody)


-- | Access the @/fork/[user]/[repo]/pull-request/[request id]@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (pullRequestFork "relrod" "pagure" 244) pc
-- Just (PullRequest {pullRequestAssignee = Nothing, pullRequestBranch = [..]
-- @
pullRequestFork :: Username -> Repo -> PullRequestId -> PagureT (Maybe PullRequest)
pullRequestFork u r = pullRequest ("fork/" ++ T.unpack u ++ "/" ++ r)
