{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure.Issues
-- Copyright : (C) 2015 Red Hat, Inc.
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Access to the \"Issues\" endpoints of the Pagure API.
----------------------------------------------------------------------------
module Web.Pagure.Issues where

import Control.Lens
import Data.Aeson.Lens
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Network.Wreq
import Web.Pagure.Internal.Wreq
import Web.Pagure.Lens
import Web.Pagure.Types
import Web.Pagure.Types.Issue

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
                                          ["private" := T.pack "true" | p])
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

-- | Access the @/[repo]/issue/[issue id]@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (issue "pagure" 4) pc
-- @
issue :: Repo -> IssueId -> PagureT Issue
issue r i = do
  resp <- asJSON =<< pagureGet (r ++ "/issue/" ++ show i)
  return (resp ^. responseBody)

-- | Access the @/fork/[username]/[repo]/issue/[issue id]@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (issueFork "codeblock" "pagure" 4) pc
-- @
issueFork :: Username -> Repo -> IssueId -> PagureT Issue
issueFork u r = issue ("fork/" ++ T.unpack u ++ "/" ++ r)

-- | Access the @/[repo]/issue/[issue id]/comment/[comment id]@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (issueComment "pagure" 262 3) pc
-- @
issueComment :: Repo -> IssueId -> CommentId -> PagureT (Maybe IssueComment)
issueComment r i c = do
  resp <- asJSON =<< pagureGet (r ++ "/issue/" ++ show i ++ "/comment/" ++ show c)
  return (resp ^. responseBody)

-- | Access the @/fork/[repo]/issue/[issue id]/comment/[comment id]@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (issueCommentFork "codeblock" "pagure" 262 3) pc
-- @
issueCommentFork ::
  Username
  -> Repo
  -> IssueId
  -> CommentId
  -> PagureT (Maybe IssueComment)
issueCommentFork u r = issueComment ("fork/" ++ T.unpack u ++ "/" ++ r)

-- | Access the @/[repo]/issue/[issue id]/comment@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" "key here"
-- >>> runPagureT (newIssueComment "test" 1 "This is a test comment. Woot.") pc
-- @
newIssueComment ::
  Repo
  -> IssueId
  -> Comment
  -> PagureT (Maybe T.Text)
newIssueComment r i c = do
  resp <- pagurePost (r ++ "/issue/" ++ show i ++ "/comment") ["comment" := c]
  return (resp ^? responseBody . key "message" . _String)

-- | Access the @/fork/[username]/[repo]/issue/[issue id]/comment@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" "key here"
-- >>> runPagureT (newIssueCommentFork "codeblock" "test" 1 "This is a test comment. Woot.") pc
-- @
newIssueCommentFork ::
  Username
  -> Repo
  -> IssueId
  -> Comment
  -> PagureT (Maybe T.Text)
newIssueCommentFork u r = newIssueComment ("fork/" ++ T.unpack u ++ "/" ++ r)
