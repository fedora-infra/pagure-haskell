{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure.PullRequests
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Access to the \"Pull Requests\" endpoints of the Pagure API.
----------------------------------------------------------------------------
module Web.Pagure.PullRequests where

import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T
import Network.Wreq
import Web.Pagure.Internal.Wreq
import Web.Pagure.Types
import Web.Pagure.Types.Project

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
-- Just (PullRequest {pullRequestAssignee = Nothing, pullRequestBranch = [...]
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
-- Just (PullRequest {pullRequestAssignee = Nothing, pullRequestBranch = [...]
-- @
pullRequestFork :: Username -> Repo -> PullRequestId -> PagureT (Maybe PullRequest)
pullRequestFork u r = pullRequest ("fork/" ++ T.unpack u ++ "/" ++ r)

-- | Access the @/[repo]/pull-request/[request id]/comment@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (commentPullRequest "pagure" 244) pc
-- @
commentPullRequest ::
  Repo
  -> PullRequestId
  -> PagureT (Maybe PullRequest)
commentPullRequest r pr = do
  resp <- asJSON =<< pagureGet (r ++ "/pull-request/" ++ show pr)
  return (resp ^. responseBody)

-- | Access the @/fork/[username]/[repo]/pull-request/[request id]/comment@ endpoint.
--
-- Example:
--
-- @
-- >>> import Web.Pagure
-- >>> let pc = PagureConfig "https://pagure.io" Nothing
-- >>> runPagureT (commrntPullRequestFork "pagure" 244) pc
-- @
commentPullRequestFork ::
  Username
  -> Repo
  -> PullRequestId
  -> PagureT (Maybe PullRequest)
commentPullRequestFork u r = commentPullRequest ("fork/" ++ T.unpack u ++ "/" ++ r)
