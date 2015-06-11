{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure.Lens
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Lenses for the various Pagure types. We generate them all here so that we
-- can take advantage of typeclass method overloading hackery put in place by
-- 'makeFields'.
----------------------------------------------------------------------------
module Web.Pagure.Lens where

import Control.Lens

import Web.Pagure.Types
import Web.Pagure.Types.Issue

-- Types
makeLenses ''PagureConfig

-- Issues
makeFields ''IssueResponse
makeFields ''IssueArgs
makeFields ''Issue
makeFields ''IssueUser
makeFields ''IssueComment
makeFields ''IssueFilters
