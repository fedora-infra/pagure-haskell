-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Meta module that includes everything except the 'Internal' module.
----------------------------------------------------------------------------
module Web.Pagure (module P) where

import Web.Pagure.Extras as P
import Web.Pagure.Projects as P
import Web.Pagure.Types as P
import Web.Pagure.Types.Issue as P hiding (tags)  -- TODO
import Web.Pagure.Users as P
