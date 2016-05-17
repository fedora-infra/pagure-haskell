{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Pagure.Extras where

import Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Control.Monad (mzero)
import qualified Data.Text as T
import Servant.Docs hiding (API)

------------------------------------------------------------
-- version
------------------------------------------------------------

-- | @/version@ endpoint response.
newtype VersionR = VersionR {
    versionRVersion :: T.Text
  } deriving (Show)

instance FromJSON VersionR where
  parseJSON (Object x) = VersionR <$> x .: "version"
  parseJSON _ = mzero

instance ToJSON VersionR where
  toJSON (VersionR a) = object ["version" .= a]

instance ToSample VersionR where
  toSamples _ = singleSample (VersionR "0.6")


------------------------------------------------------------
-- error_codes
------------------------------------------------------------

-- | @/error_codes@ endpoint response.
data ErrorCodesR = ErrorCodesR {
    errorCodesRErrorCodes :: HM.HashMap T.Text T.Text
  } deriving (Show)

instance FromJSON ErrorCodesR where
  parseJSON o@(Object _) = ErrorCodesR <$> parseJSON o
  parseJSON _ = mzero

instance ToJSON ErrorCodesR where
  toJSON (ErrorCodesR a) = toJSON a

instance ToSample ErrorCodesR where
  toSamples _ =
    singleSample . ErrorCodesR $ HM.fromList
      [ ("EDBERROR", "An error occured at the database level and prevent the action from reaching completion")
      , ("EINVALIDREQ", "Invalid or incomplete input submited")
      , ("EINVALIDTOK", "Invalid or expired token. Please visit https://pagure.io/ to get or renew your API token.")
      , ("EISSUENOTALLOWED", "You are not allowed to view this issue")
      , ("ENOCODE", "Variable message describing the issue")
      , ("ENOCOMMENT", "Comment not found")
      , ("ENOISSUE", "Issue not found")
      , ("ENOPRCLOSE", "You are not allowed to merge/close pull-request for this project")
      , ("ENOPROJECT", "Project not found")
      , ("ENOPROJECTS", "No projects found")
      , ("ENOREQ", "Pull-Request not found")
      , ("ENOTASSIGNED", "This request must be assigned to be merged")
      , ("ENOTASSIGNEE", "Only the assignee can merge this review")
      , ("ENOUSER", "No such user found")
      , ("EPRSCORE", "This request does not have the minimum review score necessary to be merged")
      , ("EPULLREQUESTSDISABLED", "Pull-Request have been deactivated for this project")
      , ("ETRACKERDISABLED", "Issue tracker disabled for this project")
      ]


------------------------------------------------------------
-- tags
------------------------------------------------------------

-- | @/tags@ endpoint response.
data TagsR = TagsR {
    tagsRTags :: [T.Text]
  , tagsRTotalTags :: Integer
  } deriving (Show)

instance FromJSON TagsR where
  parseJSON (Object x) = TagsR <$> x .: "tags"
                               <*> x .: "total_tags"
  parseJSON _ = mzero

instance ToJSON TagsR where
  toJSON (TagsR a b) = object ["tags" .= a, "total_tags" .= b]

instance ToSample TagsR where
  toSamples _ = singleSample (TagsR ["tag1", "tag2"] 2)
