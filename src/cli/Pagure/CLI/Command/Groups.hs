module Pagure.CLI.Command.Groups where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Web.Pagure
import Web.Pagure.Lens
import Web.Pagure.Users

data GroupsOptions =
  GroupsOptions { groupsOptionsPattern :: Maybe String } deriving (Eq, Ord, Show)

groupsCommandRunner :: GroupsOptions -> IO ()
groupsCommandRunner (GroupsOptions pattern) = do
  groupsResp <- prod (pagureGroups (fmap (Pattern . T.pack) pattern))
  case groupsResp of
    Left err -> error  "Unable to retrieve list of groups."
    Right groupsRes -> print groupsRes
