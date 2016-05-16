module Pagure.CLI.Command.Users where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Web.Pagure
import Web.Pagure.Lens
import Web.Pagure.Users

data UsersOptions =
  UsersOptions { usersOptionsPattern :: Maybe String } deriving (Eq, Ord, Show)

usersCommandRunner :: UsersOptions -> IO ()
usersCommandRunner (UsersOptions pattern) = do
  usersResp <- prod (pagureUsers (fmap (Pattern . T.pack) pattern))
  case usersResp of
    Left err -> error "Unable to retrieve list of users."
    Right usersRes -> print usersRes
