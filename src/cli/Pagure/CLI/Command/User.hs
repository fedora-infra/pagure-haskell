module Pagure.CLI.Command.User where

import Control.Lens
import qualified Data.Text as T
import Web.Pagure
import Web.Pagure.Lens
import Web.Pagure.Users

data UserOptions =
  UserOptions { userOptionsUsername :: String } deriving (Eq, Ord, Show)

userCommandRunner :: UserOptions -> IO ()
userCommandRunner (UserOptions un) = do
  -- TODO: unhardcode
  userResp <- prod (pagureUser (Username (T.pack un)))
  case userResp of
    Left err -> error $ "Unable to retrieve information for `" ++ un ++ ": `"
    Right userRes -> do
      putStrLn $ "User      : " ++ userRes ^. user . username . username . to T.unpack
      putStrLn $ "Full Name : " ++ userRes ^. user . fullname . fullname . to T.unpack
      let repos' = userRes ^. repos
          forks' = userRes ^. forks
      putStrLn $ "# Repos   : " ++ show (length repos')
      putStrLn $ "# Forks   : " ++ show (length forks')
