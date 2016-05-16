module Pagure.CLI.Command where

import Options.Applicative
import Web.Pagure

import Pagure.CLI.Command.Groups
import Pagure.CLI.Command.User
import Pagure.CLI.Command.Users

-- | Every command has a constructor in this type. It can have an optional field
-- which should contain the command-line options specific to that command.
data Command =
    Groups GroupsOptions
  | User UserOptions
  | Users UsersOptions
  deriving (Eq, Ord, Show)

-- | Every command should also have its relevant @command@ (defined below)
-- referenced here.
options :: Parser GlobalOptions
options = subparser $
          groupsCommand
       <> userCommand
       <> usersCommand

-- | Every command should also have a relevant function that actually does what
-- the command intends to do. That function should be referenced here. This
-- function should always be total vis-a-vis 'Command' above.
runPagureCli :: GlobalOptions -> IO ()
runPagureCli g@(GlobalOptions cmd verbose) =
  case cmd of
    Groups opts   -> groupsCommandRunner opts
    User opts     -> userCommandRunner opts
    Users opts    -> usersCommandRunner opts


--------------------------------------------------------------------------------
-- Global options, available to all commands.
--------------------------------------------------------------------------------

-- | Options available to all @pg@ commands.
data GlobalOptions =
  GlobalOptions {
    globalOptionsCommand :: Command
  , globalOptionsVerbose :: Bool
  } deriving (Eq, Ord, Show)

globalOptions :: Parser Command -> Parser GlobalOptions
globalOptions cmd = GlobalOptions
                    <$> cmd
                    <*> switch (long "verbose"
                             <> help "Output debugging information")

--------------------------------------------------------------------------------
-- Command: gropups
--------------------------------------------------------------------------------

groupsCommandParser :: Parser Command
groupsCommandParser = Groups <$> (GroupsOptions <$> optional (strOption (
                                                   long "pattern"
                                                <> short 'p'
                                                <> metavar "PATTERN"
                                                <> help "An optional beginning pattern to filter by")))

groupsCommand :: Mod CommandFields GlobalOptions
groupsCommand =
  command "groups" (info (helper <*> globalOptions groupsCommandParser) $
                   fullDesc
                <> progDesc "Retieve a list of pagure groups")


--------------------------------------------------------------------------------
-- Command: user
--------------------------------------------------------------------------------

userCommandParser :: Parser Command
userCommandParser = User <$> (UserOptions <$> argument str (
                                                metavar "USERNAME"
                                             <> help "The username to look up"))

userCommand :: Mod CommandFields GlobalOptions
userCommand =
  command "user" (info (helper <*> globalOptions userCommandParser) $
                   fullDesc
                <> progDesc "Show information about a pagure user")


--------------------------------------------------------------------------------
-- Command: users
--------------------------------------------------------------------------------

usersCommandParser :: Parser Command
usersCommandParser = Users <$> (UsersOptions <$> optional (strOption (
                                                   long "pattern"
                                                <> short 'p'
                                                <> metavar "PATTERN"
                                                <> help "An optional beginning pattern to filter by")))

usersCommand :: Mod CommandFields GlobalOptions
usersCommand =
  command "users" (info (helper <*> globalOptions usersCommandParser) $
                   fullDesc
                <> progDesc "Retieve a list of pagure users")
