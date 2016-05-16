module Main where

import Options.Applicative
import Pagure.CLI.Command

main :: IO ()
main = execParser opts >>= runPagureCli
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "pagure.io command-line interface"
     <> header "git-pagure - interact with pagure.io from the command-line" )
