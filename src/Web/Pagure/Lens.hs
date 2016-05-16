{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Pagure.Lens where

import Control.Lens
import Web.Pagure.Users

makeFields ''GroupsR
makeFields ''UsersR
makeFields ''UserR
makeFields ''Repo
makeFields ''User
makeFields ''RepoSettings
