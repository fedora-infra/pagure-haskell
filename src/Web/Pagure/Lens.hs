{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Pagure.Lens where

import Control.Lens
import Web.Pagure.Users

makeFields ''GroupName
makeFields ''GroupsR
makeFields ''Repo
makeFields ''RepoSettings
makeFields ''User
makeFields ''UserFullname
makeFields ''UserR
makeFields ''Username
makeFields ''UsersR
