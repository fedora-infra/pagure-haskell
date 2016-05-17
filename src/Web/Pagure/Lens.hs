{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Pagure.Lens where

import Control.Lens
import Web.Pagure.Common
import Web.Pagure.Extras
import Web.Pagure.Users

makeFields ''ErrorCodesR
makeFields ''GroupName
makeFields ''GroupsR
makeFields ''Repo
makeFields ''RepoSettings
makeFields ''TagsR
makeFields ''User
makeFields ''UserFullname
makeFields ''UserR
makeFields ''Username
makeFields ''UsersR
makeFields ''VersionR
