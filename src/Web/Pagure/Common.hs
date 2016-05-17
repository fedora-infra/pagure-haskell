{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Pagure.Common where

import Data.String (IsString)
import qualified Data.Text as T
import GHC.Generics
import Servant.API
import Servant.Docs hiding (API)

-- | A pattern to filter out responses in endpoints which permit such filtering.
newtype Pattern = Pattern T.Text deriving (Eq, Generic, IsString, Ord, Show)

instance ToHttpApiData Pattern where toUrlPiece (Pattern a) = a
instance ToParam (QueryParam "pattern" Pattern) where
  toParam _ =
    DocQueryParam "pattern"
                  ["Fed*", "*web*", "*ora", "..."]
                  "An optional pattern to filter by"
                  Normal
