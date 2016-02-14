{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module MergeRequestComments (MergeRequestComment (..), allComments) where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API
import Servant.Client
import Tools

data MergeRequestComment = MergeRequestComment
  { note :: String
  } deriving (Show, Generic)

instance FromJSON MergeRequestComment

type API =
  "api" :> "v3"
  :> Header "PRIVATE-TOKEN" String
  :> "projects"
  :> Capture "projectId" Int
  :> "merge_request"
  :> Capture "mergeRequestId" Int
  :> "comments"
  :> Get '[JSON] [MergeRequestComment]

api :: Proxy API
api = Proxy

apiQuery :: BaseUrl -> Maybe String -> Int -> Int
            -> EitherT ServantError IO [MergeRequestComment]
apiQuery = client api

allComments :: AppConfig -> Int
               -> EitherT ServantError IO [MergeRequestComment]
allComments config = apiQuery
                      (BaseUrl Https (cfgServerHost config) (cfgServerPort config))
                      (Just $ cfgToken config)
                      (cfgProjectId config)
