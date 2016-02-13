{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module MergeRequests (MergeRequest (..), allMergeRequests) where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API
import Servant.Client
import PagedResponse (Paged, nextPage)
import Tools (AppConfig (..))

data MergeRequest = MergeRequest
  { id :: Int
  , iid :: Int
  , title :: String
  , state :: String
  , created_at :: String
  , updated_at :: String
  , upvotes :: Int
  , downvotes :: Int
  } deriving (Show, Generic)

instance FromJSON MergeRequest
instance ToJSON MergeRequest

type API =
  "api" :> "v3"
  :> Header "PRIVATE-TOKEN" String
  :> "projects"
  :> Capture "projectId" Int
  :> "merge_requests"
  :> QueryParam "page" String
  :> Get '[JSON] (Paged [MergeRequest])

api :: Proxy API
api = Proxy

mergeRequests :: Maybe String -> Int -> Maybe String -> EitherT ServantError IO (Paged [MergeRequest])
mergeRequests = client api (BaseUrl Https "gitlab.tech.lastmile.com" 443)

query :: AppConfig -> String -> EitherT ServantError IO (Paged [MergeRequest])
query config page = mergeRequests (Just $ configToken config) 3106 (Just page)

allMergeRequests :: AppConfig -> EitherT ServantError IO [MergeRequest]
allMergeRequests config = go [] "1" where
  go current page = do
    pagedResponse <- query config page
    let mrs = getResponse pagedResponse
    let allMrs = mrs ++ current
    let newPage = nextPage pagedResponse
    case newPage of
      Just p -> go allMrs p
      Nothing -> return allMrs
