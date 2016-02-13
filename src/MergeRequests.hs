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

apiQuery :: BaseUrl -> Maybe String -> Int -> Maybe String -> EitherT ServantError IO (Paged [MergeRequest])
apiQuery url = client api url

pagedMergeRequests :: AppConfig -> String -> EitherT ServantError IO (Paged [MergeRequest])
pagedMergeRequests config page = apiQuery
                      (BaseUrl Https (cfgServerHost config) (cfgServerPort config))
                      (Just $ cfgToken config)
                      (cfgProjectId config)
                      (Just page)

allMergeRequests :: AppConfig -> EitherT ServantError IO [MergeRequest]
allMergeRequests config = fetchPage [] "1" where
  fetchPage current page = do
    pagedResponse <- pagedMergeRequests config page
    let mrs = getResponse pagedResponse
    let allMrs = mrs ++ current
    let newPage = nextPage pagedResponse
    case newPage of
      Just p -> fetchPage allMrs p
      Nothing -> return allMrs
