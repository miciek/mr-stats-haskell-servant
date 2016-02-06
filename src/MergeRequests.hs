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

data MergeRequest = MergeRequest
  { id :: Int
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

query :: String -> String -> EitherT ServantError IO (Paged [MergeRequest])
query token page = mergeRequests (Just token) 3106 (Just page)

allMergeRequests :: String -> IO [MergeRequest]
allMergeRequests token = go [] "1" where
  go current page =
    do
      res <- runEitherT $ query token page
      case res of
        Left err ->
          do
            putStrLn $ "Error: " ++ show err
            return []
        Right result ->
          case newPage of
            Just p -> go allMrs p
            Nothing -> return allMrs
          where
            mrs = getResponse result
            allMrs = mrs ++ current
            newPage = nextPage result
