{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module MergeRequestComments (MergeRequestComment (..), fetchComments) where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API
import Servant.Client

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
  :> Get '[JSON] (Maybe [MergeRequestComment])

api :: Proxy API
api = Proxy

mergeRequestComments :: Maybe String -> Int -> Int -> EitherT ServantError IO (Maybe [MergeRequestComment])
mergeRequestComments = client api (BaseUrl Https "gitlab.tech.lastmile.com" 443)

query :: String -> Int -> EitherT ServantError IO (Maybe [MergeRequestComment])
query token = mergeRequestComments (Just token) 3106

fetchComments :: String -> Int -> IO (Maybe [MergeRequestComment])
fetchComments token mergeRequestId = do
      res <- runEitherT $ query token mergeRequestId
      case res of
        Left err ->
          do
            putStrLn $ "Error: " ++ show err
            return Nothing
        Right result ->
          return result
