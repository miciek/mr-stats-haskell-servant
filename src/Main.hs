{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API
import Servant.Client
import System.Environment

tokenFromEnv :: IO String
tokenFromEnv = getEnv "GITLAB_TOKEN"

data MergeRequest = MergeRequest
  { id :: String
  , title :: String
  } deriving (Show, Generic)

instance FromJSON MergeRequest

type API = "api" :> "v3"
            :> Header "PRIVATE-TOKEN" String
            :> "projects"
            :> Capture "projectId" Int
            :> "merge_requests"
            :> Get '[JSON] [MergeRequest]

api :: Proxy API
api = Proxy

mergeRequests :: Maybe String -> Int -> EitherT ServantError IO [MergeRequest]
mergeRequests = client api (BaseUrl Https "gitlab.tech.lastmile.com" 443)

query :: String -> EitherT ServantError IO [MergeRequest]
query token = mergeRequests (Just token) 3106

main :: IO ()
main = do
  token <- tokenFromEnv
  res <- runEitherT $ query token
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right mrs -> print mrs
