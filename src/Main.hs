{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Proxy
import Data.Maybe
import Servant
import System.Environment
import Network.Wai
import Network.Wai.Handler.Warp
import MergeRequests (id, allMergeRequests)
import MergeRequestStats
import MergeRequestComments
import Tools

appConfig :: EitherT String IO AppConfig
appConfig = do
  token <- liftIO $ lookupEnv "GITLAB_TOKEN"
  case token of
    Nothing -> hoistEither . Left $ "Error: GITLAB_TOKEN not found in the environment vars."
    Just val -> return $ AppConfig val

type MergeRequestStorage = TVar [MergeRequestStats]

saveMergeRequestsToStorage :: MergeRequestStorage -> EitherT String IO [MergeRequestStats]
saveMergeRequestsToStorage storage = do
  config <- appConfig
  mrs <- errorToString . allMergeRequests $ config
  fetchedComments <- mapM (errorToString . fetchComments config . MergeRequests.id) mrs
  let stats = catMaybes $ zipWith (curry fromMergeRequestAndComments) mrs fetchedComments
  liftIO . atomically $ writeTVar storage stats
  return stats

type ServerAPI = "mrs" :> Get '[JSON] [MergeRequestStats]
                 :<|> "front" :> Raw

server :: MergeRequestStorage -> Server ServerAPI
server storage = (liftIO . atomically $ readTVar storage)
                 :<|> serveDirectory "../mrstats-front"

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

app :: MergeRequestStorage -> Application
app storage = serve serverAPI (server storage)

main :: IO ()
main = do
  storage <- atomically $ newTVar []
  _ <- async . logIt . runEitherT $ saveMergeRequestsToStorage storage
  run 8080 (app storage)
