{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.STM
import Data.Proxy
import Data.Maybe
import Servant.API
import Servant.Server
import System.Environment
import Network.Wai
import Network.Wai.Handler.Warp
import MergeRequests (id, allMergeRequests)
import MergeRequestStats
import MergeRequestComments

tokenFromEnv :: IO String
tokenFromEnv = getEnv "GITLAB_TOKEN"

type MergeRequestStorage = TVar [MergeRequestStats]

fetchMergeRequests :: MergeRequestStorage -> IO ()
fetchMergeRequests storage = do
  putStrLn "fetching merge requests"
  token <- tokenFromEnv
  mrs <- allMergeRequests token
  putStrLn "fetched merge requests, fetching comments"
  fetchedComments <- mapM (fetchComments token . MergeRequests.id) mrs
  putStrLn "fetched comments"
  let stats = mapMaybe fromMergeRequestAndComments (zip mrs fetchedComments)
  atomically $ writeTVar storage stats
  putStrLn "saved mrs to storage"

type ServerAPI = "mrs" :> Get '[JSON] [MergeRequestStats]

server :: MergeRequestStorage -> Server ServerAPI
server storage = liftIO $ atomically $ readTVar storage

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

app :: MergeRequestStorage -> Application
app storage = serve serverAPI (server storage)

main :: IO ()
main = do
  storage <- atomically $ newTVar []
  _ <- forkIO $ fetchMergeRequests storage
  run 8080 (app storage)
