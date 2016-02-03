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
import Servant.API
import Servant.Server
import System.Environment
import Network.Wai
import Network.Wai.Handler.Warp

import MergeRequests (MergeRequest, allMergeRequests)

tokenFromEnv :: IO String
tokenFromEnv = getEnv "GITLAB_TOKEN"

type MergeRequestStorage = TVar [MergeRequest]

fetchMergeRequests :: MergeRequestStorage -> IO ()
fetchMergeRequests storage = do
  putStrLn "fetching merge requests"
  token <- tokenFromEnv
  mrs <- allMergeRequests token
  putStrLn "fetched merge requests"
  atomically $ writeTVar storage mrs
  putStrLn "saved mrs to storage"

showMergeRequests :: MergeRequestStorage -> IO ()
showMergeRequests storage = do
  putStrLn "showing MRs"
  mrs <- atomically $ readTVar storage
  print (length mrs)
  threadDelay 3000000
  showMergeRequests storage

type ServerAPI = "mrs" :> Get '[JSON] [MergeRequest]

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
  _ <- forkIO $ showMergeRequests storage
  run 8080 (app storage)
