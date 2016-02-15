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
import Control.Concurrent.Async
import Data.Proxy
import Data.Maybe
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import MergeRequests (id, allMergeRequests)
import MergeRequestStats
import MergeRequestComments
import Tools

type StatsStorage = InMemStorage MergeRequestStats

fetchMRsAndSaveStats :: StatsStorage -> EitherT String IO ()
fetchMRsAndSaveStats storage = do
  config <- appConfig
  fetchedMRs <- errorToString . allMergeRequests $ config
  let fetchComments = errorToString . allComments config . MergeRequests.id
  fetchedComments <- mapM fetchComments fetchedMRs
  let stats = catMaybes $ zipWith (calculateStats config) fetchedMRs fetchedComments
  liftIO $ writeToStorage storage stats

type ServerAPI = "mrs" :> Get '[JSON] [MergeRequestStats]
                 :<|> "front" :> Raw

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

server :: StatsStorage -> Server ServerAPI
server storage = liftIO (readFromStorage storage)
                 :<|> serveDirectory "../mrstats-front"

serverApp :: StatsStorage -> Application
serverApp storage = serve serverAPI (server storage)

main :: IO ()
main = do
  storage <- newStorage
  _ <- async . logIt . runEitherT $ fetchMRsAndSaveStats storage
  run 8080 (serverApp storage)
