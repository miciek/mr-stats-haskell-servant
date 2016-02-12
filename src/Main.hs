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
import MergeRequests (id, allMergeRequests, MergeRequest)
import MergeRequestStats
import MergeRequestComments

tokenFromEnv :: EitherT String IO String
tokenFromEnv = do
  token <- liftIO $ lookupEnv "GITLAB_TOKEN"
  case token of
    Nothing -> hoistEither . Left $ "Error: GITLAB_TOKEN not found in the environment vars."
    Just val -> return val

mergeRequestsFromServer :: String -> EitherT String IO [MergeRequest]
mergeRequestsFromServer token = do
  mrsFromServer <- liftIO $ runEitherT $ allMergeRequests token
  case mrsFromServer of
    Left err -> left . show $ err
    Right mrs -> right mrs

commentsFromServer :: String -> Int -> EitherT String IO [MergeRequestComment]
commentsFromServer token mid = do
  comments <- liftIO $ runEitherT $ fetchComments token mid
  case comments of
    Left err -> left . show $ err
    Right comms -> right comms

type MergeRequestStorage = TVar [MergeRequestStats]

saveMergeRequestsToStorage :: MergeRequestStorage -> EitherT String IO [MergeRequestStats]
saveMergeRequestsToStorage storage = do
  token <- tokenFromEnv
  mrs <- mergeRequestsFromServer token
  fetchedComments <- mapM (commentsFromServer token . MergeRequests.id) mrs
  let stats = catMaybes $ zipWith (curry fromMergeRequestAndComments) mrs fetchedComments
  liftIO . atomically $ writeTVar storage stats
  return $ stats

logIt :: Show a => IO a -> IO a
logIt toLog = do
  result <- toLog
  print result
  return result

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
