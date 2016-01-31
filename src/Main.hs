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
import Data.Text (Text, unpack)
import Data.ByteString.UTF8 (fromString)
import GHC.Generics
import Servant.API
import Servant.Client
import System.Environment
import Network.HTTP.Link
import Network.HTTP.Types.URI

tokenFromEnv :: IO String
tokenFromEnv = getEnv "GITLAB_TOKEN"

data MergeRequest = MergeRequest
  { id :: Int
  , title :: String
  } deriving (Show, Generic)

instance FromJSON MergeRequest

type API =
  "api" :> "v3"
  :> Header "PRIVATE-TOKEN" String
  :> "projects"
  :> Capture "projectId" Int
  :> "merge_requests"
  :> QueryParam "page" String
  :> Get '[JSON] (Headers '[Header "Link" Text] [MergeRequest])

api :: Proxy API
api = Proxy

mergeRequests :: Maybe String -> Int -> Maybe String -> EitherT ServantError IO (Headers '[Header "Link" Text] [MergeRequest])
mergeRequests = client api (BaseUrl Https "gitlab.tech.lastmile.com" 443)

query :: String -> String -> EitherT ServantError IO (Headers '[Header "Link" Text] [MergeRequest])
query token page = mergeRequests (Just token) 3106 (Just page)

extractNextURI :: [Link] -> Maybe URI
extractNextURI [] = Nothing
extractNextURI (link:rest) =
  case link of
    Link uri params ->
      if hasNext params then
        Just uri
      else extractNextURI rest
      where hasNext = foldr ((||) . isNext) False
            isNext (Rel, "next") = True
            isNext _ = False

extractPageParam :: URI -> Maybe Text
extractPageParam uri =
  do
    item <- extractPageQueryItem queryText
    case item of (_, value) -> value
  where queryText = parseQueryText $ fromString $ uriQuery uri
        extractPageQueryItem [] = Nothing
        extractPageQueryItem (x:xs) =
          if isPageItem x then Just x else extractPageQueryItem xs
        isPageItem item =
          case item of
            ("page", _) -> True
            _ -> False

nextPage :: HList '[Header "Link" Text] -> Maybe String
nextPage hdrs = case hdrs of
  HCons (Header val) _ ->
      do
        links <- parseLinkHeader val
        uri <- extractNextURI links
        pageText <- extractPageParam uri
        return $ unpack pageText
  _ -> Nothing

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
            newPage = nextPage $ getHeadersHList result

main :: IO ()
main = do
  token <- tokenFromEnv
  mrs <- allMergeRequests token
  mapM_ (putStrLn . title) mrs
