{-# LANGUAGE DeriveGeneric #-}

module MergeRequestStats (MergeRequestStats (..), fromMergeRequestAndComments) where

import Data.Aeson
import Data.Time.Clock
import Data.Time.ISO8601
import GHC.Generics
import MergeRequests (MergeRequest (..))
import MergeRequestComments

data MergeRequestStats = MergeRequestStats
  { mergeRequest :: MergeRequest
  , timeToMerge :: NominalDiffTime
  , commentsQty :: Int
  , url :: String
  } deriving (Show, Generic)

instance ToJSON MergeRequestStats

fromMergeRequestAndComments :: String -> MergeRequest -> [MergeRequestComment] -> Maybe MergeRequestStats
fromMergeRequestAndComments entityUrl mr mrComments = do
  ttm <- calculateTimeToMerge mr
  let comms = length mrComments
  return $ MergeRequestStats mr ttm comms (entityUrl ++ (show $ MergeRequests.iid mr))

calculateTimeToMerge :: MergeRequest -> Maybe NominalDiffTime
calculateTimeToMerge mr = do
  createdAt <- parseISO8601 $ created_at mr
  updatedAt <- parseISO8601 $ updated_at mr
  return $ diffUTCTime updatedAt createdAt
