{-# LANGUAGE DeriveGeneric #-}

module MergeRequestStats (MergeRequestStats (..), fromMergeRequest) where

import Data.Aeson
import Data.Time.Clock
import Data.Time.ISO8601
import GHC.Generics
import MergeRequests (MergeRequest (..))

data MergeRequestStats = MergeRequestStats
  { mergeRequest :: MergeRequest
  , timeToMerge :: NominalDiffTime
  } deriving (Show, Generic)

instance ToJSON MergeRequestStats

fromMergeRequest :: MergeRequest -> Maybe MergeRequestStats
fromMergeRequest mr = do
  ttm <- calculateTimeToMerge mr
  return $ MergeRequestStats mr ttm

calculateTimeToMerge :: MergeRequest -> Maybe NominalDiffTime
calculateTimeToMerge mr = do
  createdAt <- parseISO8601 $ created_at mr
  updatedAt <- parseISO8601 $ updated_at mr
  return $ diffUTCTime updatedAt createdAt
