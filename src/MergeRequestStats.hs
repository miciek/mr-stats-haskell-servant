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

fromMergeRequestAndComments :: (MergeRequest, Maybe [MergeRequestComment]) -> Maybe MergeRequestStats
fromMergeRequestAndComments (mr, mrComments) = do
  ttm <- calculateTimeToMerge mr
  let comms = calculateCommentsQty mrComments
  return $ MergeRequestStats mr ttm comms ("https://gitlab.tech.lastmile.com/warehouse-stations/stations/merge_requests/" ++ (show $ MergeRequests.iid mr))

calculateTimeToMerge :: MergeRequest -> Maybe NominalDiffTime
calculateTimeToMerge mr = do
  createdAt <- parseISO8601 $ created_at mr
  updatedAt <- parseISO8601 $ updated_at mr
  return $ diffUTCTime updatedAt createdAt

calculateCommentsQty :: Maybe [MergeRequestComment] -> Int
calculateCommentsQty mrComments =
  case mrComments of
    Just mrc -> length mrc
    Nothing -> 0
