{-# LANGUAGE DeriveGeneric #-}

module MergeRequestStats (MergeRequestStats (..), calculateStats) where

import Data.Aeson
import Data.Time.Clock
import Data.Time.ISO8601
import GHC.Generics
import MergeRequests (MergeRequest (..))
import MergeRequestComments
import Tools

data MergeRequestStats = MergeRequestStats
  { mergeRequest :: MergeRequest
  , timeToMerge :: NominalDiffTime
  , commentsQty :: Int
  , url :: String
  } deriving (Show, Generic)

instance ToJSON MergeRequestStats

calculateStats :: AppConfig -> MergeRequest -> [MergeRequestComment]
                  -> Maybe MergeRequestStats
calculateStats config mr mrComments = do
  ttm <- calculateTimeToMerge mr
  return $ MergeRequestStats mr
           ttm
           (length mrComments)
           (cfgEntityUrl config ++ show (iid mr))

calculateTimeToMerge :: MergeRequest -> Maybe NominalDiffTime
calculateTimeToMerge mr = do
  createdAt <- parseISO8601 $ created_at mr
  updatedAt <- parseISO8601 $ updated_at mr
  return $ diffUTCTime updatedAt createdAt
