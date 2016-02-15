module Tools (AppConfig(..), appConfig
             , InMemStorage, newStorage, writeToStorage, readFromStorage
             , logIt, errorToString
             ) where
-- This module helps us keep the rest of the code beginner-friendly

import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Error (note)
import Text.Read
import System.Environment
import Control.Concurrent.STM

-- AppConfig stuff

data AppConfig = AppConfig
  { cfgToken :: String
  , cfgServerHost :: String
  , cfgServerPort :: Int
  , cfgProjectId :: Int
  , cfgEntityUrl :: String
  }

appConfig :: EitherT String IO AppConfig
appConfig = do
  token <- getEnvString "GITLAB_TOKEN"
  serverHost <- getEnvString "SERVER_HOST"
  serverPort <- getEnvInt "SERVER_PORT"
  projectId <- getEnvInt "PROJECT_ID"
  entityUrl <- getEnvString "ENTITY_URL"
  return $ AppConfig token serverHost serverPort projectId entityUrl

getEnvString :: String -> EitherT String IO String
getEnvString name = do
  val <- liftIO $ lookupEnv name
  hoistEither $ note errorNote val
  where
    errorNote = "Error: " ++ name ++ " not found in the environment vars."

getEnvInt :: String -> EitherT String IO Int
getEnvInt name = do
  val <- getEnvString name
  hoistEither $ note errorNote (readMaybe val)
  where
    errorNote = "Error: couldn't parse int value of env var " ++ name ++ "."

-- InMemStorage stuff

type InMemStorage a = TVar [a]

newStorage :: IO (InMemStorage a)
newStorage = atomically $ newTVar []

writeToStorage :: InMemStorage a -> [a] -> IO ()
writeToStorage storage stats = atomically $ writeTVar storage stats

readFromStorage :: InMemStorage a -> IO [a]
readFromStorage = readTVarIO

-- Various

logIt :: Show a => IO a -> IO a
logIt toLog = do
  result <- toLog
  print result
  return result

errorToString :: Show e => EitherT e IO a -> EitherT String IO a
errorToString m = do
  result <- liftIO $ runEitherT m
  case result of
    Left err -> left . show $ err
    Right val -> right val
