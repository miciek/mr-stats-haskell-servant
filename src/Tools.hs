module Tools (errorToString, logIt) where
  import Control.Monad.Trans
  import Control.Monad.Trans.Either

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
