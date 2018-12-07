module Data.Nemo.Error where

import           Control.Exception      (Exception, throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)

data Error
  = UnableToDecodeNcuInfo
  | UnableToFindMarker
  | BadDirective
  deriving (Show)

instance Exception Error

maybeDie :: MonadIO m => Error -> Maybe a -> m a
maybeDie _ (Just a)  = return a
maybeDie err Nothing = liftIO $ throwIO err
