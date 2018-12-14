module Data.Nemo.Error where

import           Control.Exception      (Exception, throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Text.Parsec            (parse)
import           Text.Parsec.Error      (ParseError, errorPos, setErrorPos)
import           Text.Parsec.Pos        (setSourceLine)
import           Text.Parsec.String     (Parser)

data Error
  = UnableToDecodeNcuInfo
  | UnableToFindMarker
  | BadDirective
  | UnableToParse ParseError
  deriving (Show)

instance Exception Error

maybeDie :: MonadIO m => Error -> Maybe a -> m a
maybeDie _ (Just a)  = return a
maybeDie err Nothing = liftIO $ throwIO err

parseOrDie :: MonadIO m => Parser a -> String -> Int -> String -> m a
parseOrDie parser file lineNum source =
  case parse parser file source of
    Right n -> return n
    Left err -> do
      pos <- pure $ setSourceLine (errorPos err) lineNum
      liftIO $ throwIO (UnableToParse (setErrorPos pos err))
