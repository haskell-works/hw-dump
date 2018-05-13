module App.Commands.Options.Type where

data BitsOptions = BitsOptions
  { _bitsOptionsFile     :: FilePath
  , _bitsOptionsBitFiles :: [FilePath]
  } deriving (Eq, Show)
