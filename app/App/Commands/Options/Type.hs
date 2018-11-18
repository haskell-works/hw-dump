module App.Commands.Options.Type where

data BitsOptions = BitsOptions
  { _bitsOptionsFile     :: FilePath
  , _bitsOptionsBitFiles :: [FilePath]
  } deriving (Eq, Show)

data SelectedByBitsOptions = SelectedByBitsOptions
  { _selectedByBitsOptionsFile    :: FilePath
  , _selectedByBitsOptionsBitFile :: FilePath
  } deriving (Eq, Show)

data WordsOptions = WordsOptions
  { _wordsOptionsFile     :: FilePath
  , _wordsOptionsWordSize :: Int
  } deriving (Eq, Show)
