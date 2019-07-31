{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Type where

import GHC.Generics

data InterestBitsOptions = InterestBitsOptions
  { file     :: FilePath
  , bitFiles :: [FilePath]
  } deriving (Eq, Show, Generic)

data ComplementOptions = ComplementOptions
  { inputFile  :: FilePath
  , outputFile :: FilePath
  } deriving (Eq, Show, Generic)

data SelectedByBitsOptions = SelectedByBitsOptions
  { file    :: FilePath
  , bitFile :: FilePath
  } deriving (Eq, Show, Generic)

data SliceOptions = SliceOptions
  { file      :: FilePath
  , wordSize  :: Int
  , wordIndex :: Int
  } deriving (Eq, Show, Generic)

data WordsOptions = WordsOptions
  { file     :: FilePath
  , wordSize :: Int
  } deriving (Eq, Show, Generic)
