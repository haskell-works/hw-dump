{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Bits
  ( cmdBits
  ) where

import Control.Lens
import Data.Generics.Product.Any
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Positioning
import Options.Applicative            hiding (columns)

import qualified App.Commands.Options.Type as Z
import qualified App.IO                    as IO
import qualified Data.ByteString.Lazy      as LBS
import qualified Options.Applicative       as O
import qualified System.IO                 as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant return"    -}

lazyByteStringChunks :: Int -> LBS.ByteString -> [LBS.ByteString]
lazyByteStringChunks n bs = case LBS.splitAt (fromIntegral n) bs of
  (lbs, rbs) -> if LBS.length rbs > 0
    then lbs:lazyByteStringChunks n rbs
    else if LBS.length lbs > 0
      then [lbs]
      else []

intersperseN :: a -> Count -> Count -> [a] -> [a]
intersperseN a n 0 xs     = a:intersperseN a n n xs
intersperseN a n i (x:xs) = x:intersperseN a n (i - 1) xs
intersperseN _ _ _ []     = []

runBits :: Z.BitsOptions -> IO ()
runBits opts = do
  let file      = opts ^. the @"file"
  let wordBits  = opts ^. the @"wordBits"

  chunkedContents <- lazyByteStringChunks 64 <$> IO.readInputFile file

  let xs :: String = foldl (.) id (fmap bitShows chunkedContents) ""

  IO.putStrLn $ intersperseN '\n' wordBits 0 (filter (/= ' ') xs)

  return ()

optsBits :: Parser Z.BitsOptions
optsBits = Z.BitsOptions
  <$> strOption
      (   long "file"
      <>  help "Source file"
      <>  metavar "FILE"
      )
  <*> option auto
      (   long "word-bits"
      <>  help "Bit file"
      <>  metavar "FILE"
      <>  showDefault <> O.value 8
      )

cmdBits :: Mod CommandFields (IO ())
cmdBits = command "bits"  $ flip info idm $ runBits <$> optsBits
