{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Bits
  ( cmdBits
  ) where

import Control.Lens
import Control.Monad
import Data.Char                      (isAscii, isPrint)
import Data.Generics.Product.Any
import Data.List                      (transpose)
import Data.Semigroup                 ((<>))
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Positioning
import Numeric                        (showHex)
import Options.Applicative            hiding (columns)

import qualified App.Commands.Options.Type  as Z
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Options.Applicative        as O
import qualified System.IO                  as IO

{-# ANN module ("HLint: ignore Redundant do"      :: String) #-}
{-# ANN module ("HLint: ignore Redundant return"  :: String) #-}

lazyByteStringChunks :: Int -> LBS.ByteString -> [LBS.ByteString]
lazyByteStringChunks n bs = case LBS.splitAt (fromIntegral n) bs of
  (lbs, rbs) -> if LBS.length rbs > 0
    then lbs:lazyByteStringChunks n rbs
    else if LBS.length lbs > 0
      then [lbs]
      else []

zap :: [a] ->  [[b]] -> [(a, [b])]
zap (a:as) (b:bs) = (a,  b):(zap as bs)
zap (a:as) _      = (a, []):(zap as [])
zap _      _      = []

isAsciiPrintable :: Char -> Bool
isAsciiPrintable c = isPrint c && isAscii c

maskNonAsciiPrintable :: Char -> Char
maskNonAsciiPrintable c = if isAsciiPrintable c then c else '.'

intersperseN :: a -> Count -> Count -> [a] -> [a]
intersperseN a n 0 xs     = a:intersperseN a n n xs
intersperseN a n i (x:xs) = x:intersperseN a n (i - 1) xs
intersperseN _ _ _ []     = []

runBits :: Z.BitsOptions -> IO ()
runBits opts = do
  let file      = opts ^. the @"file"
  let wordBits  = opts ^. the @"wordBits"

  chunkedContents <- lazyByteStringChunks 64 <$> LBS.readFile file

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
