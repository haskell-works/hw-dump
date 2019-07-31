{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.InterestBits
  ( cmdInterestBits
  ) where

import Control.Lens
import Control.Monad
import Data.Char                      (isAscii, isPrint)
import Data.Generics.Product.Any
import Data.List                      (transpose)
import Data.Semigroup                 ((<>))
import HaskellWorks.Data.Bits.BitShow
import Numeric                        (showHex)
import Options.Applicative            hiding (columns)

import qualified App.Commands.Options.Type  as Z
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
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

runInterestBits :: Z.InterestBitsOptions -> IO ()
runInterestBits opts = do
  let file      = opts ^. the @"file"
  let bitFiles  = opts ^. the @"bitFiles"

  chunkedContents    <- lazyByteStringChunks 64 <$> LBS.readFile file
  chunkedBitContents <- forM bitFiles $ (lazyByteStringChunks 8 <$>) . LBS.readFile

  forM_ (zap (zip [0..] chunkedContents) (transpose chunkedBitContents)) $ \((i :: Int, as), bss) -> do
    IO.putStr (reverse (take 8 (reverse ((("00000000" ++) . showHex i) ""))))
    IO.putStr " "
    let css = lazyByteStringChunks 8 as
    forM_ css $ \cs -> do
      IO.putStr " "
      IO.putStr (maskNonAsciiPrintable <$> C8.unpack cs)
    IO.putStrLn ""
    forM_ bss $ \bs -> do
      IO.putStr "         "
      forM_ (zip css (LBS.unpack bs)) $ \(cs, b) -> do
        IO.putStr " "
        IO.putStr $ take (fromIntegral (LBS.length cs)) (bitShow b)
      IO.putStrLn ""
    IO.putStrLn ""
    return ()

  return ()

optsInterestBits :: Parser Z.InterestBitsOptions
optsInterestBits = Z.InterestBitsOptions
  <$> strOption
        (   long "file"
        <>  help "Source file"
        <>  metavar "FILE"
        )
  <*> many
      ( strOption
        (   long "bit-file"
        <>  help "Bit file"
        <>  metavar "FILE"
        )
      )

cmdInterestBits :: Mod CommandFields (IO ())
cmdInterestBits = command "interest-bits"  $ flip info idm $ runInterestBits <$> optsInterestBits
