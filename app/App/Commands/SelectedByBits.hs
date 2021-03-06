{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SelectedByBits
  ( cmdSelectedByBits
  ) where

import Control.Lens
import Control.Monad
import Data.Bits                      (countTrailingZeros)
import Data.Generics.Product.Any
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import Options.Applicative            hiding (columns)

import qualified App.Commands.Options.Type           as Z
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Builder             as B
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Unsafe              as BS
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.ByteString.Lazy   as LBS
import qualified HaskellWorks.Data.Vector.AsVector64 as DVS
import qualified System.IO                           as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant return"    -}

selectedBytes :: BS.ByteString -> BS.ByteString -> B.Builder
selectedBytes as bs = go (DVS.head (DVS.asVector64 bs)) mempty
  where go :: Word64 -> B.Builder -> B.Builder
        go w b = if w /= 0
          then let i = countTrailingZeros w in
            go (w .&. (0xfffffffffffffffe .<. fromIntegral i)) (b <> B.word8 (BS.unsafeIndex as i))
          else b

runSelectedByBits :: Z.SelectedByBitsOptions -> IO ()
runSelectedByBits opts = do
  let file    = opts ^. the @"file"
  let bitFile = opts ^. the @"bitFile"

  chunkedContents    <- LBS.toChunks . LBS.rechunkPadded 64 <$> LBS.readFile file
  chunkedBitContents <- LBS.toChunks . LBS.rechunkPadded 8  <$> LBS.readFile bitFile

  forM_ (zip chunkedContents chunkedBitContents) $ \(byteChunk, bitChunk) -> do
    B.hPutBuilder IO.stdout $ selectedBytes byteChunk bitChunk

  return ()

optsSelectedByBits :: Parser Z.SelectedByBitsOptions
optsSelectedByBits = Z.SelectedByBitsOptions
  <$> strOption
      (   long "file"
      <>  help "Source file"
      <>  metavar "FILE"
      )
  <*> strOption
      (   long "bit-file"
      <>  help "Bit file"
      <>  metavar "FILE"
      )

cmdSelectedByBits :: Mod CommandFields (IO ())
cmdSelectedByBits = command "selected-by-bits"  $ flip info idm $ runSelectedByBits <$> optsSelectedByBits

