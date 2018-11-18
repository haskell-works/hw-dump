{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Slice
  ( cmdSlice
  ) where

import App.Commands.Options.Type
import Control.Lens
import Control.Monad
import Data.Bits.Pext
import Data.Semigroup                      ((<>))
import Data.Word
import HaskellWorks.Data.Vector.AsVector64
import Options.Applicative                 hiding (columns)

import qualified App.Commands.Options.Lens         as L
import qualified Data.ByteString.Builder           as B
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.Vector.Storable              as DVS
import qualified HaskellWorks.Data.ByteString      as BS
import qualified HaskellWorks.Data.ByteString.Lazy as LBS
import qualified System.IO                         as IO

{-# ANN module ("HLint: ignore Redundant do"      :: String) #-}
{-# ANN module ("HLint: ignore Redundant return"  :: String) #-}

runSlice :: SliceOptions -> IO ()
runSlice opts = do
  let file      = opts ^. L.file
  let wordIndex = opts ^. L.wordIndex
  let wordSize  = opts ^. L.wordSize

  case (wordSize, wordIndex) of
    (2, 0) -> do
      vs <- fmap asVector64 . LBS.toChunks . LBS.resegment 8 <$> LBS.readFile file

      forM_ vs $ \v -> do
        let u = DVS.map (\w -> fromIntegral (pext w 0x5555555555555555) :: Word32) v
        B.hPutBuilder IO.stdout $ B.byteString (BS.toByteString u)
    (2, 1) -> do
      vs <- fmap asVector64 . LBS.toChunks . LBS.resegment 8 <$> LBS.readFile file

      forM_ vs $ \v -> do
        let u = DVS.map (\w -> fromIntegral (pext w 0xaaaaaaaaaaaaaaaa) :: Word32) v
        B.hPutBuilder IO.stdout $ B.byteString (BS.toByteString u)
    config -> error $ "Unsupported slice config: " <> show config

  return ()

optsSlice :: Parser SliceOptions
optsSlice = SliceOptions
  <$> strOption
      (   long "file"
      <>  help "Source file"
      <>  metavar "FILE"
      )
  <*> option auto
      (   long "word-size"
      <>  help "Word size"
      <>  metavar "INT"
      )
  <*> option auto
      (   long "word-bit"
      <>  help "Word bit"
      <>  metavar "INT"
      )

cmdSlice :: Mod CommandFields (IO ())
cmdSlice = command "slice"  $ flip info idm $ runSlice <$> optsSlice
