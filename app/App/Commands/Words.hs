{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Words
  ( cmdWords
  ) where

import App.Commands.Options.Type
import Control.Lens
import Control.Monad
import Data.Char                      (isAscii, isPrint)
import Data.List                      (transpose)
import Data.Semigroup                 ((<>))
import Data.Word
import HaskellWorks.Data.Bits.BitShow
import Numeric                        (showHex)
import Options.Applicative            hiding (columns)

import qualified App.Commands.Options.Lens           as L
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Lazy.Char8          as C8
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.FromForeignRegion as IO
import qualified System.IO                           as IO

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

printWords64 :: DVS.Vector Word64 -> IO ()
printWords64 v = do
  forM_ [0 .. DVS.length v - 1] $ \i -> do
    IO.putStr $ reverse (take 8  (reverse ((("00000000" ++) . showHex i) "")))
    IO.putStr "  "
    IO.putStr $ reverse (take 16 (reverse ((("0000000000000000" ++) . showHex (v DVS.! i)) "")))
    IO.putStr " "
    IO.putStr $ show (v DVS.! i)
    IO.putStrLn ""
    return ()

  return ()

runWords :: WordsOptions -> IO ()
runWords opts = do
  let file      = opts ^. L.file
  let wordSize  = opts^. L.wordSize

  case wordSize of
    64 -> IO.mmapFromForeignRegion file >>= printWords64
    n  -> putStrLn "Unsupported word size"

  return ()

optsWords :: Parser WordsOptions
optsWords = WordsOptions
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

cmdWords :: Mod CommandFields (IO ())
cmdWords = command "words"  $ flip info idm $ runWords <$> optsWords
