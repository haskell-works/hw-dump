{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Words
  ( cmdWords
  ) where

import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.Word
import Numeric                   (showHex)
import Options.Applicative       hiding (columns)

import qualified App.Commands.Options.Type           as Z
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.FromForeignRegion as IO
import qualified System.IO                           as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant return"    -}

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

runWords :: Z.WordsOptions -> IO ()
runWords opts = do
  let file      = opts ^. the @"file"
  let wordSize  = opts ^. the @"wordSize"

  case wordSize of
    64 -> IO.mmapFromForeignRegion file >>= printWords64
    n  -> IO.hPutStrLn IO.stderr $ "Unsupported word size: " <> show n

  return ()

optsWords :: Parser Z.WordsOptions
optsWords = Z.WordsOptions
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
