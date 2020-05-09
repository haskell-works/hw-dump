{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Complement
  ( cmdComplement
  ) where

import Control.Lens
import Data.Generics.Product.Any
import HaskellWorks.Data.Bits.BitWise
import Options.Applicative            hiding (columns)

import qualified App.Commands.Options.Type as Z
import qualified Data.ByteString.Lazy      as LBS

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant return"    -}

runComplement :: Z.ComplementOptions -> IO ()
runComplement opts = do
  let inputFile   = opts ^. the @"inputFile"
  let outputFile  = opts ^. the @"outputFile"

  LBS.readFile inputFile <&> LBS.map comp >>= LBS.writeFile outputFile

optsComplement :: Parser Z.ComplementOptions
optsComplement = Z.ComplementOptions
  <$> strOption
        (   long "input-file"
        <>  help "Input file"
        <>  metavar "FILE"
        )
  <*> strOption
        (   long "output-file"
        <>  help "Output file"
        <>  metavar "FILE"
        )

cmdComplement :: Mod CommandFields (IO ())
cmdComplement = command "complement"  $ flip info idm $ runComplement <$> optsComplement
