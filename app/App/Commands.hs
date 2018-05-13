module App.Commands where

import App.Commands.Bits
import App.Commands.Words
import Data.Semigroup      ((<>))
import Options.Applicative

cmdOpts :: Parser (IO ())
cmdOpts = subparser $ mempty
  <>  cmdBits
  <>  cmdWords
