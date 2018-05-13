module App.Commands where

import App.Commands.Bits
import Data.Semigroup      ((<>))
import Options.Applicative

cmdOpts :: Parser (IO ())
cmdOpts = subparser $ mempty
  <>  cmdBits
