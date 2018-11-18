module App.Commands where

import App.Commands.Bits
import App.Commands.SelectedByBits
import App.Commands.Slice
import App.Commands.Words
import Data.Semigroup              ((<>))
import Options.Applicative

cmdOpts :: Parser (IO ())
cmdOpts = subparser $ mempty
  <>  cmdBits
  <>  cmdSelectedByBits
  <>  cmdSlice
  <>  cmdWords
