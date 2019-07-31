module App.Commands where

import App.Commands.Complement
import App.Commands.InterestBits
import App.Commands.SelectedByBits
import App.Commands.Slice
import App.Commands.Words
import Data.Semigroup              ((<>))
import Options.Applicative

cmdOpts :: Parser (IO ())
cmdOpts = subparser $ mempty
  <>  cmdComplement
  <>  cmdInterestBits
  <>  cmdSelectedByBits
  <>  cmdSlice
  <>  cmdWords
