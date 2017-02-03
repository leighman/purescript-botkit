module Botkit.Slack.Bot where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)

import Botkit.Slack.Types (BOTKIT, RawBot)

startRTM :: forall e. RawBot -> (Error -> Eff e Unit) -> Eff (botkit :: BOTKIT | e) Unit
startRTM = startRTMImpl

foreign import startRTMImpl
  :: forall e.
    RawBot -> (Error -> Eff e Unit) -> Eff (botkit :: BOTKIT | e) Unit
