module Botkit.Slack.Internal.Logger where

import Prelude

import Control.Monad.Eff (Eff)
import Botkit.Slack.Types (BOTKIT, RawController)

foreign import error
  :: forall e.
    RawController ->
    String ->
    Eff (botkit :: BOTKIT | e) Unit

foreign import warn
  :: forall e.
    RawController ->
    String ->
    Eff (botkit :: BOTKIT | e) Unit

foreign import info
  :: forall e.
    RawController ->
    String ->
    Eff (botkit :: BOTKIT | e) Unit

foreign import debug
  :: forall e.
    RawController ->
    String ->
    Eff (botkit :: BOTKIT | e) Unit
