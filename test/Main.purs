module Test.Main where

import Prelude

import Botkit.Slack.Types (BOTKIT)
import Control.Monad.Eff (Eff)
import Test.Spec.Runner (RunnerEffects, run)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Handler (handlerSpec)

main :: Eff (RunnerEffects (botkit :: BOTKIT)) Unit
main = run [consoleReporter] do
  handlerSpec
