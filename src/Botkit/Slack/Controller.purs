module Botkit.Slack.Controller where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Function.Uncurried (Fn4, runFn4)
import Node.Express.Handler (Handler, runHandlerM)
import Node.Express.Types (ExpressM, Request, Response)
import Node.HTTP (Server)

import Botkit.Slack.Types (BOTKIT, RawController)

type BotConfig =
  { json_file_store :: String }

type AppConfig =
  { clientId :: String
  , clientSecret :: String
  , redirectUri :: String
  , scopes :: Array String
  }

type Port = Int

createSlackBot :: forall e. BotConfig -> Eff (botkit :: BOTKIT | e) RawController
createSlackBot = createSlackBotImpl

toSlackApp :: forall e. AppConfig -> RawController -> Eff (botkit :: BOTKIT | e) RawController
toSlackApp = toSlackAppImpl

setupWebserver
  :: forall e.
    RawController ->
    Port ->
    Aff (botkit :: BOTKIT | e) Server
setupWebserver controller port =
  makeAff \onFailure onSuccess ->
    runFn4 setupWebserverImpl controller port onFailure onSuccess

createWebhookEndpoints :: forall e. RawController -> Server -> Aff (botkit :: BOTKIT | e) Unit
createWebhookEndpoints c s = liftEff $ createWebhookEndpointsImpl c s

createOauthEndpoints
  :: forall e.
    RawController ->
    Server ->
    (Handler e) ->
    (Error -> Handler e) ->
    Aff (botkit :: BOTKIT | e) Unit
createOauthEndpoints c s onSuccess onFailure =
  liftEff $
    runFn4
      createOauthEndpointsImpl
      c
      s
      (runHandlerM <<< onFailure)
      (runHandlerM onSuccess)

foreign import createSlackBotImpl
  :: forall e. BotConfig -> Eff (botkit :: BOTKIT | e) RawController

foreign import toSlackAppImpl
  :: forall e. AppConfig -> RawController -> Eff (botkit :: BOTKIT | e) RawController

foreign import setupWebserverImpl
  :: forall e.
    Fn4
      RawController
      Port
      (Error -> Eff (botkit :: BOTKIT | e) Unit)
      (Server -> Eff (botkit :: BOTKIT | e) Unit)
      (Eff (botkit :: BOTKIT | e) Unit)

foreign import createWebhookEndpointsImpl
  :: forall e.
    RawController -> Server -> Eff (botkit :: BOTKIT | e) Unit

foreign import createOauthEndpointsImpl
  :: forall e.
    Fn4
      RawController
      Server
      (Error -> Request -> Response -> ExpressM e Unit -> ExpressM e Unit)
      (Request -> Response -> ExpressM e Unit -> ExpressM e Unit)
      (Eff (botkit :: BOTKIT | e) Unit)
