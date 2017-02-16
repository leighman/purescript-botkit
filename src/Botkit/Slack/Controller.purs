module Botkit.Slack.Controller where

import Prelude

import Control.Monad.Aff (Aff, launchAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Newtype (unwrap)
import Data.String (Pattern)
import Node.Express.Handler (Handler, runHandlerM)
import Node.Express.Types (ExpressM, Request, Response)
import Node.HTTP (Server)

import Botkit.Slack.Events (Event)
import Botkit.Slack.Types (BOTKIT, class IsControllerMode, AppMode, BotMode, Controller, RawBot, RawMessage)

type BotConfig =
  { json_file_store :: String }

type AppConfig =
  { clientId :: String
  , clientSecret :: String
  , redirectUri :: String
  , scopes :: Array String
  }

type Port = Int

createSlackBot :: forall e. BotConfig -> Eff (botkit :: BOTKIT | e) (Controller BotMode)
createSlackBot = createSlackBotImpl

toSlackApp :: forall e. AppConfig -> Controller BotMode -> Eff (botkit :: BOTKIT | e) (Controller AppMode)
toSlackApp = toSlackAppImpl

setupWebserver
  :: forall e.
    Controller AppMode ->
    Port ->
    Aff (botkit :: BOTKIT | e) Server
setupWebserver controller port =
  makeAff \onFailure onSuccess ->
    runFn4 setupWebserverImpl controller port onFailure onSuccess

createWebhookEndpoints :: forall e. Controller AppMode -> Server -> Aff (botkit :: BOTKIT | e) Unit
createWebhookEndpoints c s = liftEff $ createWebhookEndpointsImpl c s

createOauthEndpoints
  :: forall e.
    Controller AppMode ->
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

on
  :: forall m e. (IsControllerMode m) =>
    Controller m ->
    Array Event ->
    (RawBot -> RawMessage -> Aff (botkit :: BOTKIT | e) Unit) ->
    Aff (botkit :: BOTKIT, err :: EXCEPTION | e) Unit
on c evs handler =
  liftEff $ runFn3
    onImpl
      c
      (map show evs)
      (\b m -> void $ launchAff $ handler b m)

hears
  :: forall m e. (IsControllerMode m) =>
    Controller m ->
    Array Pattern ->
    Array Event ->
    (RawBot -> RawMessage -> Aff (botkit :: BOTKIT | e) Unit) ->
    Aff (botkit :: BOTKIT, err :: EXCEPTION | e) Unit
hears c ps evs handler =
  liftEff $ runFn4
    hearsImpl
      c
      (map unwrap ps)
      (map show evs)
      (\b m -> void $ launchAff $ handler b m)

foreign import createSlackBotImpl
  :: forall e. BotConfig -> Eff (botkit :: BOTKIT | e) (Controller BotMode)

foreign import toSlackAppImpl
  :: forall e. AppConfig -> Controller BotMode -> Eff (botkit :: BOTKIT | e) (Controller AppMode)

foreign import setupWebserverImpl
  :: forall e.
    Fn4
      (Controller AppMode)
      Port
      (Error -> Eff (botkit :: BOTKIT | e) Unit)
      (Server -> Eff (botkit :: BOTKIT | e) Unit)
      (Eff (botkit :: BOTKIT | e) Unit)

foreign import createWebhookEndpointsImpl
  :: forall e.
    Controller AppMode -> Server -> Eff (botkit :: BOTKIT | e) Unit

foreign import createOauthEndpointsImpl
  :: forall e.
    Fn4
      (Controller AppMode)
      Server
      (Error -> Request -> Response -> ExpressM e Unit -> ExpressM e Unit)
      (Request -> Response -> ExpressM e Unit -> ExpressM e Unit)
      (Eff (botkit :: BOTKIT | e) Unit)

foreign import onImpl
  :: forall m e. (IsControllerMode m) =>
    Fn3
      (Controller m)
      (Array String)
      (RawBot -> RawMessage -> Eff (botkit :: BOTKIT | e) Unit)
      (Eff (botkit :: BOTKIT | e) Unit)

foreign import hearsImpl
  :: forall m e. (IsControllerMode m) =>
    Fn4
      (Controller m)
      (Array String)
      (Array String)
      (RawBot -> RawMessage -> Eff (botkit :: BOTKIT | e) Unit)
      (Eff (botkit :: BOTKIT | e) Unit)
