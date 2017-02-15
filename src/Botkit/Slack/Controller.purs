module Botkit.Slack.Controller where

import Prelude

import Control.Monad.Aff (Aff, launchAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Newtype (unwrap)
import Data.String (Pattern)
import Node.Express.Handler (Handler, runHandlerM) as E
import Node.Express.Types (ExpressM, Request, Response)
import Node.HTTP (Server)

import Botkit.Slack.Events (Event)
import Botkit.Slack.Handler (Handler, runHandlerM)
import Botkit.Slack.Types (BOTKIT, class IsControllerMode, AppMode, BotMode, Controller, RawBot, RawMessage)

data AppM e m a = AppM (Controller m -> Aff e a)

runAppM :: forall e m a. AppM e m a -> Controller m -> Aff e a
runAppM (AppM f) = f

instance functorAppM :: Functor (AppM e m) where
  map f (AppM g) = AppM \c -> map f (g c)

instance applyAppM :: Apply (AppM e m) where
  apply (AppM f) (AppM v) =
    AppM \c -> f c <*> v c

instance applicativeAppM :: Applicative (AppM e m) where
  pure x = AppM \_ -> pure x

instance bindAppM :: Bind (AppM e m) where
  bind (AppM f) k = AppM \c -> do
    f c >>= \a -> case k a of AppM g -> g c

instance monadAppM :: Monad (AppM e m)

instance monadEffAppM :: MonadEff e (AppM e m) where
  liftEff a = AppM \_ -> liftEff a

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
    Port ->
    AppM (botkit :: BOTKIT | e) AppMode Server
setupWebserver port = AppM \c ->
  makeAff \onFailure onSuccess ->
    runFn4 setupWebserverImpl c port onFailure onSuccess

createWebhookEndpoints :: forall e. Server -> AppM (botkit :: BOTKIT | e) AppMode Unit
createWebhookEndpoints s = AppM \c ->
  liftEff $ createWebhookEndpointsImpl c s

createOauthEndpoints
  :: forall e.
    Server ->
    (E.Handler e) ->
    (Error -> E.Handler e) ->
    AppM (botkit :: BOTKIT | e) AppMode Unit
createOauthEndpoints s onSuccess onFailure = AppM \c ->
  liftEff $
    runFn4
      createOauthEndpointsImpl
      c
      s
      (E.runHandlerM <<< onFailure)
      (E.runHandlerM onSuccess)

on
  :: forall m e. (IsControllerMode m) =>
    Array Event ->
    Handler e ->
    AppM (botkit :: BOTKIT, err :: EXCEPTION | e) m Unit
on evs handler = AppM \c ->
  liftEff $ runFn3
    onImpl
      c
      (map show evs)
      (\b m -> void $ launchAff $ runHandlerM handler b m)

hears
  :: forall m e. (IsControllerMode m) =>
    Array Pattern ->
    Array Event ->
    Handler e ->
    AppM (botkit :: BOTKIT, err :: EXCEPTION | e) m Unit
hears ps evs handler = AppM \c ->
  liftEff $ runFn4
    hearsImpl
      c
      (map unwrap ps)
      (map show evs)
      (\b m -> void $ launchAff $ runHandlerM handler b m)

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
    Controller AppMode ->
    Server ->
    Eff (botkit :: BOTKIT | e) Unit

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
