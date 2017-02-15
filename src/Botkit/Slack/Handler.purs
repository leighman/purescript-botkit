module Botkit.Slack.Handler
  ( Handler
  , HandlerM(..)
  , runHandlerM
  , message
  , reply
  , replyAcknowledge
  , replyInteractive
  , replyPublic
  , replyPrivate
  , replyPublicDelayed
  , replyPrivateDelayed
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)

import Botkit.Slack.Types (BOTKIT, RawBot, RawMessage)

data HandlerM e a = HandlerM (RawBot -> RawMessage -> Aff e a)

type Handler e = HandlerM (botkit :: BOTKIT | e) Unit

runHandlerM :: forall e a. HandlerM e a -> RawBot -> RawMessage -> Aff e a
runHandlerM (HandlerM f) = f

instance functorHandlerM :: Functor (HandlerM e) where
  map f (HandlerM g) = HandlerM \b m ->
    map f (g b m)

instance applyHandlerM :: Apply (HandlerM e) where
  apply (HandlerM f) (HandlerM v) =
    HandlerM \b m -> f b m <*> v b m

instance applicativeHandlerM :: Applicative (HandlerM e) where
  pure x = HandlerM \_ _ -> pure x

instance bindHandlerM :: Bind (HandlerM e) where
  bind (HandlerM f) k = HandlerM \b m ->
    f b m >>= \a -> case k a of HandlerM g -> g b m

instance monadHandlerM :: Monad (HandlerM e)

instance monadEffHandlerM :: MonadEff e (HandlerM e) where
  liftEff a = HandlerM \_ _ -> liftEff a

instance monadAffHandlerM :: MonadAff e (HandlerM e) where
  liftAff a = HandlerM \_ _ -> a

message :: forall e. HandlerM e RawMessage
message = HandlerM \b m -> pure m

reply :: forall e. String -> Handler e
reply = replyType "reply"

replyAcknowledge :: forall e. Handler e
replyAcknowledge = HandlerM \b _ ->
  liftEff $ replyAcknowledgeImpl b

replyPublic :: forall e. String -> Handler e
replyPublic = replyType "replyPublic"

replyPrivate :: forall e. String -> Handler e
replyPrivate = replyType "replyPrivate"

replyInteractive :: forall e. String -> Handler e
replyInteractive = replyType "replyInteractive"

replyPublicDelayed :: forall e. String -> Handler e
replyPublicDelayed = replyType "replyPublicDelayed"

replyPrivateDelayed :: forall e. String -> Handler e
replyPrivateDelayed = replyType "replyPrivateDelayed"

replyType :: forall e. String -> String -> Handler e
replyType t r = HandlerM \b m ->
  liftEff $ replyTypeImpl t b m r

foreign import replyTypeImpl
  :: forall e.
    String -> RawBot -> RawMessage -> String -> Eff (botkit :: BOTKIT | e) Unit

foreign import replyAcknowledgeImpl
  :: forall e.
    RawBot -> Eff (botkit :: BOTKIT | e) Unit
