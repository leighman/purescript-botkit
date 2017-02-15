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

import Botkit.Slack.Reply (Reply, toRawReply)
import Botkit.Slack.Types (BOTKIT, RawBot, RawController, RawMessage, RawReply)

data HandlerM e a = HandlerM (RawController -> RawBot -> RawMessage -> Aff e a)

type Handler e = HandlerM (botkit :: BOTKIT | e) Unit

runHandlerM :: forall e a. HandlerM e a -> RawController -> RawBot -> RawMessage -> Aff e a
runHandlerM (HandlerM f) = f

instance functorHandlerM :: Functor (HandlerM e) where
  map f (HandlerM g) = HandlerM \c b m ->
    map f (g c b m)

instance applyHandlerM :: Apply (HandlerM e) where
  apply (HandlerM f) (HandlerM v) =
    HandlerM \c b m -> f c b m <*> v c b m

instance applicativeHandlerM :: Applicative (HandlerM e) where
  pure x = HandlerM \_ _ _ -> pure x

instance bindHandlerM :: Bind (HandlerM e) where
  bind (HandlerM f) k = HandlerM \c b m ->
    f c b m >>= \a -> case k a of HandlerM g -> g c b m

instance monadHandlerM :: Monad (HandlerM e)

instance monadEffHandlerM :: MonadEff e (HandlerM e) where
  liftEff a = HandlerM \_ _ _ -> liftEff a

instance monadAffHandlerM :: MonadAff e (HandlerM e) where
  liftAff a = HandlerM \_ _ _ -> a

message :: forall e. HandlerM e RawMessage
message = HandlerM \_ _ m -> pure m

reply
  {-- :: forall e. Warn "Unsafe. Prefer `replyXDelayed`. Do not use more than once in a Handler." => --}
  :: forall e.
    Reply -> Handler e
reply = replyType "reply"

replyAcknowledge :: forall e. Handler e
replyAcknowledge = HandlerM \_ b _ ->
  liftEff $ replyAcknowledgeImpl b

replyPublic
  {-- :: forall e. Warn "Unsafe. Prefer `replyPublicDelayed`. Do not use more than once in a Handler." => --}
  :: forall e.
    Reply -> Handler e
replyPublic = replyType "replyPublic"

replyPrivate
  {-- :: forall e. Warn "Unsafe. Prefer `replyPrivateDelayed`. Do not use more than once in a Handler." => --}
  :: forall e.
    Reply -> Handler e
replyPrivate = replyType "replyPrivate"

replyInteractive :: forall e. Reply -> Handler e
replyInteractive = replyType "replyInteractive"

replyPublicDelayed :: forall e. Reply -> Handler e
replyPublicDelayed = replyType "replyPublicDelayed"

replyPrivateDelayed :: forall e. Reply -> Handler e
replyPrivateDelayed = replyType "replyPrivateDelayed"

replyType :: forall e. String -> Reply -> Handler e
replyType t r = HandlerM \_ b m ->
  liftEff $ replyTypeImpl t b m (toRawReply r)

foreign import replyTypeImpl
  :: forall e.
    String -> RawBot -> RawMessage -> RawReply -> Eff (botkit :: BOTKIT | e) Unit

foreign import replyAcknowledgeImpl
  :: forall e.
    RawBot -> Eff (botkit :: BOTKIT | e) Unit
