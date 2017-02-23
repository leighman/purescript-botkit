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

  , allChannels
  , allTeams
  , allUsers
  , getChannel
  , getTeam
  , getUser
  , saveChannel
  , saveTeam
  , saveUser
  , deleteChannel
  , deleteTeam
  , deleteUser

  ) where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

import Botkit.Slack.Reply (Reply, toRawReply)
import Botkit.Slack.Types (BOTKIT, ChannelId, RawBot, RawChannel, RawController, RawMessage, RawReply, RawTeam, RawUser, TeamId, UserId)

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

getChannel :: forall e. ChannelId -> HandlerM (botkit :: BOTKIT | e) (Maybe RawChannel)
getChannel = get "channels"

getTeam :: forall e. TeamId -> HandlerM (botkit :: BOTKIT | e) (Maybe RawTeam)
getTeam = get "teams"

getUser :: forall e. UserId -> HandlerM (botkit :: BOTKIT | e) (Maybe RawUser)
getUser = get "users"

get :: forall a e i. String -> i -> HandlerM (botkit :: BOTKIT | e) (Maybe a)
get t xid = HandlerM \c _ _ ->
  makeAff (\onError onSuccess ->
    getImpl c t xid onError (onSuccess <<< toMaybe)
  )

foreign import getImpl
  :: forall a e i.
    RawController ->
    String ->
    i ->
    (Error -> Eff (botkit :: BOTKIT | e) Unit) ->
    (Nullable a -> Eff (botkit :: BOTKIT | e) Unit) ->
    Eff (botkit :: BOTKIT | e) Unit

saveChannel :: forall e. RawChannel -> Handler e
saveChannel = save "channels"

saveTeam :: forall e. RawTeam -> Handler e
saveTeam = save "teams"

saveUser :: forall e. RawUser -> Handler e
saveUser = save "users"

deleteChannel :: forall e. ChannelId -> Handler e
deleteChannel = delete "channels"

deleteTeam :: forall e. TeamId -> Handler e
deleteTeam = delete "teams"

deleteUser :: forall e. UserId -> Handler e
deleteUser = delete "users"

save :: forall a e. String -> a -> Handler e
save t x = HandlerM \c _ _ ->
  makeAff (\onError _ ->
    saveOrDeleteImpl c t "save" x onError
  )

delete :: forall i e. String -> i -> Handler e
delete t xid = HandlerM \c _ _ ->
  makeAff (\onError onSuccess ->
    saveOrDeleteImpl c t "delete" xid onError
  )

foreign import saveOrDeleteImpl
  :: forall ai e.
    RawController ->
    String ->
    String ->
    ai ->
    (Error -> Eff (botkit :: BOTKIT | e) Unit) ->
    Eff (botkit :: BOTKIT | e) Unit

allChannels :: forall e. HandlerM (botkit :: BOTKIT | e) (Array RawChannel)
allChannels = all "channels"

allTeams :: forall e. HandlerM (botkit :: BOTKIT | e) (Array RawTeam)
allTeams = all "teams"

allUsers :: forall e. HandlerM (botkit :: BOTKIT | e) (Array RawUser)
allUsers = all "users"

all :: forall a e. String -> HandlerM (botkit :: BOTKIT | e) (Array a)
all t = HandlerM \c _ _ ->
  makeAff (\onError onSuccess ->
    allImpl c t onError onSuccess
  )

foreign import allImpl
  :: forall a e.
    RawController ->
    String ->
    (Error -> Eff (botkit :: BOTKIT | e) Unit) ->
    (a -> Eff (botkit :: BOTKIT | e) Unit) ->
    Eff (botkit :: BOTKIT | e) Unit
