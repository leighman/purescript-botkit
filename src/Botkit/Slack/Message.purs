module Botkit.Slack.Message where

import Data.Maybe (Maybe(..))

import Botkit.Slack.Types (RawMessage)

text :: RawMessage -> Maybe String
text = messageGet "text"

callbackId :: RawMessage -> Maybe String
callbackId = messageGet "callback_id"

command :: RawMessage -> Maybe String
command = messageGet "command"

user :: RawMessage -> Maybe String
user = messageGet "user"

messageGet :: String -> RawMessage -> Maybe String
messageGet = messageGetImpl Nothing Just

foreign import messageGetImpl
  :: forall a.
    Maybe a ->
    (a -> Maybe a) ->
    String ->
    RawMessage ->
    Maybe String
