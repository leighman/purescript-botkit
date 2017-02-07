module Botkit.Slack.Types
  ( BOTKIT
  , class IsControllerMode
  , AppMode
  , BotMode
  , Controller

  , RawAttachment
  , RawBot
  , RawController
  , RawMessage
  , RawReply
  ) where

foreign import data BOTKIT :: !

foreign import data RawBot :: *
foreign import data RawController :: *
foreign import data RawMessage :: *

foreign import data RawReply :: *
foreign import data RawAttachment :: *

newtype Controller a = Controller RawController

data AppMode
data BotMode

class IsControllerMode a

instance isControllerModeAppMode :: IsControllerMode AppMode
instance isControllerModeBotMode :: IsControllerMode BotMode
