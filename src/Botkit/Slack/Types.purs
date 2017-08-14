module Botkit.Slack.Types
  ( BOTKIT
  , class IsControllerMode
  , AppMode
  , BotMode
  , Controller

  , RawChannel
  , RawTeam
  , RawUser
  , ChannelId(..)
  , TeamId(..)
  , UserId(..)

  , RawAttachment
  , RawBot
  , RawController
  , RawMessage
  , RawReply
  , RawStorage

  , toRawController
  ) where

import Control.Monad.Eff (kind Effect)

foreign import data BOTKIT :: Effect

foreign import data RawBot :: Type
foreign import data RawController :: Type
foreign import data RawMessage :: Type

foreign import data RawStorage :: Type
foreign import data RawChannel :: Type
foreign import data RawUser :: Type
foreign import data RawTeam :: Type

newtype ChannelId = ChannelId String
newtype TeamId = TeamId String
newtype UserId = UserId String

foreign import data RawReply :: Type
foreign import data RawAttachment :: Type

newtype Controller a = Controller RawController

toRawController :: forall a. IsControllerMode a => Controller a -> RawController
toRawController (Controller r) = r

data AppMode
data BotMode

class IsControllerMode a

instance isControllerModeAppMode :: IsControllerMode AppMode
instance isControllerModeBotMode :: IsControllerMode BotMode
