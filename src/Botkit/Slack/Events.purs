module Botkit.Slack.Events where

import Prelude (class Show)

data Event
  = CreateBot
  | DirectMention
  | DirectMessage
  | InteractiveMessageCallback
  | Mention
  | RTMClose
  | RTMOpen
  | SlashCommand

instance showEvent :: Show Event where
  show = toJsString

toJsString :: Event -> String
toJsString CreateBot = "create_bot"
toJsString DirectMention = "direct_mention"
toJsString DirectMessage = "direct_message"
toJsString InteractiveMessageCallback = "interactive_message_callback"
toJsString Mention = "mention"
toJsString RTMClose = "rtm_close"
toJsString RTMOpen = "rtm_open"
toJsString SlashCommand = "slash_command"
