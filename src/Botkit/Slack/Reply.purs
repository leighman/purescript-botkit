module Botkit.Slack.Reply
  ( Reply
  , stringReply
  , singleAttachment
  , multipleAttachments
  , singleAttachmentWithText
  , multipleAttachmentsWithText

  , toRawReply
  ) where

import Prelude (($), (<$>))
import Data.Maybe (Maybe(..), isJust)
import Unsafe.Coerce (unsafeCoerce)

import Botkit.Slack.Attachment (Attachment, toRawAttachment)
import Botkit.Slack.Types (RawReply)

data Reply
  = StringReply String
  | ComplexReply
    { text :: Maybe String
    , attachments :: Array Attachment
    }

stringReply :: String -> Reply
stringReply = StringReply

singleAttachment :: Attachment -> Reply
singleAttachment a = ComplexReply {text: Nothing, attachments: [a]}

multipleAttachments :: Array Attachment -> Reply
multipleAttachments as = ComplexReply {text: Nothing, attachments: as}

singleAttachmentWithText :: String -> Attachment -> Reply
singleAttachmentWithText str a = ComplexReply {text: Just str, attachments: [a]}

multipleAttachmentsWithText :: String -> Array Attachment -> Reply
multipleAttachmentsWithText str as = ComplexReply {text: Just str, attachments: as}

toRawReply :: Reply -> RawReply
toRawReply (StringReply str) = unsafeCoerce $ str
toRawReply (ComplexReply {text, attachments}) =
  if isJust text
    then
      unsafeCoerce $
        { text
        , attachments: toRawAttachment <$> attachments
        }
    else
      unsafeCoerce $ { attachments: toRawAttachment <$> attachments }
