module Botkit.Slack.Attachment
  ( class IsValue
  , Attachment
  , Button(..)
  , Confirm(..)
  , Field(..)
  , Property
  , PropertyKey
  , PropertyValue
  , property
  , (:=)

  , actions
  , authorName
  , authorLink
  , authorIcon
  , callbackId
  , color
  , fallback
  , fields
  , footer
  , footerIcon
  , imageUrl
  , pretext
  , text
  , thumbUrl
  , timestamp
  , title
  , titleLink

  , toValue
  , toRawAttachment
  ) where

import Prelude ((<<<), map)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

import Botkit.Slack.Types (RawAttachment)

type Attachment = Array Property

data PropertyValue = I Int | S String | F (Array Field) | B (Array Button)

toString :: Partial => PropertyValue -> String
toString (S s) = s

newtype Property = Property
  { k :: String
  , v :: PropertyValue
  }

newtype Field = Field
  { title :: String
  , value :: String
  , short :: Boolean
  }

data Button
  = NormalButton {name :: String, text :: String, value :: String}
  | PrimaryButton {name :: String, text :: String, value :: String}
  | DangerButton {name :: String, text :: String, value :: String, confirm :: Confirm}

newtype Confirm = Confirm
  { title :: String
  , text :: String
  , okText :: String
  , dismissText :: String
  }

newtype PropertyKey a = PropertyKey PropertyValue

property :: forall a. IsValue a => PropertyKey a -> a -> Property
property (PropertyKey k) v = Property {k: (unsafePartial toString) k, v: toValue v}
infix 4 property as :=

class IsValue a where
  toValue :: a -> PropertyValue

instance stringIsValue :: IsValue String where
  toValue = S

instance intIsValue :: IsValue Int where
  toValue = I

instance fieldArrayIsValue :: IsValue (Array Field) where
  toValue = F

instance buttonArrayIsValue :: IsValue (Array Button) where
  toValue = B

key :: forall a. String -> PropertyKey a
key = PropertyKey <<< S

actions :: PropertyKey (Array Button)
actions = key "actions"

authorName :: PropertyKey String
authorName = key "author_name"

authorLink :: PropertyKey String
authorLink = key "author_link"

authorIcon :: PropertyKey String
authorIcon = key "author_icon"

callbackId :: PropertyKey String
callbackId = key "callback_id"

color :: PropertyKey String
color = key "color"

fallback :: PropertyKey String
fallback = key "fallback"

fields :: PropertyKey (Array Field)
fields = key "fields"

footer :: PropertyKey String
footer = key "footer"

footerIcon :: PropertyKey String
footerIcon = key "footer_icon"

imageUrl :: PropertyKey String
imageUrl = key "image_url"

pretext :: PropertyKey String
pretext = key "pretext"

text :: PropertyKey String
text = key "text"

thumbUrl :: PropertyKey String
thumbUrl = key "thumb_url"

timestamp :: PropertyKey Int
timestamp = key "ts"

title :: PropertyKey String
title = key "title"

titleLink :: PropertyKey String
titleLink = key "title_link"

unsafeUnwrap :: forall a. PropertyValue -> a
unsafeUnwrap (I i) = unsafeCoerce i
unsafeUnwrap (S s) = unsafeCoerce s
unsafeUnwrap (F fs) = unsafeCoerce fs
unsafeUnwrap (B bs) = unsafeCoerce (map unwrapButton bs)
  where
    unwrapButton :: Button -> a
    unwrapButton b =
      case b of
        NormalButton ({name, text: t, value}) ->
          unsafeCoerce {name, text: t, value, type: "button"}
        PrimaryButton ({name, text: t, value}) ->
          unsafeCoerce {name, text: t, value, type: "button", style: "primary"}
        DangerButton ({name, text: t, value, confirm}) ->
          unsafeCoerce {name, text: t, value, type: "button", style: "danger", confirm: renderConfirmImpl confirm}

toRawAttachment :: Attachment -> RawAttachment
toRawAttachment = toRawAttachmentImpl unsafeUnwrap

foreign import toRawAttachmentImpl
  :: (forall a. PropertyValue -> a) -> Attachment -> RawAttachment

foreign import renderConfirmImpl
  :: forall a. Confirm -> a
