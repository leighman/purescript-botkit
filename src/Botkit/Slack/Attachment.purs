module Botkit.Slack.Attachment
  ( class IsValue
  , Attachment
  , Field(..)
  , Property
  , PropertyKey
  , PropertyValue
  , property
  , (:=)

  , authorName
  , authorLink
  , authorIcon
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

import Prelude ((<<<))
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

import Botkit.Slack.Types (RawAttachment)

type Attachment = Array Property

data PropertyValue = I Int | S String | F (Array Field)

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

key :: forall a. String -> PropertyKey a
key = PropertyKey <<< S

authorName :: PropertyKey String
authorName = key "author_name"

authorLink :: PropertyKey String
authorLink = key "author_link"

authorIcon :: PropertyKey String
authorIcon = key "author_icon"

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

toRawAttachment :: Attachment -> RawAttachment
toRawAttachment = toRawAttachmentImpl unsafeUnwrap

foreign import toRawAttachmentImpl
  :: (forall a. PropertyValue -> a) -> Attachment -> RawAttachment
