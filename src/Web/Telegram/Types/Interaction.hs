{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | User interactions: customized keyboards, clickable buttons, popups and inline displays
module Web.Telegram.Types.Interaction
  ( -- ** queries
    CallbackQuery (..),
    ShippingQuery (..),
    PreCheckoutQuery (..),

    -- ** replys
    ReplyKeyboardMarkup (..),
    KeyboardButton (..),
    KeyboardButtonAddon (..),
    KeyboardButtonPollType (..),
    ReplyKeyboardRemove (..),
    InlineKeyboardMarkup (..),
    InlineKeyboardButton (..),
    ForceReply (..),
    LoginUrl (..),
    ReplyMarkup,
    Action (..),
  )
where

import Data.Aeson
import Data.OpenUnion
import Deriving.Aeson
import Servant.API
import Web.Telegram.Types.Internal.Common
import Web.Telegram.Types.Internal.Keyboard
import Web.Telegram.Types.Internal.Utils

newtype ReplyMarkup
  = ReplyMarkup
      ( Union
          '[ InlineKeyboardMarkup,
             ReplyKeyboardMarkup,
             ReplyKeyboardRemove,
             ForceReply
           ]
      )
  deriving (Show, Eq, Generic, Default)
instance ToJSON ReplyMarkup where
  toJSON (ReplyMarkup v) =
    ( (\(inlineM :: InlineKeyboardMarkup) -> toJSON inlineM)
        @> (\(replyM :: ReplyKeyboardMarkup) -> toJSON replyM)
        @> (\(replyR :: ReplyKeyboardRemove) -> toJSON replyR)
        @> (\(forceR :: ForceReply) -> toJSON forceR)
        @> typesExhausted
    )
      v

deriving via Serialize ReplyMarkup instance ToHttpApiData ReplyMarkup

data Action
  = Typing
  | UploadPhoto
  | RecordVideo
  | UploadVideo
  | RecordAudio
  | UploadAudio
  | UploadDocument
  | FindLocation
  | RecordVideoNote
  | UploadVideoNote
  deriving (Show, Eq, Ord, Generic, Default)
  deriving (ToJSON, FromJSON) via Snake Action
  deriving (ToHttpApiData) via Serialize Action
