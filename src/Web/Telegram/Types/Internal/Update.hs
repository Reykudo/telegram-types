{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Web.Telegram.Types.Internal.Update where
import Data.Aeson
import Data.Aeson.Types
import Data.Int (Int64)
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Deriving.Aeson
import qualified Web.Telegram.Types.Internal.Common as C
import qualified Web.Telegram.Types.Internal.InlineQuery as IQ
import qualified Web.Telegram.Types.Internal.Media as M
import Web.Telegram.Types.Internal.UpdateType (UpdateType)
import Web.Telegram.Types.Internal.Utils

-- | An incoming update
data Update
  = -- | New incoming message of any kind — text, photo, sticker, etc.
    Message
      { updateId :: Int64,
        message :: C.Message
      }
  | -- | New version of a message that is known to the bot and was edited
    EditedMessage
      { updateId :: Int64,
        message :: C.Message
      }
  | -- | New incoming channel post of any kind — text, photo, sticker, etc.
    ChannelPost
      { updateId :: Int64,
        message :: C.Message
      }
  | -- | New version of a channel post that is known to the bot and was edited
    EditedChannelPost
      { updateId :: Int64,
        message :: C.Message
      }
  | -- | New incoming inline query
    InlineQuery
      { updateId :: Int64,
        iquery :: IQ.InlineQuery
      }
  | -- | The result of an inline query that was chosen by a user and sent to their chat partner. Please see our documentation on the feedback collecting for details on how to enable these updates for your bot
    ChosenInlineResult
      { updateId :: Int64,
        result :: IQ.ChosenInlineResult
      }
  | -- | New incoming callback query
    CallbackQuery
      { updateId :: Int64,
        cbquery :: C.CallbackQuery
      }
  | -- | New incoming shipping query. Only for invoices with flexible price
    ShippingQuery
      { updateId :: Int64,
        squery :: C.ShippingQuery
      }
  | -- | New incoming pre-checkout query. Contains full information about checkout
    PreCheckoutQuery
      { updateId :: Int64,
        pcquery :: C.PreCheckoutQuery
      }
  | -- | New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot
    PollUpdate
      { updateId :: Int64,
        poll :: M.Poll
      }
  | -- | A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself.
    PollAnswer
      { updateId :: Int64,
        answer :: M.PollAnswer
      }
  | Unknown {updateId :: Int64}
  deriving (Show, Eq, Generic, Default)

instance FromJSON Update where
  parseJSON = withObject "Update object" $ \o -> do
    uid <- o .: "update_id"
    let pair :: FromJSON a => (Text, Int64 -> a -> Update) -> Parser (Maybe Update)
        pair (k, c) = do
          m <- o .:? k
          return $ fmap (c uid) m
    l <-
      sequence
          [ pair ("message", Message),
            pair ("edited_message", EditedMessage),
            pair ("channel_post", ChannelPost),
            pair ("edited_channel_post", EditedChannelPost),
            pair ("inline_query", InlineQuery),
            pair ("chosen_inline_result", ChosenInlineResult),
            pair ("callback_query", CallbackQuery),
            pair ("shipping_query", ShippingQuery),
            pair ("pre_checkout_query", PreCheckoutQuery),
            pair ("poll", PollUpdate),
            pair ("poll_answer", PollAnswer),
            pure $ Just $ Unknown { updateId = uid }
          ]
    let r = getFirst $ foldMap First l
    case r of
      Nothing -> fail "Empty Message"
      Just r' -> return r'

-- | Contains information about the current status of a webhook.
data WebhookInfo = WebhookInfo
  { -- | Webhook URL, may be empty if webhook is not set up
    url :: Text,
    -- | True, if a custom certificate was provided for webhook certificate checks
    hasCustomCertificate :: Bool,
    -- | Number of updates awaiting delivery
    pendingUpdateCount :: Int,
    -- | Unix time for the most recent error that happened when trying to deliver an update via webhook
    lastErrorDate :: Maybe POSIXTime,
    -- | Error message in human-readable format for the most recent error that happened when trying to deliver an update via webhook
    lastErrorMessage :: Maybe Text,
    -- | Maximum allowed number of simultaneous HTTPS connections to the webhook for update delivery
    maxConnections :: Maybe Int,
    -- | A list of update types the bot is subscribed to. Defaults to all update types
    allowedUpdates :: Maybe [UpdateType]
  }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake WebhookInfo
