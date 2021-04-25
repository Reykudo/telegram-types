{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Telegram.Types.Internal.Common where

import Control.Applicative
import Data.Aeson
import Data.Int
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Deriving.Aeson
import Servant.API
import Web.Telegram.Types.Internal.Keyboard
import Web.Telegram.Types.Internal.Media
import Web.Telegram.Types.Internal.Passport
import Web.Telegram.Types.Internal.Sticker
import Web.Telegram.Types.Internal.User
import Web.Telegram.Types.Internal.Utils

data ChatType
  = Private
  | Group
  | Supergroup
  | Channel
  deriving (Show, Eq, Ord, Enum, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[SumUntaggedValue, ConstructorTagModifier CamelToSnake] ChatType
  deriving (ToHttpApiData) via Serialize ChatType

-- | https://core.telegram.org/bots/api#chat
data Chat
  = Chat
      { chatId :: Int64,
        chatType :: ChatType,
        title :: Maybe Text,
        username :: Maybe Text,
        firstName :: Maybe Text,
        lastName :: Maybe Text,
        photo :: Maybe ChatPhoto,
        description :: Maybe Text,
        inviteLink :: Maybe Text,
        pinnedMessage :: Maybe Message,
        permissions :: Maybe ChatPermissions,
        slowModeDelay :: Maybe Int,
        stickerSetName :: Maybe Int,
        canSetStickerSet :: Maybe Bool
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake "chat" Chat
  deriving (ToHttpApiData) via Serialize Chat

data Message
  = Msg
      { metadata :: MessageMetadata,
        content :: MessageContent
      }
  deriving (Show, Eq, Generic, Default)
  deriving (ToHttpApiData) via Serialize Message

instance FromJSON Message where
  parseJSON o = do
    metadata <- parseJSON o
    content <- parseJSON o
    return Msg {..}

instance ToJSON Message where
  toJSON Msg {..} =
    let Object hm1 = toJSON metadata
        Object hm2 = toJSON content
     in Object (hm1 <> hm2)

-- |
data MessageMetadata
  = MMetadata
      { messageId :: Int,
        from :: Maybe User,
        date :: POSIXTime,
        chat :: Chat,
        forwardFrom :: Maybe User,
        forwardFromChat :: Maybe Chat,
        forwardFromMessageId :: Maybe Int,
        forwardSignature :: Maybe Text,
        forwardSenderName :: Maybe Text,
        forwardDate :: Maybe POSIXTime,
        replyToMessage :: Maybe Message,
        editDate :: Maybe POSIXTime,
        mediaGroupId :: Maybe Text,
        authorSignature :: Maybe Text,
        replyMarkup :: Maybe InlineKeyboardMarkup
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[SumUntaggedValue, FieldLabelModifier CamelToSnake] MessageMetadata
  deriving (ToHttpApiData) via Serialize MessageMetadata

data MessageContent
  = TextM
      { text :: Text,
        entities :: Maybe [MessageEntity]
      }
  | AudioM
      { audio :: Audio,
        caption :: Maybe Text,
        captionEntities :: Maybe [MessageEntity]
      }
  | DocumentM
      { document :: Document,
        caption :: Maybe Text,
        captionEntities :: Maybe [MessageEntity]
      }
  | AnimationM
      { animation :: Animation
      }
  | GameM
      { game :: Game
      }
  | PhotoM
      { photo :: [PhotoSize],
        caption :: Maybe Text,
        captionEntities :: Maybe [MessageEntity]
      }
  | StickerM
      { sticker :: Sticker
      }
  | VideoM
      { video :: Video,
        caption :: Maybe Text,
        captionEntities :: Maybe [MessageEntity]
      }
  | VoiceM
      { voice :: Voice,
        caption :: Maybe Text,
        captionEntities :: Maybe [MessageEntity]
      }
  | VideoNoteM
      { videoNote :: VideoNote
      }
  | ContactM
      { contact :: Contact
      }
  | LocationM
      { location :: Location
      }
  | VenueM
      { venue :: Venue
      }
  | PollM
      { poll :: Poll
      }
  | NewChatMembers
      { newChatMembers :: [User]
      }
  | LeftChatMember
      { leftChatMember :: User
      }
  | NewChatPhoto
      { newChatPhoto :: [PhotoSize]
      }
  | DeleteChatPhoto
      { deleteChatPhoto :: Bool
      }
  | GroupChatCreated
      { groupChatCreated :: Bool
      }
  | SupergroupChatCreated
      { supergroupChatCreated :: Bool
      }
  | ChannelChatCreated
      { channelChatCreated :: Bool
      }
  | MigrateToChatId
      { migrateToChatId :: Int64
      }
  | MigrateFromChatId
      { migrateFromChatId :: Int64
      }
  | PinnedMessage
      { pinnedMessage :: Message
      }
  | InvoiceM
      { invoice :: Invoice
      }
  | SuccessfulPaymentM
      { successfulPayment :: SuccessfulPayment
      }
  | ConnectedWebsite
      { connectedWebsite :: Text
      }
  | PassportData
      { passPortData :: PassportData
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[SumUntaggedValue, FieldLabelModifier CamelToSnake] MessageContent
  deriving (ToHttpApiData) via Serialize MessageContent

data MessageEntityType
  = Mention
  | Hashtag
  | Cashtag
  | BotCommand
  | Url
  | Email
  | PhoneNumber
  | Bold
  | Italic
  | Underline
  | Strikethrough
  | Code
  | Pre
  | TextLink
  | TextMention
  deriving (Show, Eq, Ord, Enum, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[SumUntaggedValue, ConstructorTagModifier CamelToSnake] MessageEntityType
  deriving (ToHttpApiData) via Serialize MessageEntityType

data MessageEntity
  = MessageEntity
      { entityType :: MessageEntityType,
        offset :: Int,
        length :: Int,
        url :: Maybe Text,
        user :: Maybe User,
        language :: Maybe Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake "entity" MessageEntity
  deriving (ToHttpApiData) via Serialize MessageEntity

data CallbackQuery
  = CBQuery
      { callbackId :: Text,
        from :: User,
        message :: Message,
        inlineMessageId :: Maybe Text,
        chatInstance :: Text,
        callbackData :: Maybe Text,
        gameShortName :: Maybe Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake "callback" CallbackQuery
  deriving (ToHttpApiData) via Serialize CallbackQuery

data ChatPhoto
  = ChatPhoto
      { smallFileId :: Text,
        smallFileUniqueId :: Text,
        bigFileId :: Text,
        bitFileUniqueId :: Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake ChatPhoto
  deriving (ToHttpApiData) via Serialize ChatPhoto

data ChatStatus
  = Creator
  | Administrator
  | Member
  | Restricted
  | Left
  | Kicked
  deriving (Show, Eq, Ord, Enum, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[SumUntaggedValue, ConstructorTagModifier CamelToSnake] ChatStatus
  deriving (ToHttpApiData) via Serialize ChatStatus

data ChatMember
  = ChatMember
      { user :: User,
        status :: ChatStatus,
        customTitle :: Maybe Text,
        untilDate :: Maybe POSIXTime,
        canBeEdited :: Maybe Bool,
        canPostMessages :: Maybe Bool,
        canEditMessages :: Maybe Bool,
        canDeleteMessages :: Maybe Bool,
        canRestrictMembers :: Maybe Bool,
        canPromoteMembers :: Maybe Bool,
        canChangeInfo :: Maybe Bool,
        canInviteUsers :: Maybe Bool,
        canPinMessages :: Maybe Bool,
        isMember :: Maybe Bool,
        canSendMessages :: Maybe Bool,
        canSendMediaMessages :: Maybe Bool,
        canSendPolls :: Maybe Bool,
        canSendOtherMesssages :: Maybe Bool,
        canAddWebPagePreviews :: Maybe Bool
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake ChatMember
  deriving (ToHttpApiData) via Serialize ChatMember

data ChatPermissions
  = ChatPermissions
      { canSendMessages :: Maybe Bool,
        canSendMediaMessages :: Maybe Bool,
        canSendPolls :: Maybe Bool,
        canSendOtherMesssages :: Maybe Bool,
        canAddWebPagePreviews :: Maybe Bool,
        canChangeInfo :: Maybe Bool,
        canInviteUsers :: Maybe Bool,
        canPinMessages :: Maybe Bool
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake ChatPermissions
  deriving (ToHttpApiData) via Serialize ChatPermissions

data BotCommand
  = BC
      { command :: Text,
        description :: Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via OmitNothing BotCommand
  deriving (ToHttpApiData) via Serialize BotCommand

data LabeledPrice
  = LabeledPrice
      { label :: Text,
        amount :: Int
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via OmitNothing LabeledPrice
  deriving (ToHttpApiData) via Serialize LabeledPrice

data Invoice
  = Invoice
      { title :: Text,
        description :: Text,
        startParameter :: Text,
        currency :: Text,
        totalAmount :: Int
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake Invoice
  deriving (ToHttpApiData) via Serialize Invoice

data ShippingAddress
  = ShippingAddress
      { countryCode :: Text,
        state :: Text,
        city :: Text,
        streetLine1 :: Text,
        streetLine2 :: Text,
        postCode :: Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake ShippingAddress
  deriving (ToHttpApiData) via Serialize ShippingAddress

data OrderInfo
  = OrderInfo
      { name :: Maybe Text,
        phoneNumber :: Maybe Text,
        email :: Maybe Text,
        shippingAddress :: ShippingAddress
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake OrderInfo
  deriving (ToHttpApiData) via Serialize OrderInfo

data ShippingOption
  = ShippingOption
      { optionId :: Text,
        title :: Text,
        prices :: Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake "option" ShippingOption
  deriving (ToHttpApiData) via Serialize ShippingOption

data SuccessfulPayment
  = SuccessfulPayment
      { currency :: Text,
        totalAmount :: Int,
        invoicePayload :: Text,
        shippingOptionId :: Maybe Text,
        orderInfo :: Maybe OrderInfo,
        telegramPaymentChargeId :: Text,
        providerPaymentChargeId :: Text
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake SuccessfulPayment
  deriving (ToHttpApiData) via Serialize SuccessfulPayment

data ShippingQuery
  = SQuery
      { queryId :: Text,
        from :: User,
        invoicePayload :: Text,
        shippingAddress :: ShippingAddress
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake "query" ShippingQuery
  deriving (ToHttpApiData) via Serialize ShippingQuery

data PreCheckoutQuery
  = PCQuery
      { queryId :: Text,
        from :: User,
        currency :: Text,
        totalAmount :: Int,
        invoicePayload :: Text,
        shippingOptionId :: Maybe String,
        orderInfo :: Maybe OrderInfo
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake "query" PreCheckoutQuery
  deriving (ToHttpApiData) via Serialize PreCheckoutQuery

data Game
  = Game
      { title :: Text,
        description :: Text,
        photo :: [PhotoSize],
        text :: Maybe Text,
        textEntities :: Maybe MessageEntity,
        animation :: Animation
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake Game
  deriving (ToHttpApiData) via Serialize Game

data CallbackGame

data GameHighScore
  = GameHighScore
      { position :: Int,
        user :: User,
        score :: Int
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via OmitNothing GameHighScore
  deriving (ToHttpApiData) via Serialize GameHighScore

data ResponseParameters
  = ResponseParameters
      { migrateToChatId :: Maybe Int64,
        retryAfter :: Maybe Int
      }
  deriving (Show, Eq, Generic, Default)
  deriving
    (FromJSON, ToJSON)
    via Snake ResponseParameters
  deriving (ToHttpApiData) via Serialize ResponseParameters

newtype ReqResult a
  = Ok a
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (ReqResult a) where
  parseJSON = withObject "request result" $ \o -> do
    a <- o .: "result"
    a' <- parseJSON a
    return $ Ok a'

data ReqEither a b
  = LLL a
  | RRR b
  deriving (Show, Eq, Generic)

instance (FromJSON a, FromJSON b) => FromJSON (ReqEither a b) where
  parseJSON o = LLL <$> parseJSON o <|> RRR <$> parseJSON o
