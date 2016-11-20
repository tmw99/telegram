{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Network.Telegram.RequestTypes where

import Data.Default
import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Functor

type Token = String


class URLParams a where
    params :: a -> [(String, String)]

-- GetUpdates
data GetUpdatesParams = GetUpdatesParams
    { getUpdatesOffset :: Maybe Int
    , getUpdatesLimit :: Maybe Int
    , getUpdatesTimeout :: Maybe Int
    } deriving Show

instance Default GetUpdatesParams where
    def = GetUpdatesParams
        { getUpdatesOffset = Nothing
        , getUpdatesLimit = Nothing
        , getUpdatesTimeout = Nothing
        }

instance URLParams GetUpdatesParams where
    params p = catMaybes mparams
        where mparams = [ createParam "offset" $ encode' <$> getUpdatesOffset p
                        , createParam "limit" $ encode' <$> getUpdatesLimit p
                        , createParam "timeout" $ encode' <$> getUpdatesTimeout p
                        ]

-- SendMessage
data SendMessageParams = SendMessageParams
    { sendMessageChatId :: Int
    , sendMessageText :: String
    , sendMessageParseMode :: ParseMode
    , sendMessageDisableWebPagePreview :: Maybe Bool
    , sendMessageDisableNotification :: Maybe Bool
    , sendMessageReplyToMessageId :: Maybe Int
    , sendMessageReplyMarkup :: Maybe Markup
    } deriving Show

instance URLParams SendMessageParams where
    params p = catMaybes
        [ createParam "chat_id" $ Just $ encode' $ sendMessageChatId p
        , createParam "text" $ Just $ sendMessageText p
        , createParam "parse_mode" $ f $ sendMessageParseMode p
        , createParam "disable_web_page_preview" $
            encode' <$> sendMessageDisableWebPagePreview p
        , createParam "disable_notification" $
            encode' <$> sendMessageDisableNotification p
        , createParam "reply_to_message_id" $
            encode' <$> sendMessageReplyToMessageId p
        , createParam "reply_markup" $
            encode' <$> sendMessageReplyMarkup p
        ]
      where f ParseModeMarkdown = Just "Markdown"
            f ParseModeHTML = Just "HTML"
            f ParseModeNormal = Nothing

data ParseMode = ParseModeMarkdown
               | ParseModeHTML
               | ParseModeNormal
               deriving Show

-- SendChatAction
data SendChatActionParams = SendChatActionParams
    { sendChatActionChatId :: Int
    , sendChatActionAction :: ChatAction
    } deriving Show

instance URLParams SendChatActionParams where
    params p = catMaybes
        [ createParam "chat_id" $ Just $ encode' $ sendChatActionChatId p
        , createParam "action" $ Just $ f $ sendChatActionAction p
        ]
        where f ChatActionTyping         = "typing"
              f ChatActionUploadPhoto    = "upload_photo"
              f ChatActionRecordVideo    = "record_video"
              f ChatActionUploadVideo    = "upload_video"
              f ChatActionRecordAudio    = "record_audio"
              f ChatActionUploadAudio    = "upload_audio"
              f ChatActionUploadDocument = "upload_document"
              f ChatActionFindLocation   = "find_Location"

data ChatAction = ChatActionTyping
                | ChatActionUploadPhoto
                | ChatActionRecordVideo
                | ChatActionUploadVideo
                | ChatActionRecordAudio
                | ChatActionUploadAudio
                | ChatActionUploadDocument
                | ChatActionFindLocation
                deriving Show

-- ReplyKeyboardMarkup
data Markup = ReplyKeyboardMarkup
    { replyKeyboardMarkupKeyboard :: [[String]]
    , replyKeyboardMarkupResizeKeyboard :: Bool
    , replyKeyboardMarkupOneTimeKeyboard :: Bool
    , replyKeyboardMarkupSelective :: Bool
    }
            | ReplyKeyboardRemove
    { replyKeyboardRemoveHideKeyboard :: Bool
    , replyKeyboardRemoveSelective :: Bool
    }
            | ForceReply
    { forceReplyForceReply :: Bool
    , forceReplySelective :: Bool
    } deriving Show

instance ToJSON Markup where
    toJSON p@(ReplyKeyboardMarkup _ _ _ _) = object
       [ "keyboard" .=  replyKeyboardMarkupKeyboard p
       , "resize_keyboard" .= replyKeyboardMarkupResizeKeyboard p
       , "one_time_keyboard" .= replyKeyboardMarkupOneTimeKeyboard p
       , "selective" .= replyKeyboardMarkupSelective p
       ]
    toJSON p@(ReplyKeyboardRemove _ _) = object
       [ "hide_keyboard" .= replyKeyboardRemoveHideKeyboard p
       , "selective" .= replyKeyboardRemoveSelective p
       ]
    toJSON p@(ForceReply _ _) = object
       [ "force_reply" .= forceReplyForceReply p
       , "selective" .= forceReplySelective p
       ]

encode' :: ToJSON a => a -> String
encode' = toString . encode

createParam :: String -> Maybe String -> Maybe (String, String)
createParam key value = (key,) <$> value
