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

data SendMessageParams = SendMessageParams
    { sendMessageChatId :: Int
    , sendMessageText :: String
    , sendMessageParseMode :: Maybe String
    , sendMessageDisableWebPagePreview :: Maybe Bool
    , sendMessageReplyToMessageId :: Maybe Int
    , sendMessageReplyMarkup :: Maybe Markup
    } deriving Show

instance URLParams SendMessageParams where
    params p = catMaybes
        [ createParam "chat_id" $ Just $ encode' $ sendMessageChatId p
        , createParam "text" $ Just $ sendMessageText p
        , createParam "parse_mode" $ sendMessageParseMode p
        , createParam "disable_web_page_preview" $
            encode' <$> sendMessageDisableWebPagePreview p
        , createParam "reply_to_message_id" $
            encode' <$> sendMessageReplyToMessageId p
        , createParam "reply_markup" $
            encode' <$> sendMessageReplyMarkup p
        ]


data Markup = ReplyKeyboardMarkup
    { replyKeyboardMarkupKeyboard :: [[String]]
    , replyKeyboardMarkupResizeKeyboard :: Maybe Bool
    , replyKeyboardMarkupOneTimeKeyboard :: Maybe Bool
    , replyKeyboardMarkupSelective :: Maybe Bool
    }                    | ReplyKeyboardHide
    { replyKeyboardHideHideKeyboard :: Bool
    , replyKeyboardHideSelective :: Maybe Bool
    }                    | ForceReply
    { forceReplyForceReply :: Bool
    , forceReplySelective :: Maybe Bool
    } deriving Show

instance ToJSON Markup where
    toJSON (ReplyKeyboardMarkup keyboard
                                resizeKeyboard
                                oneTimeKeyboard
                                selective)      = object $ catMaybes $
                                  Just ("keyboard"           .= keyboard):
                                  (if isNothing resizeKeyboard then Nothing else
                                  Just $ "resize_keyboard"   .= resizeKeyboard):
                                  (if isNothing oneTimeKeyboard then Nothing else
                                  Just $ "one_time_keyboard" .= oneTimeKeyboard):
                                  (if isNothing selective then Nothing else
                                  Just $ "selective"         .= selective):
                                  []
    toJSON (ReplyKeyboardHide   hide
                                selective)     = object $ catMaybes $
                                  Just ("hide_keyboard" .= hide):
                                  (if isNothing selective then Nothing else
                                  Just $ "selective"    .= selective):
                                  []
    toJSON (ForceReply          forceReply
                                selective)     = object $ catMaybes $
                                  Just ("force_reply" .= forceReply):
                                  (if isNothing selective then Nothing else
                                  Just $ "selective"   .= selective):
                                  []

encode' :: ToJSON a => a -> String
encode' = toString . encode

createParam :: String -> Maybe String -> Maybe (String, String)
createParam key value = (key,) <$> value
