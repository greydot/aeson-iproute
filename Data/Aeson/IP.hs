module Data.Aeson.IP () where

import Data.Aeson
import Data.Aeson.Types
import Data.IP
import qualified Data.Text as Text
import Text.Read (readMaybe)

instance FromJSON IPv4 where
    parseJSON (String s)
        | Just r <- readMaybe (Text.unpack s) = pure r
        | otherwise = fail "Unable to parse"
    parseJSON v = typeMismatch "IPv4" v

instance ToJSON IPv4 where
    toJSON = String . Text.pack . show

instance FromJSON IPv6 where
    parseJSON (String s)
        | Just r <- readMaybe (Text.unpack s) = pure r
        | otherwise = fail "Unable to parse"
    parseJSON v = typeMismatch "IPv6" v

instance ToJSON IPv6 where
    toJSON = String . Text.pack . show

instance FromJSON IP where
    parseJSON (String s)
        | Just r <- readMaybe (Text.unpack s) = pure r
        | otherwise = fail "Unable to parse"
    parseJSON v = typeMismatch "IP" v

instance ToJSON IP where
    toJSON = String . Text.pack . show

instance FromJSON IPRange where
    parseJSON (String s)
        | Just r <- readMaybe (Text.unpack s) = pure r
        | otherwise = fail "Unable to parse"
    parseJSON v = typeMismatch "IPRange" v

instance ToJSON IPRange where
    toJSON = String . Text.pack . show

