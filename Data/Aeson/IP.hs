{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Aeson.IP () where

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative (pure)
#endif

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

instance FromJSONKey IPv4 where
    fromJSONKey = FromJSONKeyTextParser $ \t ->
                      case readMaybe (Text.unpack t) of
                          Just r -> pure r
                          Nothing -> fail "Unable to parse IPv4"

-- | The @ToJSON@ instance produces JSON strings matching the @Show@ instance.
-- 
-- >>> toJSON (toIPv4 [127,0,0,1])
-- String "127.0.0.1"
instance ToJSON IPv4 where
    toJSON = String . Text.pack . show

instance ToJSONKey IPv4 where
    toJSONKey = toJSONKeyText (Text.pack . show)

instance FromJSON IPv6 where
    parseJSON (String s)
        | Just r <- readMaybe (Text.unpack s) = pure r
        | otherwise = fail "Unable to parse"
    parseJSON v = typeMismatch "IPv6" v

instance FromJSONKey IPv6 where
    fromJSONKey = FromJSONKeyTextParser $ \t ->
                      case readMaybe (Text.unpack t) of
                          Just r -> pure r
                          Nothing -> fail "Unable to parse IPv6"

-- | The @ToJSON@ instance produces JSON strings matching the @Show@ instance.
-- 
-- >>> toJSON (toIPv6 [0x2001,0xDB8,0,0,0,0,0,1])
-- String "2001:db8::1"
instance ToJSON IPv6 where
    toJSON = String . Text.pack . show

instance ToJSONKey IPv6 where
    toJSONKey = toJSONKeyText (Text.pack . show)

instance FromJSON IP where
    parseJSON (String s)
        | Just r <- readMaybe (Text.unpack s) = pure r
        | otherwise = fail "Unable to parse"
    parseJSON v = typeMismatch "IP" v

instance FromJSONKey IP where
    fromJSONKey = FromJSONKeyTextParser $ \t ->
                      case readMaybe (Text.unpack t) of
                          Just r -> pure r
                          Nothing -> fail "Unable to parse IP"

instance ToJSON IP where
    toJSON = String . Text.pack . show

instance ToJSONKey IP where
    toJSONKey = toJSONKeyText (Text.pack . show)

instance Read (AddrRange a) => FromJSON (AddrRange a) where
    parseJSON (String s)
        | Just r <- readMaybe (Text.unpack s) = pure r
        | otherwise = fail "Unable to parse"
    parseJSON v = typeMismatch "AddrRange" v

instance Show a => ToJSON (AddrRange a) where
    toJSON = String . Text.pack . show

instance FromJSON IPRange where
    parseJSON (String s)
        | Just r <- readMaybe (Text.unpack s) = pure r
        | otherwise = fail "Unable to parse"
    parseJSON v = typeMismatch "IPRange" v

instance ToJSON IPRange where
    toJSON = String . Text.pack . show

instance Read (AddrRange a) => FromJSONKey (AddrRange a) where
    fromJSONKey = FromJSONKeyTextParser $ \t ->
                      case readMaybe (Text.unpack t) of
                          Just r -> pure r
                          Nothing -> fail "Unable to parse AddrRange"

instance Show a => ToJSONKey (AddrRange a) where
    toJSONKey = toJSONKeyText (Text.pack . show)

instance FromJSONKey IPRange where
    fromJSONKey = FromJSONKeyTextParser $ \t ->
                      case readMaybe (Text.unpack t) of
                          Just r -> pure r
                          Nothing -> fail "Unable to parse IPRange"

instance ToJSONKey IPRange where
    toJSONKey = toJSONKeyText (Text.pack . show)
