{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Aeson.IP () where

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative (pure)
#endif

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import           Data.IP
import           Data.IP.RouteTable (Routable, IPRTable)
import qualified Data.IP.RouteTable as RouteTable
import qualified Data.Text as Text
import           Text.Read (readMaybe)

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

instance ( FromJSONKey k
         , Read (AddrRange k)
         , Routable k
         ) => FromJSON1 (IPRTable k) where
    liftParseJSON p _ = case fromJSONKey of
        FromJSONKeyTextParser f -> withObject "IPRTable k v" $
            KeyMap.foldrWithKey
                (\k v rt -> RouteTable.insert <$> f (Key.toText k) <?> Key k
                                              <*> p v <?> Key k
                                              <*> rt)
                (pure RouteTable.empty)
        _ -> \_ -> fail "using IPRTable in this context is not yet supported"

instance ( FromJSONKey k
         , Read (AddrRange k)
         , Routable k
         , FromJSON v
         ) => FromJSON (IPRTable k v) where
    parseJSON = parseJSON1

instance (Routable k, Show k, ToJSON k) => ToJSON1 (IPRTable k) where
    liftToJSON g _ = case toJSONKey of
        ToJSONKeyText f _ -> Object . KeyMap.fromList
                                    . map (\(k, v) -> (f k, g v))
                                    . RouteTable.toList
        _ -> error "using IPRTable as a JSON key is not yet supported"

instance (Routable k, Show k, ToJSON k, ToJSON v) => ToJSON (IPRTable k v) where
    toJSON = toJSON1
