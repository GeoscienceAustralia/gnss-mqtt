{-# LANGUAGE NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import BasicPrelude hiding (map)
import Data.Aeson
import Data.ByteString.Lazy hiding (ByteString, map)
import Data.ByteString hiding (map)
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List
import Data.Conduit.Serialization.Binary
import Data.RTCM3
import Data.Binary as Binary
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Text hiding (map)
import Network.MQTT as MQTT
import GHC.Generics

-- ahhhhhh!
-- This whole block came from https://stackoverflow.com/questions/48179380/getting-the-data-constructor-name-as-a-string-using-ghc-generics
-- Should probably hide all this Generics garbage in another file
deriving instance Generic RTCM3Msg

constrName :: (HasConstructor (Rep a), Generic a) => a -> String
constrName = genericConstrName . from

class HasConstructor (f :: * -> *) where
  genericConstrName :: f x -> String

instance HasConstructor f => HasConstructor (D1 c f) where
  genericConstrName (M1 x) = genericConstrName x

instance (HasConstructor x, HasConstructor y) => HasConstructor (x :+: y) where
  genericConstrName (L1 l) = genericConstrName l
  genericConstrName (R1 r) = genericConstrName r

instance Constructor c => HasConstructor (C1 c f) where
  genericConstrName = conName

-- Might submit an issue against Data.RTCM3 to see if they are interested in implementing something like the following
-- This would mean we wouldn't need to use constrName to get the message number
--msgNum :: RTCM3Msg -> Maybe Word16
--msgNum (RTCM3Msg1077 _ _) = Just msg1077
--msgNum (RTCM3Msg1087 _ _) = Just msg1087
--msgNum (RTCM3Msg1097 _ _) = Just msg1097
-- ... 
--msg _ = Nothing

encodeLine :: RTCM3Msg -> (Topic, ByteString)
encodeLine message = (msgTopic, msg)
  where
    msgTopic = toTopic $ MqttText $ Data.Text.pack msgNumber
    msgNumber = "rtcm/" <> (BasicPrelude.drop 8 $ constrName message)
    msg = toStrict $ Binary.encode message

sink :: MQTT.Config -> Sink (Topic, ByteString) IO ()
sink mConf = Data.Conduit.List.mapM_ $ uncurry (MQTT.publish mConf MQTT.NoConfirm False)

main :: IO ()
main = do
  mqtt <- defaultConfig <$> mkCommands <*> newTChanIO

  _ <- forkIO $ do
    runConduit 
      $ sourceHandle stdin 
      .| conduitDecode 
      .| map encodeLine
      .| sink mqtt

    disconnect mqtt

  _ <- run mqtt
  return ()
