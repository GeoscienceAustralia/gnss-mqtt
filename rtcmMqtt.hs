{-# LANGUAGE NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}
module Main where

import BasicPrelude                      hiding (map)
import Data.Aeson
import Data.ByteString.Lazy              hiding (ByteString, map)
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List
import Data.Conduit.Serialization.Binary
import Data.RTCM3
import System.IO

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Network.MQTT as MQTT

import qualified Data.ByteString.Char8 as C

t :: Topic
t = "topic"

-- From rtcm2json
encodeLine :: RTCM3Msg -> ByteString
encodeLine v = toStrict $ encode v <> "\n"

sink :: MQTT.Config -> Sink ByteString IO ()
sink mConf = Data.Conduit.List.mapM_ (MQTT.publish mConf MQTT.NoConfirm False t)

main :: IO ()
main = do
  cmds <- mkCommands
  pub <- newTChanIO
  let mqtt = defaultConfig cmds pub   

  _ <- forkIO $ do
    runConduit 
      $ sourceHandle stdin 
      .| conduitDecode 
      .| map encodeLine
      .| sink mqtt

    disconnect mqtt

  _ <- run mqtt
  return ()
