{-# Language OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Network.MQTT as MQTT
import qualified Data.ByteString.Char8 as C

topic :: MQTT.Topic
topic = "test"

main :: IO ()
main = do
    cmds <- MQTT.mkCommands
    pubChan <- newTChanIO
    let conf = MQTT.defaultConfig cmds pubChan
    _ <- forkIO $ do
        MQTT.publish conf MQTT.NoConfirm False topic (C.pack "hello")
    terminated <- MQTT.run conf
    print terminated
