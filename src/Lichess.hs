{-# LANGUAGE DeriveGeneric #-}

module Main where

import Network.Socket
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import Control.Monad.Trans

main = withSocketsDo $ WS.runClient "socket.lichess.org" 9000 "/lobby/socket?mobile=1&sri=ghsggfjgf33&version=0" app

app conn = do
	liftIO $ putStrLn "Connected!"

	WS.sendTextData conn (T.pack "{ t:'p', v:0 }")
	msg <- WS.receiveData conn
	liftIO $ putStrLn (T.unpack msg)

{-
import GHC.Generics

data LichessMsg =
	Ping String |
	Pong Int
	deriving (Generic,Show)

instance ToJSON LichessMsg where
	toJSON (Ping v) = object [ "t" .= "p", "v" .= 
	toEncoding = genericToEncoding defaultOptions
instance FromJSON LichessMsg
 
data LichessMsg = LichessMsg { t : LichessMsgType, d : LichessMsgData } deriving (Generic,Show)
instance ToJSON LichessMsg where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON LichessMsg

instance ToJSON MsgType where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON LichessMsg

data MsgData = 
-}