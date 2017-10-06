{-# LANGUAGE DeriveGeneric,ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where

import Debug.Trace
import Wuss
import Network.Connection
import Network.Socket
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import Control.Monad.Trans
import Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy as BSL
import Data.ByteString as BSX
import Data.ByteString.Char8 as BS8

import Network.WebSockets.Stream

main = do
	let tlsSettings = TLSSettingsSimple
		-- This is the important setting.
		{ settingDisableCertificateValidation = True
		, settingDisableSession = False
		, settingUseServerName = False
		}
	let connectionParams = ConnectionParams
		{ connectionHostname = "socket.lichess.org"
		, connectionPort = 443
		, connectionUseSecure = Just tlsSettings
		, connectionUseSocks = Nothing
		}
	let headers = []
	context <- initConnectionContext
	connection <- connectTo context connectionParams
	stream <- makeStream
		(fmap Just (connectionGetChunk connection))
		(maybe (return ()) (connectionPut connection . (\ a -> trace (BS8.unpack a) a) . toStrict))
	WS.runClientWithStream stream "socket.lichess.org" "/lobby/socket/v2?sri=abdhfzends" WS.defaultConnectionOptions headers app

--	withSocketsDo $ runSecureClient "socket.lichess.org" 443 "/lobby/socket/v2?sri=abdhfzends" app

app conn = do
	WS.sendTextData conn $ T.pack "{t:\"p\"}"

--	WS.sendTextData conn $ T.pack "{t:\"startWatching\",d:\"abc123GH\"}"

	liftIO $ Prelude.putStrLn "XXXXXXXXXX"
	WS.Text msg <- WS.receiveDataMessage conn
	liftIO $ BS.putStrLn msg

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