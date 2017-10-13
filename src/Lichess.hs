{-# LANGUAGE DeriveGeneric,ScopedTypeVariables,OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where

--import Debug.Trace
import Wuss
--import Network.Connection
import Network.Socket
import Network.WebSockets as WS
import qualified Data.Text as T
import Control.Monad.Trans
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 as BSL
--import Data.ByteString as BSX
--import Data.ByteString.Char8 as BS8
import System.Random
import Control.Monad
import Data.CaseInsensitive

--import Network.WebSockets.Stream

apiVersion = 2

main = do
	sri <- forM [1..10] $ \ _ -> getStdRandom (randomR ('a','z'))
	print sri
	withSocketsDo $ runSecureClientWith
		"socket.lichess.org" 443 ("/lobby/socket/v" ++ show apiVersion ++ "?sri=" ++ sri)
		defaultConnectionOptions
		[	("X-Requested-With","XMLHttpRequest"),
			("Accept",BS.pack $ "application/vnd.lichess.v" ++ show apiVersion ++ "+json") ]
		app

app conn = do
	WS.sendTextData conn $ T.pack "{t:\"p\"}"

--	WS.sendTextData conn $ T.pack "{t:\"startWatching\",d:\"abc123GH\"}"

	liftIO $ Prelude.putStrLn "XXXXXXXXXX"
	WS.Text msg <- WS.receiveDataMessage conn
	liftIO $ BSL.putStrLn msg

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