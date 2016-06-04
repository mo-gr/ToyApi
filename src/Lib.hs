{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Reader
import Servant

newtype Greet = Greet String deriving (Show)

type API = "a" :> Get '[JSON] String
  :<|> "b" :> Get '[JSON] Int

startApp :: IO ()
startApp = run 8080 (app (Greet "Hello"))

app :: Greet -> Application
app g = serve api enteredServer
  where enteredServer :: Server API
        enteredServer = enter r2h server
        r2h = Nat $ \r -> return (runReader r g)

api :: Proxy API
api = Proxy

server :: ServerT API (Reader Greet)
server = aHandler
  :<|> pure bHandler

aHandler :: Reader Greet String
aHandler = do
  (Greet greet) <- ask
  return $ greet ++ "a"

bHandler :: Int
bHandler = 1
