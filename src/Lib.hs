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
type Handler' a = ReaderT Greet Handler a

type API = "a" :> Get '[JSON] String
  :<|> "b" :> Get '[JSON] Int

startApp :: IO ()
startApp = run 8080 (app (Greet "Hello"))

app :: Greet -> Application
app g = serve api enteredServer
  where enteredServer :: Server API
        enteredServer = enter r2h server
        r2h = runReaderTNat g

api :: Proxy API
api = Proxy

server :: Handler' String :<|> Handler' Int
server = aHandler'
  :<|> bHandler'

aHandler' :: Handler' String
aHandler' = do
  liftIO $ putStrLn "debug"
  (Greet greet) <- ask
  return greet

bHandler' :: Handler' Int
bHandler' = return 1
