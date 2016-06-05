{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Lib
    ( startApp
    ) where

import           Control.Monad.Reader
import           Data.Maybe               (fromMaybe)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

newtype Greet = Greet String deriving (Show)
type Handler' a = ReaderT Greet Handler a

type API = "a" :> Get '[JSON] String
  :<|> "b" :> Get '[JSON] Int
  :<|> "c" :> Header "Accept" String :> Get '[PlainText] (Headers '[Header "X-Foo" String] String)

startApp :: IO ()
startApp = run 8080 (app (Greet "Hello"))

app :: Greet -> Application
app g = serve api enteredServer
  where enteredServer :: Server API
        enteredServer = enter r2h server
        r2h = runReaderTNat g

api :: Proxy API
api = Proxy

server = aHandler'
  :<|> bHandler'
  :<|> cHandler

aHandler' :: Handler' String
aHandler' = do
  liftIO $ putStrLn "debug"
  (Greet greet) <- ask
  return greet

bHandler' :: Handler' Int
bHandler' = return 1

cHandler :: Maybe String -> Handler' (Headers '[Header "X-Foo" String] String)
cHandler x = let f = fromMaybe "none" x in
               return $ addHeader "bar" f
