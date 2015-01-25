module Main where

import Data.Aeson ((.:))
import Data.Aeson.Lens (key, _String, _Integer)
import Data.Text (Text)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

import qualified Data.Aeson as A
import qualified System.Environment as E

import BasePrelude hiding (app, log)
import Control.Lens
import Utils

newtype URL = URL Text
  deriving (Show)

data Job = Job
  { jobUrl :: URL
  , jobViewport :: (Integer, Integer)
  , jobCallback :: URL
  } deriving (Show)

instance A.FromJSON Job where
  parseJSON (A.Object o) =
    Job <$> (URL <$> o .: "url")
        <*> (o .: "viewport")
        <*> (URL <$> o .: "callback_url")
  parseJSON _ =
    error "FromJSON Job: expected object"

log :: String -> IO ()
log = putStrLn

app :: Application
app req respond = do
  t <- requestBody req :: ApplicationM Job
  respond (respondText status200 [] (present t))

main' :: Int -> IO ()
main' port = do
  log ("+ Listening on port " <> show port)
  run port (safely app)

main :: IO ()
main = do
  args <- E.getArgs
  let port = read (head args)
  main' port
