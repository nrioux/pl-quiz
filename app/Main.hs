module Main where

import Data.Text
import Data.Time (UTCTime)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import System.Environment

import Lib

type StaticAPI = Raw

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

server :: Server StaticAPI
server = serveDirectoryWebApp "static-files"

app :: Application
app = serve staticAPI server

main :: IO ()
main = do
  cd <- getCurrentDirectory
  portStr <- getEnv "PORT"
  let port = read portStr
  putStrLn $ "CD: " ++ cd
  putStrLn $ "Running server: http://localhost:" ++ (show port)
  run port app
