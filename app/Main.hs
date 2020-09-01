module Main where

import Data.Text
import Data.Time (UTCTime)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory

import Lib

type StaticAPI = Raw

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

server :: Server StaticAPI
-- server = serveDirectoryWebApp "static-files"
server = serveDirectoryFileServer "static-files"

app :: Application
app = serve staticAPI server

main :: IO ()
main = do
  cd <- getCurrentDirectory
  putStrLn $ "CD: " ++ cd
  putStrLn "Running server: http://localhost:8081"
  run 8081 app
