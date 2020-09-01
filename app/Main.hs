{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import System.Environment

import Lib

data Team = Team { id :: Int, name :: String }
  deriving (Eq, Show, Generic)
instance ToJSON Team

data Score = Score { score :: Int, team :: Team }
  deriving (Eq, Show, Generic)
instance ToJSON Score

data Scoreboard = Scoreboard { entries :: [Score] }
  deriving (Eq, Show, Generic)
instance ToJSON Scoreboard

type ScoreAPI = "score" :> Get '[JSON] Scoreboard
scoreServer :: Server ScoreAPI
scoreServer = return Scoreboard { entries = [] }

type StaticAPI = Raw

type QuizAPI = ScoreAPI :<|> StaticAPI

quizAPI :: Proxy QuizAPI
quizAPI = Proxy


staticServer :: Server StaticAPI
staticServer = serveDirectoryFileServer "/app/static-files"

server :: Server QuizAPI
server = scoreServer :<|> staticServer

app :: Application
app = serve quizAPI server

main :: IO ()
main = do
  cd <- getCurrentDirectory
  portStr <- getEnv "PORT"
  let port = read portStr
  putStrLn $ "CD: " ++ cd
  putStrLn $ "Running server: http://localhost:" ++ (show port)
  run port app
