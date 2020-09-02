{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
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

data ServerState = ServerState (TVar Scoreboard)
type AppM = ReaderT ServerState Handler

type ScoreAPI = "score" :> Get '[JSON] Scoreboard
scoreServer :: ServerT ScoreAPI AppM
scoreServer = do
  ServerState boardptr <- ask
  board <- liftIO $ atomically $ readTVar boardptr
  return board

type StaticAPI = Raw

type QuizAPI = ScoreAPI :<|> StaticAPI


quizAPI :: Proxy QuizAPI
quizAPI = Proxy


staticServer :: ServerT StaticAPI AppM
staticServer = serveDirectoryFileServer "/app/static-files"

server :: ServerT QuizAPI AppM
server = scoreServer :<|> staticServer


appToHandler :: ServerState -> AppM a -> Handler a
appToHandler s m = runReaderT m s

app :: ServerState -> Application
app s = serve quizAPI $ hoistServer quizAPI (appToHandler s) server

main :: IO ()
main = do
  cd <- getCurrentDirectory
  portStr <- getEnv "PORT"
  board <- atomically $ newTVar $ Scoreboard { entries = [] }
  let port = read portStr
  putStrLn $ "CD: " ++ cd
  putStrLn $ "Running server: http://localhost:" ++ (show port)
  run port (app $ ServerState board)
