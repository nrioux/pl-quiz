{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Data.Aeson
import qualified Data.Map.Strict as Map
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import System.Environment

import Lib

data Team = Team { key :: Int, name :: String }
  deriving (Eq, Show, Generic)
instance ToJSON Team
instance FromJSON Team

data Score = Score { score :: Int, team :: Team }
  deriving (Eq, Show, Generic)
instance ToJSON Score
instance FromJSON Score

newtype Scoreboard = Scoreboard (Map.Map Int Score)

instance ToJSON Scoreboard where
  toJSON (Scoreboard board) = toJSON $ Map.toList board

insertScore :: Score -> Scoreboard -> Scoreboard
insertScore score (Scoreboard board) =
  Scoreboard $ Map.insert (key $ team score) score board

data ServerState = ServerState (TVar Scoreboard)
type AppM = ReaderT ServerState Handler

type GetScore = Get '[JSON] Scoreboard
type AddScore = ReqBody '[JSON] Score :> Post '[JSON] ()

type ScoreAPI = "score" :> (GetScore :<|> AddScore)
scoreServer :: ServerT ScoreAPI AppM
scoreServer = getScore :<|> addScore
  where
    getScore = do ServerState boardptr <- ask
                  liftIO $ atomically $ readTVar boardptr
    addScore score = do ServerState boardptr <- ask
                        liftIO $ updateBoard score boardptr
    updateBoard score boardptr = atomically $ do
      board <- readTVar boardptr
      let board' = insertScore score board
      writeTVar boardptr board'

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
  board <- atomically $ newTVar $ Scoreboard $ Map.singleton 0 Score { score = 10, team = Team { key = 1, name = "Green Team" } }
  let port = read portStr
  putStrLn $ "CD: " ++ cd
  putStrLn $ "Running server: http://localhost:" ++ (show port)
  run port (app $ ServerState board)
