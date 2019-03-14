{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens
import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Text as T

import Discord hiding (Role)


type Username = T.Text

data NightActivity 
  = Kill Username
  | Heal Username
  | Investigate Username
  deriving (Eq,Ord)

data Reaction
  = Vote Username Username | UnVote Username Username
  | SetNightTarget NightActivity | UnsetNightTarget NightActivity
  | NoOp

instance Semigroup Reaction where
  Vote _ _ <> UnVote _ _ = NoOp
  SetNightTarget _ <> UnsetNightTarget _ = NoOp
  a <> b = b

instance Monoid Reaction where
  mempty = NoOp

data TestState = TestState
  {_reactions :: [[Reaction]]}

data Role = Godfather | Mafia | Villager | Detective | Doctor
  deriving (Show,Eq)

data Player = Player
  {_alive :: Bool
  ,_role :: Role
  ,_iD :: Snowflake}
  deriving Eq

type Users = [(Username,UserId)]
type Players = M.Map Username Player

data GameState = GameState
  {_round :: Int
  ,_players :: M.Map Username Player
  ,_messagesToDelete :: [(ChannelId,MessageId)]
  ,_villageChannel :: ChannelId
  ,_mafiaChannel :: ChannelId
  ,_votes :: M.Map Username Username
  ,_nightActivities :: M.Map UserId NightActivity
  ,_testState :: TestState
  ,_serverID :: Snowflake
  }

type Client = (RestChan, Gateway, [ThreadIdType])

data Env = Env {client :: Client, user :: User, var :: MVar [Event]}

data MafiaError
  = GameOver 
  | InvalidPlayerNumber
  | RestError RestCallException

makeLenses ''TestState
makeLenses ''Player
makeLenses ''GameState

makePrisms ''Event

type Base m = (Monad m, UI m, MonadError MafiaError m, Sleep m, MonadIO m)

type Game m = (Base m, MonadState GameState m)

newtype Prod a =
  Prod {runProd :: ExceptT MafiaError (ReaderT Env (StateT GameState IO)) a}
  deriving (Functor,Applicative,Monad,MonadIO,MonadReader Env
           ,MonadState GameState,MonadError MafiaError)


class Monad m => UI m where
  sendMessage :: ChannelId -> T.Text -> m Message
  deleteMessage :: (ChannelId,MessageId) -> m ()
  getMessage :: (ChannelId,MessageId) -> m Message
  createReaction :: (ChannelId,MessageId) -> T.Text -> m ()
  createChannel :: T.Text -> [Overwrite] -> m Channel
  getEvent :: m (Either GatewayException Event)
  getEvents :: m [Event]
  getUser :: UserId -> m User
  clearEvents :: m ()
  -- permission

class Monad m => Sleep m where
  sleep :: Int -> m ()

call q = do
  r <- asks client >>= liftIO . flip restCall q
  liftEither $ first RestError r

is l = maybe False (const True) . preview l

getReactions :: Game m => m [Event]
getReactions = undefined
-- map toReaction . filter (is _MessageReactionAdd) <$> getEvents

-- toReaction :: Event -> Reaction
-- toReaction 

instance UI Prod where
  sendMessage c = call . CreateMessage c -- TODO add to toBeDelted
  deleteMessage = call . DeleteMessage
  getMessage = call . GetChannelMessage
  createReaction p = call . CreateReaction p
  getEvent = asks client >>= liftIO . nextEvent
  getEvents = asks var >>= liftIO . readMVar
  getUser = call . GetUser
  clearEvents = asks var >>= void . liftIO . readMVar
  createChannel name overwrites = do
    server <- use serverID
    let defaultOptions = CreateGuildChannelOptsText Nothing Nothing (Just False) Nothing
    call $ CreateGuildChannel server name overwrites defaultOptions
    
instance Sleep Prod where
  sleep = liftIO . threadDelay . (*100)