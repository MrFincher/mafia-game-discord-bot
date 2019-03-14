{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (round)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Lens hiding (zoom)
import qualified Data.Map as M
import Data.Bifunctor
import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Discord hiding (Role) -- role needed?

import Types


-- refactor into monad stack, needs fork tho
collectEvents :: MVar [Event] -> Client -> IO ()
collectEvents var client = flip finally (stopDiscord client) $ forever $
    nextEvent client >>= \case
      Right e -> modifyMVar_ var $ return . (e:)
      Left ex -> error "exception while getting events"

handleReaction :: Game m => ReactionInfo -> m ()
handleReaction ReactionInfo {..} = do
  Message {..} <- getMessage (reactionChannelId,reactionMessageId)
  unless (tail (T.words messageText) == ["no","one"]) $ do
    let target = last $ T.words messageText
    case emojiName reactionEmoji of -- name probably not what we need here
      "\\U0001F5E1" -> Kill target
      "\\U0001F5E1" -> Kill target
      "\\U0001F50E" -> Investigate target
      "\\U0001F3E5" -> Heal target
      "\\U0001F4DD" -> Vote target

      -- nightActivities %= M.insert reactionUserId

tShow :: Show s => s -> T.Text
tShow = T.pack . show

main :: IO ()
main = return ()

run :: IO (Either MafiaError (), GameState)
run = do
  let token = "NTE5NTQ5OTgxMzY4MTIzNDIw.DxZU1w.HAojtgq8hBUhJXc0kw3211GBsZc"
  client <- loginRestGateway (Auth token)
  Right user <- restCall client GetCurrentUser
  var <- newMVar []
  forkIO $ collectEvents var client
  flip runStateT (initState M.empty 0)
    $ flip runReaderT (Env client user var) $ runExceptT $ runProd start 

start :: Game m => m ()
start = do
  Message {..} <- firstStartMessage
  sendStartMessage messageChannel
  serverID .= fromJust messageGuild
  playerJoin messageId >>= setupPlayers
  setupChannels
  tellRoles
  sendAllies
  forever gameLoop

sendStartMessage chan = do
  Message {..} <- sendMessage chan
    "A NEW GAME OF MAFIA HAS STARTED! REACT TO PLAY!"
  createReaction (messageChannel,messageId) "✅"

firstStartMessage :: Game m => m Message
firstStartMessage = getEvent >>= \case
    Right (MessageCreate msg) | messageText msg == "mafia start" ->
      -- TODO only allows from mafia gamemaster
      return msg
    _ -> firstStartMessage

playerJoin :: MessageId -> Game m => m Users
playerJoin mid = getEvent >>= \case
  Right (MessageCreate msg) | messageText msg == "mafia start" -> return []
  Right (MessageReactionAdd ri) | reactionMessageId ri == mid -> do
    let id = reactionUserId ri
    username <- T.pack . userName <$> getUser id
    ((username,id):) <$> playerJoin mid
  _ -> playerJoin mid
    
  -- while game is running always do this:
    -- Right (MessageCreate msg) -> sendMessage_ (view messageAuthor msg) "Game is already running!"

setupPlayers :: Game m => Users -> m ()
setupPlayers us = do
  counts <- maybe (throwError InvalidPlayerNumber) return $ calcRoleCounts $ length us
  let roles = [Godfather,Mafia,Detective,Doctor] ++ repeat Villager
      roleSet = concat $ zipWith replicate counts roles
      aux = zipWith (Player True) roleSet
  players .= M.fromList (uncurry zip . second aux $ unzip us)

calcRoleCounts :: Int -> Maybe [Int]
calcRoleCounts p
  | p == 4 = Just [1,0,1,1,1]
  | p <4 || p >20 = Nothing
  | otherwise = Just [godfather,mafia,detective,doctor]
  where godfather = 1
        mafia = if p>=6 then p-3 `div` 3 else 0
        detective = 1 + p-4 `div` 5
        doctor = if p>=6 then p-6 `div` 5 else 1


sendMessage_ t = void . sendMessage t

sendMafia :: Game m => T.Text -> m Message
sendMafia msg = use mafiaChannel >>= flip sendMessage msg

sendVillage :: Game m => T.Text -> m Message
sendVillage msg = use villageChannel >>= flip sendMessage msg

forPlayers :: Game m => (Player -> m a) -> m [a]
forPlayers f = uses players M.elems >>= mapM f

forPlayers_ :: Game m => (Player -> m a) -> m ()
forPlayers_ = void . forPlayers

deleteMessages :: Game m => m ()
deleteMessages = use messagesToDelete >>= mapM_ deleteMessage

setupChannels :: Game m => m ()
setupChannels = error "TODO setup channels"

tellRoles :: Game m => m ()
tellRoles = forPlayers_ $ \player -> do
  messagePlayer_ player $ "Welcome! Your role is " <> tShow (player ^. role)

isMafia :: Player -> Bool
isMafia player = view role player `elem` [Godfather,Mafia]

sendAllies :: Game m => m ()
sendAllies = forPlayers_ $ \player ->
  when (isMafia player) $ do
    mafias <- uses players $ M.filter isMafia
    let allies = T.intercalate ", " $ M.keys mafias
    messagePlayer_ player $ "Your allies are " <> allies

messagePlayer_ player = void . sendMessage_ (player ^. iD)

messagePlayerR player e t = do
  Message {..} <- sendMessage (player ^. iD) t
  createReaction (messageChannel,messageId) e

gameLoop :: Game m => m ()
gameLoop = do
  round += 1
  r <- use round
  liftM2 (>>) sendVillage sendMafia $ "Day " <> tShow r 
  forPlayers_ $ flip sendMessage ("Day " <> tShow r) . view iD
  day
  deleteMessages
  checkGameover
  liftM2 (>>) sendVillage sendMafia "Night has started!"
  forPlayers_ $ flip sendMessage "Night has started!" . view iD
  night
  checkGameover

checkGameover :: Game m => m () 
checkGameover = do
  ps <- use players
  let p@(ms,vs) = M.partition (flip elem [Godfather,Mafia] . view role) ps
  case join bimap M.size p of
    (m,v) | m+v == 2, m >= 1 -> gameOver ms $ Just "Mafia"
    (m,0) | m > 0 -> gameOver ms $ Just "Mafia"
    (0,v) | v > 0 -> gameOver vs $ Just "Village"
    (0,0) -> gameOver M.empty Nothing

gameOver :: Game m => Players -> Maybe T.Text -> m ()
gameOver _ Nothing = void $ sendVillage "Draw!"
gameOver winners (Just w) = do
  sendVillage $ "The " <> w <> " have won!"
  let winnerNames = T.intercalate ", " $ M.keys winners
  sendVillage $ "Winning players: " <> winnerNames
  throwError GameOver

day :: Game m => m ()
day = do
  -- permissions
  clearEvents
  sendVoteOptions
  sleep 60
  proccessVotes

forAlives_ f = do
  players <- uses players M.assocs 
  mapM_ f $ filter (view alive . snd) players

sendVoteOptions :: Game m => m ()
sendVoteOptions = forAlives_ $ \(name,player) ->
  forAlives_ $ \(other,_) -> when (other /= name) $
    messagePlayerR player "\\U0001F4DD" $ "Vote to hang  " <> other

proccessVotes :: Game m => m ()
proccessVotes = do
  -- votes in state neccessary?
  getVotes
  (n,vs) <- M.findMax . M.fromListWith (+) . map (,1) <$> uses votes M.elems
  alivePlayers <- length . filter (view alive) . M.elems <$> use players
  when (vs > div alivePlayers 2) $ kill n

kill :: Game m => Username -> m ()
kill username = do
  Player {..} <- uses players (M.!username)
  sendVillage $ username <> " was killed! Their role was " <> tShow _role
  sendMessage_ _iD "You werer killed!"
  players %= M.adjust (alive .~ False) username
  when (_role == Godfather) determineNextGodFather

determineNextGodFather :: Game m => m ()
determineNextGodFather = do
  find ((==Mafia) . view role . snd) <$> uses players M.assocs >>= \case
    Nothing -> liftIO $ T.putStrLn "No Mafia left, game should end soon"
    Just (u,_) -> players %= M.adjust (role .~ Godfather) u

getVotes :: Game m => m ()
getVotes = do
  rs <- getReactions
  forM_ rs $ \case
    Vote voter target -> votes %= M.insert voter target
    UnVote voter target -> votes %= M.alter (mfilter (/=target)) voter

night :: Game m => m ()
night = do
  -- permissions
  clearEvents
  sendNightActivitiyOptions
  sleep 60
  -- proccess ativities

sendNightActivitiyOptions :: Game m => m ()
sendNightActivitiyOptions = forAlives_ $ \(_,p@Player {..}) -> do
  when (_role == Godfather) $ godfatherActivity p

godfatherActivity :: Game m => Player -> m ()
godfatherActivity p = do
  messagePlayer_ p "Who do you want to kill?"
  forAlives_ $ \(name,Player {..}) -> do
  when (_role `notElem` [Godfather,Mafia]) $ do
    messagePlayerR p "\\U0001F5E1" $ "Kill " <> name 
  messagePlayerR p "\\U0001F5E1" "Kill no one"

detectiveActivity :: Game m => Player -> m ()
detectiveActivity p = do
  messagePlayer_ p "Who do you want to investigate?"
  forAlives_ $ \(name,o@Player {..}) -> do
    when (o /= p) $ do
      messagePlayerR p "\\U0001F50E" $ "Ivestigate " <> name 
  messagePlayerR p "\\U0001F50E" "Ivestigate no one"

doctorActivity :: Game m => Player -> m ()
doctorActivity p = do
  messagePlayer_ p "Who do you want to heal?"
  forAlives_ $ \(name,o@Player {..}) -> do
    when (o /= p) $ do
      messagePlayerR p "\\U0001F3E5" $ "Heal " <> name 
  messagePlayerR p "\\U0001F3E5" "Heal no one"

initState :: Players -> Snowflake -> GameState
initState players = GameState 0 players [] 1 2 M.empty M.empty initTest
  where initTest = TestState [[Vote "pavl" "lukas", Vote "lukas" "lukas"]]