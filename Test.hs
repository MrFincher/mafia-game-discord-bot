module Test where

import Types

newtype TestM a = TestM {runTestM :: StateT GameState (ExceptT MafiaError IO) a}
  deriving (Functor,Applicative,Monad,MonadIO,MonadState GameState,MonadError MafiaError)

runTest :: TestM a -> IO (Either MafiaError (a,GameState))
runTest = runExceptT . flip runStateT (initState players 1) . runTestM 
  where players = M.fromList [("pavl",Player True Godfather 5),("lukas",Player True Villager 6)]

test = runTest gameLoop

instance UI TestM where
  sendMessage c m = do
    liftIO $ T.putStrLn $ T.unwords ["Message:", tShow c, m]
    error "no message to return in test mode"
  deleteMessage (c,m) = return ()
  getMessage _ = error "not implemented in test mode"
  getReactions = fmap head $ testState . reactions %%= splitAt 1

instance Sleep TestM where
  sleep _ = return ()
