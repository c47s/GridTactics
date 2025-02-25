-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module API
    ( API
    , api

    , getActor
    , quitActor
    , look
    , act
    , delAct
    , getDone
    , setDone
    , getActorIDs
    , getTurnOrder
    , actorNames
    , newActor
    , newCluster
    , getNumDone
    , multiLook
    , roundLook
    , getConfig

    , Config (..)
    , runServer
    ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async (race)
import           Control.Monad.Extra (firstJustM)
import           Data.Aeson
import           Data.Time.Clock
import qualified Deque.Lazy as D
import           Mechanics
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger (withStdoutLogger)
import           Relude
import           Servant
import           Servant.API.Flatten
import           Servant.Client
import           System.Posix.Signals hiding (Handler)
import           Util
import           WebInstances ()




worldBakPath :: String
worldBakPath = ".gridtac_bak.json"


type ActorAPI = Capture "id" UID
    :> ( ActorSelfAPI
    :<|> ViewAPI
    :<|> ActAPI
    :<|> DoneAPI
    )

type ActorSelfAPI = Get '[JSON] Actor
               :<|> DeleteNoContent

type ViewAPI = "view" :> Get '[JSON] Grid

type ActAPI = "act" :>
        ( ReqBody '[JSON] Action :> PostNoContent
     :<|> DeleteNoContent
        )

type DoneAPI = "done" :> Modify '[JSON] Bool

type ActorsAPI = UIDsAPI
            :<|> OrderAPI
            :<|> NamesAPI
            :<|> NewAPI
            :<|> ClusterAPI
            :<|> NumDoneAPI
            :<|> MultiViewAPI
            :<|> RoundViewAPI

type UIDsAPI = Get '[JSON] [UID]

type OrderAPI = "order" :> Get '[JSON] [UID]

type NamesAPI = "names" :> Get '[JSON] [Text]

type NewAPI = "new"
    :> ReqBody '[JSON] (Text, Text) -- Pawn name, Owner name
    :> Post '[JSON] UID

type ClusterAPI = "cluster"
    :> ReqBody '[JSON] ([Text], Text) -- Pawn names, Owner name
    :> Post '[JSON] [UID]

type NumDoneAPI = "done" :> Get '[JSON] Int

type MultiViewAPI = "view"
    :> ReqBody '[JSON] [UID] :> Get '[JSON] Grid

type RoundViewAPI = "replay"
    :> ReqBody '[JSON] [UID] :> Get '[JSON] (Seq Grid)

type ConfigAPI = Get '[JSON] Config

type API = "actor"  :> ActorAPI
      :<|> "actors" :> ActorsAPI
      :<|> "config" :> ConfigAPI

api :: Proxy API
api = Proxy



data Config = Config
    { pawnTemplate :: Entity
    , actorTemplate :: Actor
    , pawnsPerClient :: Int
    , runDailyAt :: Maybe DiffTime
    }
    deriving stock (Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)



hActorSelf :: (World w) => UID -> Config -> IORef w -> Server ActorSelfAPI
hActorSelf aID conf ref = hGetActor aID conf ref
        :<|> hDelActor aID conf ref

hGetActor :: (World w) => UID -> Config -> IORef w -> Handler Actor
hGetActor aID _conf = doState $ gets $ lookupActor aID

hDelActor :: (World w) => UID -> Config -> IORef w -> Handler NoContent
hDelActor aID _conf = doState do
    modify $ delActor aID
    return NoContent

hView :: (World w) => UID -> Config -> IORef w -> Server ViewAPI
hView aID _conf = doState do
    w <- get
    gets $ view (vision w $ lookupActor aID w) (findActor aID w)

hPostAct :: (World w) => UID -> Config -> IORef w -> Action -> Handler NoContent
hPostAct aID _conf ref a = stateToIO ref do
    modify $ updateActor (pushAct a) aID
    return NoContent

hDelAct :: (World w) => UID -> Config -> IORef w -> Handler NoContent
hDelAct aID _conf ref = stateToIO ref do
    modify $ flip updateActor aID \a ->
        a {queue = D.tail $ queue a}
    return NoContent

hGetDone :: (World w) => UID -> Config -> IORef w -> Handler Bool
hGetDone aID _conf = doState $ gets $ done . lookupActor aID

hPostDone :: (World w) => UID -> Config -> IORef w -> Bool -> Handler NoContent
hPostDone aID conf ref isDone = do
    modifyIORef' ref $ updateActor (\a -> a {done = isDone}) aID
    w <- readIORef ref
    let nDone = numDone w
        nActors = length $ actors w
    when (isNothing (runDailyAt conf) && nActors > 0 && nDone == nActors) $ do
        liftIO $ runTurnAndSave ref
    return NoContent 

hDone :: (World w) => UID -> Config -> IORef w -> Server DoneAPI
hDone aID conf ref = hGetDone aID conf ref
                :<|> hPostDone aID conf ref

hUIDs :: (World w) => Config -> IORef w -> Server UIDsAPI
hUIDs _conf = doState $ gets actors

hOrder :: (World w) => Config -> IORef w -> Server OrderAPI
hOrder _conf = doState $ gets turnOrder

hNames :: (World w) => Config -> IORef w -> Server NamesAPI
hNames _conf = doState $ gets $ \w -> aname . flip lookupActor w <$> actors w

hNew :: (World w) => Config -> IORef w -> Server NewAPI
hNew conf ref (name', owner') = do
    maybeAID <- stateToIO ref . maybeState $
        scatterPawn
            ((pawnTemplate conf) {ename=Just name'})
            ((actorTemplate conf) {aname=name', owner=owner'})
    hoistEither' . maybeToRight err500 $ maybeAID

hCluster :: (World w) => Config -> IORef w -> Server ClusterAPI
hCluster conf ref (names, owner') = do
    let maybeCluster = stateToIO ref . maybeState $
            clusterPawns (ceiling (sqrt (fromIntegral $ length names :: Double)) + 2)
                [ ( (pawnTemplate conf) {ename=Just name'}
                , (actorTemplate conf) {aname=name', owner=owner'} )
                | name' <- names ]
        rollGen = modifyIORef' ref $ execState splitGen
    
    rollGen
    maybeAIDs <- firstJustM (\n -> replicateM_ n rollGen >> maybeCluster) [0..10] -- Retries
    hoistEither' . maybeToRight err500 $ maybeAIDs

hNumDone :: (World w) => Config -> IORef w -> Server NumDoneAPI
hNumDone _conf = doState $ gets numDone

hMultiView :: (World w) => Config -> IORef w -> Server MultiViewAPI
hMultiView _conf ref aIDs = stateToIO ref do
    w <- get
    return $ multiView ((\aID -> (vision w $ lookupActor aID w, findActor aID w)) <$> aIDs) w

hRoundView :: (World w) => Config -> IORef w -> Server RoundViewAPI
hRoundView _conf ref aIDs = stateToIO ref do
    roundView aIDs <$> get

hActor :: (World w) => Config -> IORef w -> Server ActorAPI
hActor conf ref aID = hActorSelf aID conf ref
            :<|> hView aID conf ref
            :<|> (hPostAct aID conf ref :<|> hDelAct aID conf ref)
            :<|> hDone aID conf ref

hActors :: (World w) => Config -> IORef w -> Server ActorsAPI
hActors conf ref = hUIDs conf ref
     :<|> hOrder conf ref
     :<|> hNames conf ref
     :<|> hNew conf ref
     :<|> hCluster conf ref
     :<|> hNumDone conf ref
     :<|> hMultiView conf ref
     :<|> hRoundView conf ref

hConfig :: Config -> Server ConfigAPI
hConfig = return

hAPI :: (World w) => Config -> IORef w -> Server API
hAPI conf ref = hActor conf ref
  :<|> hActors conf ref
  :<|> hConfig conf



-- | Generalize API client actions by converting ClientErrors to runtime errors,
-- and getting the environment using ReaderT.
clientToReader :: (MonadIO m) => ClientM a -> ReaderT ClientEnv m a
clientToReader cl = (either (error . show) id <$>) . liftIO . runClientM cl =<< ask

getActor :: (MonadIO m) => UID -> ReaderT ClientEnv m Actor
quitActor :: MonadIO m => UID -> ReaderT ClientEnv m NoContent
look :: (MonadIO m) => UID -> ReaderT ClientEnv m [[Square]]
act :: (MonadIO m) => UID -> Action -> ReaderT ClientEnv m NoContent
delAct :: (MonadIO m) => UID -> ReaderT ClientEnv m NoContent
getDone :: (MonadIO m) => UID -> ReaderT ClientEnv m Bool
setDone :: (MonadIO m) => UID -> Bool -> ReaderT ClientEnv m NoContent
getActorIDs :: (MonadIO m) => ReaderT ClientEnv m [UID]
getTurnOrder :: (MonadIO m) => ReaderT ClientEnv m [UID]
actorNames :: (MonadIO m) => ReaderT ClientEnv m [Text]
newActor :: (MonadIO m) => (Text, Text) -> ReaderT ClientEnv m UID
newCluster :: MonadIO m => ([Text], Text) -> ReaderT ClientEnv m [UID]
getNumDone :: (MonadIO m) => ReaderT ClientEnv m Int
multiLook :: (MonadIO m) => [UID] -> ReaderT ClientEnv m [[Square]]
roundLook :: (MonadIO m) => [UID] -> ReaderT ClientEnv m (Seq Grid)
getConfig :: (MonadIO m) => ReaderT ClientEnv m Config
getActor :<|> quitActor :<|> look :<|> act :<|> delAct :<|> getDone :<|> setDone
    :<|> getActorIDs :<|> getTurnOrder :<|> actorNames :<|> newActor :<|> newCluster
    :<|> getNumDone :<|> multiLook :<|> roundLook :<|> getConfig
    = hoistClient (flatten api) clientToReader $ client $ flatten api

saveTheWorld :: (World w) => IORef w -> IO ()
saveTheWorld wRef = do
    w <- readIORef wRef
    encodeFile worldBakPath w

runTurnAndSave :: (World w) => IORef w -> IO ()
runTurnAndSave wRef = do
    modifyIORef' wRef runTurn
    saveTheWorld wRef

runServer :: (World w) => Int -> w -> Config -> IO ()
runServer port initialWorld config = do
    wRef <- newIORef initialWorld


    case runDailyAt config of
        Nothing -> pass
        Just t -> void . forkIO . void . infinitely $ do
                waitUntilTimeOfDay t
                runTurnAndSave wRef


    termVar <- newEmptyMVar
    let terminate = void $ tryPutMVar termVar ()
    let waitForTermination = takeMVar termVar
    let gracefulTerm = CatchOnce $ saveTheWorld wRef >> terminate

    forM_ [softwareTermination, keyboardTermination, keyboardSignal] \sig ->
        installHandler sig gracefulTerm Nothing
    

    void $ race waitForTermination $ withStdoutLogger $ \aplogger -> do
        let settings = setPort port $ setLogger aplogger defaultSettings
        runSettings settings (serve api (hAPI config wRef))