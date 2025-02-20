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
    , getNumDone
    , multiLook
    , roundLook
    , getConfig

    , Config (..)
    , runServer
    ) where

import           Data.Aeson 
import qualified Deque.Lazy as D
import           Mechanics
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger (withStdoutLogger)
import           Relude
import           Servant
import           Servant.API.Flatten
import           Servant.Client
import           Util
import           WebInstances ()




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
            :<|> NumDoneAPI
            :<|> MultiViewAPI
            :<|> RoundViewAPI

type UIDsAPI = Get '[JSON] [UID]

type OrderAPI = "order" :> Get '[JSON] [UID]

type NamesAPI = "names" :> Get '[JSON] [Text]

type NewAPI = "new"
    :> ReqBody '[JSON] Text -- Provide a name
    :> Post '[JSON] UID

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
hPostDone aID _conf ref isDone = stateToIO ref do
    modify $ updateActor (\a -> a {done = isDone}) aID
    nDone <- gets numDone
    nActors <- gets (length . actors)
    when (nActors > 0 && nDone == nActors) $
        modify runTurn
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

hNew :: (World w) => Config -> IORef w -> Text -> Handler UID
hNew conf ref name' = do
    maybeAID <- stateToIO ref . maybeState $
        scatterActor ((pawnTemplate conf) {ename=Just name'}) ((actorTemplate conf) {aname=name'})
    hoistEither' . maybeToRight err500 $ maybeAID

hNumDone :: (World w) => Config -> IORef w -> Server NumDoneAPI
hNumDone _conf = doState $ gets numDone

hMultiView :: (World w) => Config -> IORef w -> Server MultiViewAPI
hMultiView _conf ref aIDs = stateToIO ref do
    w <- get
    return $ multiView ((\aID -> (vision w $ lookupActor aID w, findActor aID w)) <$> aIDs) w

hRoundView :: (World w) => Config -> IORef w -> Server RoundViewAPI
hRoundView _conf ref aIDs = stateToIO ref do
    w <- get
    pass -- to quiet hlint
    return $ roundView aIDs w

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
newActor :: (MonadIO m) => Text -> ReaderT ClientEnv m UID
getNumDone :: (MonadIO m) => ReaderT ClientEnv m Int
multiLook :: (MonadIO m) => [UID] -> ReaderT ClientEnv m [[Square]]
roundLook :: (MonadIO m) => [UID] -> ReaderT ClientEnv m (Seq Grid)
getConfig :: (MonadIO m) => ReaderT ClientEnv m Config
getActor :<|> quitActor :<|> look :<|> act :<|> delAct :<|> getDone :<|> setDone
    :<|> getActorIDs :<|> getTurnOrder :<|> actorNames :<|> newActor :<|> getNumDone
    :<|> multiLook :<|> roundLook :<|> getConfig
    = hoistClient (flatten api) clientToReader $ client $ flatten api



runServer :: (World w) => Int -> w -> Config -> IO ()
runServer port initialWorld config = do
    wRef <- newIORef initialWorld
    withStdoutLogger $ \aplogger -> do
        let settings = setPort port $ setLogger aplogger defaultSettings
        runSettings settings (serve api (hAPI config wRef))