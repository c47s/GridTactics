-- {-# LANGUAGE OverloadedStrings #-}

module API
    ( API
    , runServer
    ) where

import Mechanics
import Network.Wai.Handler.Warp
import Relude
import Servant
import WebInstances ()



type Modify ts a = Get ts a
              :<|> ReqBody ts a :> PostNoContent

type ActorAPI = Capture "id" UID
    :> ( ActorSelfAPI
    :<|> ViewAPI
    :<|> ActAPI
    :<|> DoneAPI
    )
type ActorSelfAPI = Get '[JSON] Actor
type ViewAPI = "view" :> Get '[JSON] Grid
type ActAPI = "act" :> ReqBody '[JSON] Action :> PostNoContent
type DoneAPI = "done" :> Modify '[JSON] Bool

type ActorsAPI = NamesAPI
            :<|> NewAPI
            :<|> NumDoneAPI
type NamesAPI = "names" :> Get '[JSON] [Text]
type NewAPI = "new"
    :> ReqBody '[JSON] Text -- Provide a name
    :> Post '[JSON] UID
type NumDoneAPI = "done" :> Get '[JSON] Int

type API = "actor"  :> ActorAPI
      :<|> "actors" :> ActorsAPI



stateToIO :: (MonadIO io) => IORef s -> State s a -> io a
stateToIO ref s = liftIO do
    before <- readIORef ref
    modifyIORef' ref $ execState s
    return $ evalState s before

doState :: (MonadIO io) => State s a -> IORef s -> io a
doState = flip stateToIO



hActorSelf :: (World w) => UID -> IORef w -> Server ActorSelfAPI
hActorSelf aID = doState $ gets $ lookupActor aID

hView :: (World w) => UID -> IORef w -> Server ViewAPI
hView aID = doState do
    w <- get
    gets $ view (vision $ lookupActor aID w) (findActor aID w)

hAct :: (World w) => UID -> IORef w -> Server ActAPI
hAct aID ref act = stateToIO ref do
    modify $ updateActor (pushAct act) aID
    return NoContent

hDone :: (World w) => UID -> IORef w -> Server DoneAPI
hDone = undefined

hNames :: (World w) => IORef w -> Server NamesAPI
hNames = undefined

hNew :: (World w) => IORef w -> Server NewAPI
hNew = undefined

hNumDone :: (World w) => IORef w -> Server NumDoneAPI
hNumDone = undefined

hActor :: (World w) => IORef w -> Server ActorAPI
hActor ref aID = hActorSelf aID ref
            :<|> hView aID ref
            :<|> hAct aID ref
            :<|> hDone aID ref

hActors :: (World w) => IORef w -> Server ActorsAPI
hActors ref = hNames ref
     :<|> hNew ref
     :<|> hNumDone ref

hAPI :: (World w) => IORef w -> Server API
hAPI ref = hActor ref
  :<|> hActors ref



runServer :: (World w) => w -> IO ()
runServer initialWorld = do
    wRef <- newIORef initialWorld
    
    run 8081 (serve (Proxy :: Proxy API) (hAPI wRef))