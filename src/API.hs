-- {-# LANGUAGE OverloadedStrings #-}

module API
    ( API
    , Config (..)
    , runServer
    ) where

import Control.Monad.Except
import Mechanics
import Network.Wai.Handler.Warp
import Relude
import Servant
import WebInstances ()



stateToIO :: (MonadIO io) => IORef s -> State s a -> io a
stateToIO ref s = liftIO do
    before <- readIORef ref
    modifyIORef' ref $ execState s
    return $ evalState s before

doState :: (MonadIO io) => State s a -> IORef s -> io a
doState = flip stateToIO

maybeState :: StateT s Maybe a -> State s (Maybe a)
maybeState t = state \s -> let
    m = runStateT t s
    a = fst <$> m
    s' = maybe s snd m
    in (a,s')

hoistExcept :: (MonadError e m) => Except e a -> m a
hoistExcept exc = do
    case runExcept exc of
        Left e  -> throwError e
        Right a -> return a

hoistEither' :: (MonadError e m) => Either e a -> m a
hoistEither' = hoistExcept . hoistEither



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



data Config = Config
    { pawnTemplate :: Entity
    , actorTemplate :: Actor
    }



hActorSelf :: (World w) => UID -> Config -> IORef w -> Server ActorSelfAPI
hActorSelf aID _conf = doState $ gets $ lookupActor aID

hView :: (World w) => UID -> Config -> IORef w -> Server ViewAPI
hView aID _conf = doState do
    w <- get
    gets $ view (vision $ lookupActor aID w) (findActor aID w)

hAct :: (World w) => UID -> Config -> IORef w -> Server ActAPI
hAct aID _conf ref act = stateToIO ref do
    modify $ updateActor (pushAct act) aID
    return NoContent

hGetDone :: (World w) => UID -> Config -> IORef w -> Handler Bool
hGetDone aID _conf = doState $ gets $ done . lookupActor aID

hPostDone :: (World w) => UID -> Config -> IORef w -> Bool -> Handler NoContent
hPostDone aID _conf ref isDone = stateToIO ref do
    modify $ updateActor (\a -> a {done = isDone}) aID
    return NoContent

hDone :: (World w) => UID -> Config -> IORef w -> Server DoneAPI
hDone aID conf ref = hGetDone aID conf ref
              :<|> hPostDone aID conf ref

hNames :: (World w) => Config -> IORef w -> Server NamesAPI
hNames _conf = doState $ gets $ \w -> name . flip lookupActor w <$> actors w

hNew :: (World w) => Config -> IORef w -> Text -> Handler UID
hNew conf ref name' = do
    maybeAID <- stateToIO ref . maybeState $
        scatterActor (pawnTemplate conf) ((actorTemplate conf) {name=name'})
    hoistEither' . maybeToRight err500 $ maybeAID

hNumDone :: (World w) => Config -> IORef w -> Server NumDoneAPI
hNumDone _conf = doState $ gets $ sum . map fromEnum
    . \w -> done . flip lookupActor w <$> actors w

hActor :: (World w) => Config -> IORef w -> Server ActorAPI
hActor conf ref aID = hActorSelf aID conf ref
            :<|> hView aID conf ref
            :<|> hAct aID conf ref
            :<|> hDone aID conf ref

hActors :: (World w) => Config -> IORef w -> Server ActorsAPI
hActors conf ref = hNames conf ref
     :<|> hNew conf ref
     :<|> hNumDone conf ref

hAPI :: (World w) => Config -> IORef w -> Server API
hAPI conf ref = hActor conf ref
  :<|> hActors conf ref



runServer :: (World w) => Int -> w -> Config -> IO ()
runServer port initialWorld config = do
    wRef <- newIORef initialWorld
    run port (serve (Proxy :: Proxy API) (hAPI config wRef))