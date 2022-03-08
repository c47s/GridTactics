{-# LANGUAGE NoMonomorphismRestriction #-}

module Client (main) where

import Brick
import Brick.Main
import Control.Monad.Except
import Graphics.Vty
import GridTactics hiding (World(..))
import Network.HTTP.Client
import Relude
import Relude.Extra.Enum (prev, next)
import Servant
import Servant.API.Flatten
import Servant.Client
import System.Console.Haskeline



getActor :: (MonadIO m) => UID -> ReaderT ClientEnv m Actor
delActor :: MonadIO m => UID -> ReaderT ClientEnv m NoContent
look :: (MonadIO m) => UID -> ReaderT ClientEnv m [[Square]]
act :: (MonadIO m) => UID -> Action -> ReaderT ClientEnv m NoContent
delAct :: (MonadIO m) => UID -> ReaderT ClientEnv m NoContent
getDone :: (MonadIO m) => UID -> ReaderT ClientEnv m Bool
setDone :: (MonadIO m) => UID -> Bool -> ReaderT ClientEnv m NoContent
actorNames :: (MonadIO m) => ReaderT ClientEnv m [Text]
newActor :: (MonadIO m) => Text -> ReaderT ClientEnv m UID
numDone :: (MonadIO m) => ReaderT ClientEnv m Int
self :<|> look :<|> act :<|> delAct :<|> getDone :<|> setDone
    :<|> actorNames :<|> newActor :<|> numDone 
    = hoistClient (flatten api) clientToReader $ client $ flatten api

-- Crashes on any error!!!
clientToReader :: (MonadIO m) => ClientM a -> ReaderT ClientEnv m a
clientToReader cl = (either (error . show) id <$>) . liftIO . runClientM cl =<< ask

inApp :: AppState -> ReaderT ClientEnv (EventM Name) a -> EventM Name a
inApp s r = runReaderT r (clientEnv s)



type GTEvent = ()

gtApp :: App AppState GTEvent Name
gtApp = App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = startEvent
    , appAttrMap = const $ attrMap defAttr []
    }

selAction :: Action -> AppState -> AppState
selAction a s = s {currAction = a}

selUndirAct :: UndirAction -> AppState -> AppState
selUndirAct a = selAction $ Undir a

selDirAct :: DirAction -> AppState -> AppState
selDirAct a s = s & case currAction s of
    Dir _ d -> selAction $ Dir a d
    Undir _ -> selAction $ Dir a
        (fromMaybe N . asum . fmap getDir . queue $ currActor s)

updateFromServer :: AppState -> EventM Name AppState
updateFromServer s = do
    currActor' <- inApp s $ self    $ currActorID s
    nextActor' <- inApp s $ self    $ nextActorID s
    currView'  <- inApp s $ look    $ currActorID s
    currDone'  <- inApp s $ getDone $ currActorID s
    currNumDone' <- inApp s numDone
    currNames' <- inApp s actorNames
    return s
        { currActor = currActor'
        , nextActor = nextActor'
        , currView = currView'
        , currDone = currDone'
        , currNumDone = currNumDone'
        , currNames = currNames'
        }

startEvent :: AppState -> EventM Name AppState
startEvent = updateFromServer <=< (activateMouseMode $>)

continue' :: AppState -> EventM Name (Next AppState)
continue' = continue <=< updateFromServer

handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvKey (KChar c) _modifiers)) = case c of
    'm' -> continue $ selDirAct Move s
    's' -> continue $ selDirAct Shoot s
    't' -> continue $ selDirAct (Throw mempty) s
    'g' -> continue $ selDirAct Grab s
    'h' -> continue $ selDirAct Heal s
    'd' -> continue $ selUndirAct Die s
    'H' -> continue $ selUndirAct HealMe s
    'S' -> continue $ selUndirAct ShootMe s
    'w' -> continue $ selUndirAct Wait s
    ((`elem` ['=','+']) -> True) -> continue case currAction s of
        Dir (Throw l) d -> s {currAction = Dir (Throw (l <> singloot (currResource s))) d}
        _ -> s
    '-' -> continue case currAction s of
        Dir (Throw l) d -> s {currAction = Dir (Throw $ maybeToMonoid (l `without` singloot (currResource s))) d}
        _ -> s
    '}' -> continue $ s {currResource = next . currResource $ s}
    ']' -> continue case currAction s of
        Dir a d -> s {currAction = Dir a (next d)}
        _ -> s
    '{' -> continue $ s {currResource = prev . currResource $ s}
    '[' -> continue case currAction s of
        Dir a d -> s {currAction = Dir a (prev d)}
        _ -> s
    '.' -> continue if changingPlayers s
        then s {actorIDs = rotate $ actorIDs s}
        else s {changingPlayers = True}
    ',' -> continue if changingPlayers s
        then s {actorIDs = etator $ actorIDs s}
        else s
    'y' -> continue $ if changingPlayers s
        then s
            { actorIDs = rotate $ actorIDs s
            , currAction = Undir Wait
            , currResource = Actions
            , changingPlayers = False
            }
        else s
    'n' -> continue $ if changingPlayers s
        then s { changingPlayers = False
               , actorIDs = rotateTo (currActorID s) $ actorIDs s
               }
        else s
    'q' -> halt s
    _ -> continue s
handleEvent s (MouseDown (DirActBtn a) _ _ _) = continue $ selDirAct a s
handleEvent s (MouseDown (UndirActBtn a) _ _ _) = continue $ selUndirAct a s
handleEvent s (MouseDown (DoneBtn aID) _ _ _) = inApp s do
    thisDone <- getDone aID
    _ <- setDone aID $ not thisDone
    lift $ continue' s
handleEvent s (VtyEvent (EvKey KEnter _modifiers)) = inApp s (act (currActorID s) (currAction s)) >> continue' s
handleEvent s (VtyEvent (EvKey KBS _modifiers)) = inApp s (delAct $ currActorID s) >> continue' s
handleEvent s (VtyEvent (EvKey KEsc _modifiers)) = halt s
handleEvent s _otherEvent = continue s



activateMouseMode :: EventM n ()
activateMouseMode = do
  vty <- Brick.Main.getVtyHandle
  let output = outputIface vty
  when (supportsMode output Mouse) $
    liftIO $ setMode output Mouse True

main :: IO ()
main = runInputT defaultSettings do
    outputStrLn "Hello!"
    outputStrLn $ "Welcome to the " ++ productName ++ " client."

    outputStrLn ""
    hostName <- untilJust do
        outputStrLn "Enter hostname (IP or domain name):"
        getInputLineWithInitial "> " ("localhost","")

    outputStrLn ""
    port <- untilValid do
        outputStrLn "Enter port:"
        getInputLineWithInitial "> " ("42069","")
    
    outputStrLn ""
    initName <- untilJust do
        outputStrLn "Enter player name:"
        getInputLineWithInitial "> " ("Jean","")

    manager <- liftIO $ newManager defaultManagerSettings

    let baseUrl = BaseUrl Http (fromString hostName) port ""

    let env = mkClientEnv manager baseUrl

    outputStrLn ""
    outputStrLn "Contacting server..."
    initActorID <- runReaderT (newActor $ fromString initName) env

    outputStrLn ""
    outputStrLn "Starting UI..."
    _ <- liftIO $ defaultMain gtApp $ AppState
        { clientEnv = env
        , actorIDs = one initActorID
        , currAction = Undir Wait
        , currResource = Actions
        , changingPlayers = True
        , currActor = error "currActor not yet initialized"
        , nextActor = error "nextActor not yet initialized"
        , currView = error "currView not yet initialized"
        , currDone = error "currDone not yet initialized"
        , currNumDone = error "currNumDone not yet initialized"
        , currNames = error "currNames not yet initialized"
        }
    outputStrLn ""
    outputStrLn "Goodbye!"