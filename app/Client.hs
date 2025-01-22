{-# LANGUAGE NoMonomorphismRestriction #-}

module Client (main) where

import           Brick
import           Brick.BChan
import           Brick.Main
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad.Except
import qualified Data.Bimap as Bap
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Graphics.Vty
import           Graphics.Vty as V hiding (Default)
import           GridTactics hiding (World(..))
import           Network.HTTP.Client
import           Relude
import           Relude.Extra.Enum (next, prev)
import           Servant.Client
import           System.Console.Haskeline
import           System.Random hiding (next)



-- | Run an API client request, getting the ClientEnv from the given app.
inApp :: AppState -> ReaderT ClientEnv (EventM Name) a -> EventM Name a
inApp s r = runReaderT r (clientEnv s)

newtype GTEvent = Tick Int -- ^ Number of seconds since app start

-- | The GridTactics Brick app
gtApp :: App AppState GTEvent Name
gtApp = App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = startEvent
    , appAttrMap = const $ attrMap defAttr
        [ (selfAttr, bg V.blue)
        , (friendlyAttr, bg V.green)
        , (hostileAttr, bg V.red)
        , (wallAttr, bg V.black)
        , (fogAttr, V.white `Brick.on` V.white)
        ]
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

-- | Grab and cache info from the server
updateFromServer :: AppState -> EventM Name AppState
updateFromServer s = do
    currActor' <- inApp s $ getActor  $ currActorID s
    nextActor' <- inApp s $ getActor  $ nextActorID s
    currView'  <- inApp s $ look      $ currActorID s
    currMap'   <- inApp s $ multiLook $ toList $ actorIDs s
    currReplay' <- inApp s $ roundLook $ toList $ actorIDs s
    currDone'  <- inApp s $ getDone   $ currActorID s
    currNumDone' <- inApp s getNumDone
    currNames' <- inApp s actorNames
    currOrder' <- inApp s getTurnOrder
    return s
        { currActor = currActor'
        , nextActor = nextActor'
        , currView = currView'
        , currMap = currMap'
        , currReplay = currReplay'
        , currDone = currDone'
        , currNumDone = currNumDone'
        , currNames = currNames'
        , currOrder = currOrder'
        }

startEvent :: AppState -> EventM Name AppState
startEvent = updateFromServer <=< (activateMouseMode $>)

-- | Continue after updating cached values from server
continue' :: AppState -> EventM Name (Next AppState)
continue' = continue <=< updateFromServer

quit :: AppState -> EventM Name (Next AppState)
quit s = do
    traverse_ (inApp s <$> quitActor) (actorIDs s)
    halt s

toggleDone :: UID -> AppState -> EventM Name (Next AppState)
toggleDone aID s = inApp s do
    thisDone <- getDone aID
    _ <- setDone aID $ not thisDone
    lift $ continue' s

doUIAction :: UIAction -> AppState -> EventM Name (Next AppState)
doUIAction (SelUndirAct a) s = continue $ selUndirAct a s
doUIAction (SelDirAct a) s = continue $ selDirAct a s
doUIAction Increment s = continue case currAction s of
    Dir (Throw l) d -> s {currAction = Dir (Throw (l <> singloot (currResource s) 1)) d}
    Dir (Build n) d -> s {currAction = Dir (Build $ n + 1) d}
    _ -> s
doUIAction Decrement s = continue case currAction s of
    Dir (Throw l) d -> s {currAction = Dir (Throw $ maybeToMonoid (l `without` singloot (currResource s) 1)) d}
    Dir (Build n) d -> s {currAction = Dir (Build $ max 0 $ n - 1) d}
    _ -> s
doUIAction RotL s = continue case currAction s of
    Dir a d -> s {currAction = Dir a (prev d)}
    _ -> s
doUIAction RotR s = continue case currAction s of
    Dir a d -> s {currAction = Dir a (next d)}
    _ -> s
doUIAction RotL' s = continue $ s {currResource = prev . currResource $ s}
doUIAction RotR' s = continue $ s {currResource = next . currResource $ s}
doUIAction PlayerL s = continue' if True -- changingPlayers s
    then s {actorIDs = etator $ actorIDs s}
    else s {changingPlayers = True}
doUIAction PlayerR s = continue' if True --changingPlayers s
    then s {actorIDs = rotate $ actorIDs s}
    else s {changingPlayers = True}
doUIAction Yes s = continue' $ if changingPlayers s
    then s
        { currAction = Undir Wait
        , currResource = Actions
        , changingPlayers = False
        }
    else s
doUIAction No s = continue $ if changingPlayers s
    then s { changingPlayers = False
            , actorIDs = rotateTo (currActorID s) $ actorIDs s
            }
    else s
doUIAction ViewMap s = continue' $ s {viewingMap = not . viewingMap $ s}
doUIAction ViewReplay s = continue' $ s { viewingReplay = not . viewingReplay $ s
                                        , replayIndex = -1
                                        , replayTick = 0
                                        }
doUIAction (SwitchTo aID) s = continue' $ s {actorIDs = rotateTo aID $ actorIDs s}
doUIAction ToggleDone s = toggleDone (currActorID s) s
doUIAction SubmAction s = inApp s (act (currActorID s) (currAction s)) >> continue' s
doUIAction DelAction s = inApp s (delAct $ currActorID s) >> continue' s
doUIAction ClaimAllPawns s = do
    allActors <- inApp s getActorIDs
    continue' $ s {actorIDs = fromList allActors}
doUIAction RemovePawn s = do
    _ <- inApp s . quitActor $ currActorID s
    continue' $ s {actorIDs = Seq.filter (/= currActorID s) $ actorIDs s}
doUIAction DisownPawn s = continue' $ s {actorIDs = Seq.filter (/= currActorID s) $ actorIDs s}
doUIAction Quit s = quit s
doUIAction (Also uiA) s = doUIAction uiA s

handleEvent :: AppState -> BrickEvent Name GTEvent -> EventM Name (Next AppState)
handleEvent s (VtyEvent (EvKey key _modifiers)) = maybe continue doUIAction (Bap.lookup key (keybinds s)) s
handleEvent s (MouseDown (Btn uiAct) _ _ _) = doUIAction uiAct s
handleEvent s (AppEvent (Tick n)) = do
    let s' = if viewingReplay s && n >= replayTick s
        then if replayIndex s + 1 == length (currReplay s)
            then s {viewingReplay = False, replayIndex = 0}
            else s
                { replayIndex = replayIndex s + 1
                , replayTick = n + round (30 * 0.99 ^ (replayIndex s + 1) :: Double)
                }
        else s
    s' & if currDone s || n `mod` 500 == 0 then continue' else continue
handleEvent s _otherEvent = continue s



activateMouseMode :: EventM n ()
activateMouseMode = do
  vty <- Brick.Main.getVtyHandle
  let output = outputIface vty
  when (supportsMode output Mouse) $
    liftIO $ setMode output Mouse True

defaultKeybinds :: Bap.Bimap Key UIAction
defaultKeybinds = Bap.fromList
    [ (KChar 'm', SelDirAct Move)
    , (KChar 's', SelDirAct Shoot)
    , (KChar 'B', SelDirAct Blast)
    , (KChar 't', SelDirAct (Throw mempty))
    , (KChar 'g', SelDirAct Grab)
    , (KChar 'r', SelDirAct Repair)
    , (KChar 'b', SelDirAct (Build 1))
    , (KChar 'R', SelUndirAct RepairMe)
    , (KChar 'S', SelUndirAct ShootMe)
    , (KChar 'e', SelUndirAct Recycle)
    , (KChar 'G', SelUndirAct UpRange)
    , (KChar 'v', SelUndirAct UpVision)
    , (KChar 'w', SelUndirAct Wait)
    , (KChar '+', Increment)
    , (KChar '=', Also Increment)
    , (KChar '-', Decrement)
    , (KChar '[', RotL)
    , (KChar ']', RotR)
    , (KChar '{', RotL')
    , (KChar '}', RotR')
    , (KChar ',', PlayerL)
    , (KChar '.', PlayerR)
    , (KChar 'y', Yes)
    , (KChar 'n', No)
    , (KChar 'M', ViewMap)
    , (KChar 'V', ViewReplay)
    , (KChar 'd', ToggleDone)
    , (KChar 'q', Quit)
    , (KChar 'Z', ClaimAllPawns)
    , (KChar 'X', DisownPawn)
    , (KChar 'Q', RemovePawn)
    , (KEnter, SubmAction)
    , (KDel, DelAction)
    , (KBS, Also DelAction)
    ]

frenchNames :: [Text]
frenchNames =
    [ "Aimée", "Chloé", "Fleur", "Jewel", "Jolie", "Lucie", "Manon"
    , "Marie", "Noel ", "Renée", "Zoe  ", "Brice", "Denis", "Guy  "
    , "Hugo ", "Jean ", "Jules", "Leo  ", "Louis", "Luc  ", "Marc "
    , "Noel ", "Paul ", "René ", "Roy  ", "Sacha", "Simon", "Théo "
    ]

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
    
    manager <- liftIO $ newManager defaultManagerSettings

    let baseUrl = BaseUrl Http (fromString hostName) port ""

    let env = mkClientEnv manager baseUrl

    outputStrLn ""
    outputStrLn "Contacting server..."
    config <- runReaderT getConfig env
    
    g1 <- newStdGen
    g2 <- newStdGen
    let baseName1 = randElem g1 frenchNames
    let baseName2 = randElem g2 frenchNames
    initActorIDs <- forM [1..(pawnsPerClient config)] $ \n -> do
        outputStrLn ""
        newName <- untilJustAnd nonBlank do
            g3 <- newStdGen
            g4 <- newStdGen
            g5 <- newStdGen
            let suggestedName = T.takeWhile (/= ' ') $ mixText g3 baseName1 $
                    mixText g4 baseName2 (randElem g5 frenchNames)
                    

            outputStrLn ("Enter pawn " <> show n <> " name:")
            getInputLineWithInitial "> " (toString suggestedName,"")
        runReaderT (newActor $ fromString newName) env

    let initialState = AppState
            { clientEnv = env
            , keybinds = defaultKeybinds
            , actorIDs = fromList initActorIDs
            , currAction = Undir Wait
            , currResource = Actions
            , changingPlayers = True
            , viewingMap = False
            , viewingReplay = False
            , replayIndex = 0
            , replayTick = 0
            , currActor = error "currActor not yet initialized"
            , nextActor = error "nextActor not yet initialized"
            , currView = error "currView not yet initialized"
            , currMap = error "currMap not yet initialized"
            , currReplay = error "currReplay not yet initialized"
            , currDone = error "currDone not yet initialized"
            , currNumDone = error "currNumDone not yet initialized"
            , currNames = error "currNames not yet initialized"
            , currOrder = error "currOrder not yet initialized"
            }

    outputStrLn ""
    outputStrLn "Starting UI..."

    chan <- liftIO $ newBChan 10
    tickCounter <- newIORef 1

    _ <- liftIO $ forkIO $ void $ infinitely $ do
        n <- readIORef tickCounter
        writeIORef tickCounter (n + 1)

        writeBChan chan (Tick n)
        threadDelay 10000 -- centisecond
    
    let buildVty = mkVty defaultConfig
    initialVty <- liftIO buildVty

    _ <- liftIO $ customMain initialVty buildVty (Just chan) gtApp initialState

    outputStrLn ""
    outputStrLn "Goodbye!"
