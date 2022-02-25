module Main (main) where

import           Brick
import           Brick.Main
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Table
import           Data.Composition
import qualified Deque.Lazy as D
import           Graphics.Vty
import           GridTactics
import           Relude
import           Relude.Extra.Enum (prev, next)
import           Relude.Unsafe (fromJust, (!!))
import qualified Relude.Unsafe as Unsafe (head, tail)
import           System.Random hiding (next)
type GTEvent = ()

data Name = UndirActBtn UndirAction | DirActBtn DirAction
    deriving stock (Eq, Ord)

data Resource = Actions | Hearts
    deriving stock (Eq, Enum, Bounded, Show)

singloot :: Resource -> Loot
singloot Actions = Loot {hearts = 0, actions = 1}
singloot Hearts = Loot {hearts = 1, actions = 0}

data AppState w = AppState
    { world :: w
    , actorStack :: [UID]
    , currAction :: Action
    , currResource :: Resource
    , changingPlayers :: Bool
    }

gtApp :: (World w) => App (AppState w) GTEvent Name
gtApp = App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = (activateMouseMode $>)
    , appAttrMap = const $ attrMap defAttr []
    }

currActor :: AppState w -> UID
currActor = Unsafe.head . actorStack

selAction :: Action -> AppState w -> AppState w
selAction a s = s {currAction = a}

selUndirAct :: UndirAction -> AppState w -> AppState w
selUndirAct a = selAction $ Undir a

selDirAct :: (World w) => DirAction -> AppState w -> AppState w
selDirAct a s = s & case currAction s of
    Dir _ d -> selAction $ Dir a d
    Undir _ -> selAction $ Dir a
        (fromMaybe N . asum . fmap getDir . queue . lookupActor (currActor s) . world $ s)

handleEvent :: (World w) => AppState w -> BrickEvent Name e -> EventM Name (Next (AppState w))
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
        then s {actorStack = Unsafe.head (actorStack s) : drop 2 (actorStack s)}
        else s {changingPlayers = True}
    'y' -> continue $ if changingPlayers s
        then s
            { actorStack = Unsafe.tail $ actorStack s
            , currAction = Undir Wait
            , currResource = Actions
            , changingPlayers = False
            }
        else s
    'n' -> continue $ if changingPlayers s
        then s { changingPlayers = False
               , actorStack = dropWhile (/= Unsafe.head (actorStack s)) . Unsafe.tail $ actorStack s
               }
        else s
    'q' -> halt s
    _ -> continue s
handleEvent s (MouseDown (DirActBtn a) _ _ _) = continue $ selDirAct a s
handleEvent s (MouseDown (UndirActBtn a) _ _ _) = continue $ selUndirAct a s
handleEvent s (VtyEvent (EvKey KEnter _modifiers)) = continue . flip modifyWorld s $
    updateActor (pushAct $ currAction s) (currActor s)
handleEvent s (VtyEvent (EvKey KBS _modifiers)) = continue . flip modifyWorld s $
    flip updateActor (currActor s) \a ->
        a {queue = D.tail $ queue a}
handleEvent s (VtyEvent (EvKey KHome _modifiers)) = continue $ modifyWorld runTurn s
handleEvent s (VtyEvent (EvKey KEsc _modifiers)) = halt s
handleEvent s _otherEvent = continue s

modifyWorld :: (w -> w) -> AppState w -> AppState w
modifyWorld f s = s {world = f . world $ s}

defaultElem :: a -> [a] -> [a]
defaultElem x [] = [x]
defaultElem _ xs = xs



draw :: (World w) => AppState w -> [Widget Name]
draw s = let
    w = world s
    aID = currActor s
    nextAID = (actorStack s !! 1)
    a = lookupActor aID w
    nextA = lookupActor nextAID w
    c = findActor aID w
    worldTable = renderTable . grid2Table w $ view (vision a) c w
    queueBox = borderWithLabel (txt "Action Plan")
        (vBox (defaultElem (txtWrap "Nothing Planned (Yet!)")
        . fmap (strWrap . show) . reverse . toList . queue $ a))
    dispDActCost act = clickable (DirActBtn act) . txt $ show (cost (Dir act N))
    dispUActCost act = clickable (UndirActBtn act) . txt $ show (cost (Undir act))
    actMenu = joinBorders $ border
        (renderTable . alignRight 1 . rowBorders False . columnBorders False . surroundingBorder False . table $
            [ [txt "_: Action", txt "Cost"]
            , [clickable (DirActBtn Move) . txt $ "m: Move", dispDActCost Move]
            , [clickable (DirActBtn Shoot) . txt $ "s: Shoot", dispDActCost Shoot]
            , [clickable (DirActBtn (Throw mempty)) . txt  $ "t: Throw", dispDActCost (Throw mempty)]
            , [clickable (DirActBtn Grab) . txt $ "g: Grab", dispDActCost Grab]
            , [clickable (DirActBtn Heal) . txt $ "h: Heal", dispDActCost Heal]
            , [clickable (UndirActBtn HealMe) . txt $ "H: Heal Self", dispUActCost HealMe]
            , [clickable (UndirActBtn ShootMe) . txt $ "S: Shoot Self", dispUActCost ShootMe]
            , [clickable (UndirActBtn Wait) . txt $ "w: Wait", dispUActCost Wait]
            ])
    inventory = str ("Your Inventory: " ++ show (contents =<< getSquare c w))
    selectedAct = vBox . fmap hCenter $
        [ str ("Selected Action: " ++ show (currAction s))
        , case currAction s of
            Dir (Throw _) _ ->
                str ("Selected Resource: " ++ show (currResource s))
            _ -> emptyWidget
        ]
    lSidebar = vBox
        [ actMenu
        , case currAction s of
            Dir _ _ -> txt "[ and ] rotate direction"
            _ -> emptyWidget 
        , case currAction s of
            Dir (Throw _) _ -> vBox
                [ txt "+ and - adjust resource amount"
                , txt "{ and } select resource type"
                ]
            _ -> emptyWidget
        ]
    rSidebar = queueBox
    centerContent = vBox . fmap hCenter $
        [ txt ("You are " <> name a)
        , worldTable
        , inventory
        , selectedAct
        ]
    centerWidthPercent = 50
    sbarWidthPercent = (100 - centerWidthPercent) `div` 2
    controlScreen = vCenter $ hBox
        [ hLimitPercent sbarWidthPercent . hCenter $ lSidebar
        , hCenter centerContent
        , hLimitPercent sbarWidthPercent . hCenter $ rSidebar
        ]
    playerSwitchScreen = vCenter . vBox . fmap hCenter $
        [ txt $ "Ready, " <> name nextA <> "?"
        , txt "(y/n)"
        , txt "Press . for next player"
        ]
    in  [
        if changingPlayers s
            then playerSwitchScreen
            else controlScreen
        ]

grid2Table :: (World w) => w -> [[Square]] -> Table Name
grid2Table w = table . reverse . fmap (fmap $ renderSquare w)

renderSquare :: (World w) => w -> Square -> Widget Name
renderSquare = vLimit 2 . hLimit 5 . center . vBox . fmap txt .: sq2Texts

-- Give multiple lines of text.
sq2Texts :: (World w) => w -> Square -> [Text]
sq2Texts _ Nothing = [" "," "]
sq2Texts w (Just (Entity maID hp cont)) =
    [displayMaybeActorID w maID, show hp <> "/" <> show (hp + hearts (maybeToMonoid cont))]

displayMaybeActorID :: (World w) => w -> Maybe UID -> Text
displayMaybeActorID _w Nothing = ""
displayMaybeActorID w (Just aID) = name $ lookupActor aID w


activateMouseMode :: EventM n ()
activateMouseMode = do
  vty <- Brick.Main.getVtyHandle
  let output = outputIface vty
  when (supportsMode output Mouse) $
    liftIO $ setMode output Mouse True

modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = do
    s <- get
    s' <- lift $ f s
    put s'

populateWorld :: (World w) => Int -> StateT w Maybe ()
populateWorld numScatters = do
    replicateM_ numScatters . scatter $ Entity Nothing 2 (Just $ Loot {hearts = 0, actions = 1})
    modifyM $ scatterActors ["Matt", "Batt", "Catt", "Datt", "Nahan", "Bahan", "Cahan", "Dahan"]
        (Entity Nothing 3 (Just $ Loot {hearts = 2, actions = 0}))
        (Actor {name = "", coords = (0,0), range = 2, vision = 3, queue = empty})
    modify runTurn

main :: IO ()
main = do
    gen <- getStdGen
    let width = 8
    let area = width ^ (2 :: Int)
    let w = mkWorld gen width
    let w' :: SeqWorld = fromJust $ execStateT (populateWorld (area `div` 2)) w
    _ <- defaultMain gtApp $ AppState
        { world = w'
        , actorStack = Unsafe.head (actors w') : cycle (actors w')
        , currAction = Undir Wait
        , currResource = Actions
        , changingPlayers = True
        }
    pass
