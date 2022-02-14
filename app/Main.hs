module Main (main) where

import           Brick
import           Brick.Main
import           Brick.Widgets.Center
import           Brick.Widgets.Core
import           Brick.Widgets.Table
import           Data.Composition
import qualified Deque.Lazy as D
import           Graphics.Vty
import           GridTactics
import           Relude
import           Relude.Extra.Enum (next)
import           Relude.Unsafe (fromJust)
import           System.Random hiding (next)
type GTEvent = ()

newtype Name = Name {unName :: Text}
    deriving stock (Eq, Ord)
    deriving newtype IsString

data Resource = Actions | Hearts
    deriving stock (Eq, Enum, Bounded, Show)

singloot :: Resource -> Loot
singloot Actions = Loot {hearts = 0, actions = 1}
singloot Hearts = Loot {hearts = 1, actions = 0}

data AppState w = AppState
    { world :: w
    , currActor :: UID
    , currAction :: Action
    , currResource :: Resource
    }

gtApp :: (World w) => App (AppState w) GTEvent Name
gtApp = App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = (activateMouseMode $>)
    , appAttrMap = const $ attrMap defAttr []
    }

handleEvent :: (World w) => AppState w -> BrickEvent Name e -> EventM Name (Next (AppState w))
handleEvent s (VtyEvent (EvKey (KChar c) _modifiers)) = case c of
    'm' -> continue $ s {currAction = Dir Move N}
    's' -> continue $ s {currAction = Dir Shoot N}
    't' -> continue $ s {currAction = Dir (Throw mempty) N}
    'g' -> continue $ s {currAction = Dir Grab N}
    'd' -> continue $ s {currAction = Undir Die}
    ((`elem` ['=','+']) -> True) -> continue case currAction s of
        Dir (Throw l) d -> s {currAction = Dir (Throw (l <> singloot (currResource s))) d}
        _ -> s {currAction = Undir Hearts2HP}
    '-' -> continue case currAction s of
        Dir (Throw l) d -> s {currAction = Dir (Throw $ maybeToMonoid (l `without` singloot (currResource s))) d}
        _ -> s {currAction = Undir HP2Hearts}
    'z' -> continue $ modifyWorld (\w -> fromMaybe w $ execStateT (scatter $ Entity Nothing 3 (Just $ Loot {hearts = 0, actions = 5})) w) s
    '}' -> continue $ s {currResource = next . currResource $ s}
    ']' -> continue case currAction s of
        Dir a d -> s {currAction = Dir a (next d)}
        _ -> s
    'q' -> halt s
    _ -> continue s
handleEvent s (VtyEvent (EvKey KEnter _modifiers)) = continue $
    s {world = updateActor (pushAct $ currAction s) (currActor s) $ world s}
handleEvent s (VtyEvent (EvKey KHome _modifiers)) = continue $ s {world = runTurn $ world s}
handleEvent s (VtyEvent (EvKey KEsc _modifiers)) = halt s
handleEvent s _otherEvent = continue s

modifyWorld :: (w -> w) -> AppState w -> AppState w
modifyWorld f s = s {world = f . world $ s}

draw :: (World w) => AppState w -> [Widget Name]
draw s = let w = world s
             aID = currActor s
             a = lookupActor aID w
             worldTable = renderTable . grid2Table w $ view (vision a) (findActor aID w) w
         in  [ hCenter worldTable
             <=> hCenter (str ("Selected Action: " ++ show (currAction s)))
             <=> case currAction s of
                 Dir (Throw _) _ -> hCenter $ str ("Selected Resource: " ++ show (currResource s))
                 _ -> emptyWidget
             ]

grid2Table :: (World w) => w -> [[Square]] -> Table Name
grid2Table w = table . fmap (fmap $ renderSquare w)

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

main :: IO ()
main = do
    let w = mkWorld (mkStdGen 0) 10 :: SeqWorld
    let e = Entity Nothing 3 (Just $ Loot {hearts = 10, actions = 20})
    let (aID, w') = fromJust $ usingStateT w do
            c <- scatter e
            register $ Actor {name = "Test", coords = c, range = 3, vision = 5, queue = empty}
    _ <- defaultMain gtApp $ AppState {world = w', currActor = aID, currAction = Undir Die, currResource = Actions}
    pass
