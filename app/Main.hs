module Main (main) where

import           Brick
import           Brick.Main
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Table
import           Data.Composition
import           Graphics.Vty
import           GridTactics
import           Relude
import           Relude.Extra.Enum (prev, next)
import           Relude.Unsafe (fromJust)
import qualified Relude.Unsafe as Unsafe (head)
import           System.Random hiding (next)
type GTEvent = ()

newtype Name = Name {_unName :: Text}
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
    '{' -> continue $ s {currResource = prev . currResource $ s}
    '[' -> continue case currAction s of
        Dir a d -> s {currAction = Dir a (prev d)}
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

defaultElem :: a -> [a] -> [a]
defaultElem x [] = [x]
defaultElem _ xs = xs

draw :: (World w) => AppState w -> [Widget Name]
draw s = let w = world s
             aID = currActor s
             a = lookupActor aID w
             c = findActor aID w
             worldTable = renderTable . grid2Table w $ view (vision a) c w
         in  [ (vBox . fmap hCenter $ [worldTable
                , str ("Your Inventory: " ++ show (contents =<< getSquare c w))
                , str ("Selected Action: " ++ show (currAction s))
                , case currAction s of
                    Dir (Throw _) _ -> hCenter $ str ("Selected Resource: " ++ show (currResource s))
                    _ -> emptyWidget
                ]) <+> borderWithLabel (txt "Your Action Queue:")
                    (hLimit 50 . hCenter $ vBox (defaultElem (txt "No Actions Planned (Yet!)")
                    . fmap (str . show) . reverse . toList . queue . lookupActor aID $ w))
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

main :: IO ()
main = do
    let w = mkWorld (mkStdGen 0) 30 :: SeqWorld
    let w' = fromJust . scatterActors ["Guy", "Jean", "Marie", "Anne", "Luc"]
             (Entity Nothing 3 (Just $ Loot {hearts = 2, actions = 1}))
             (Actor {name = "", coords = (0,0), range = 2, vision = 3, queue = empty})
             $ w
    _ <- defaultMain gtApp $ AppState
        { world = w'
        , currActor = Unsafe.head . actors $ w'
        , currAction = Undir Die
        , currResource = Actions
        }
    pass
