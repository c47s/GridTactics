module TUI
    ( draw
    , AppState(..)
    , currActorID
    , nextActorID
    , Name(..)
    , Resource(..)
    , singloot
    ) where

import           Brick.Types
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Core
import           Brick.Widgets.Table
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Mechanics
import           Relude
import           Servant.Client
import           Util

data AppState = AppState
    { clientEnv :: ClientEnv
    , actorIDs :: Seq UID
    , currAction :: Action
    , currResource :: Resource
    , changingPlayers :: Bool
    , currActor :: Actor
    , nextActor :: Actor
    , currView :: Grid
    , currDone :: Bool
    , currNumDone :: Int
    , currNames :: [Text]
    }

currActorID :: AppState -> UID
currActorID = fromJust . (Seq.!? 0) . actorIDs

nextActorID :: AppState -> UID
nextActorID = fromJust . (Seq.!? 0) . rotate . actorIDs



data Name = UndirActBtn UndirAction | DirActBtn DirAction | DoneBtn UID
    deriving stock (Eq, Ord)



grid2Table :: [[Square]] -> Table Name
grid2Table = table . reverse . fmap (fmap renderSquare)

renderSquare :: Square -> Widget Name
renderSquare = vLimit 2 . hLimit 5 . center . vBox . fmap txt . sq2Texts

-- A list of lines to display
sq2Texts :: Square -> [Text]
sq2Texts Nothing = [" "," "]
sq2Texts (Just (Entity _ mname hp cont)) =
    [fromMaybe "" mname, show hp <> "/" <> show (hp + res Hearts cont)]

res2Text :: Resource -> Int -> Text
res2Text r n = show n <> " " <> show r

list2Text :: [Text] -> Text
list2Text = maybe "Nothing" nempty2Text . nonEmpty

nempty2Text :: NonEmpty Text -> Text
nempty2Text xs
    | length xs == 1 = head xs
    | length xs == 2 = maybe "" head (nonEmpty $ tail xs) <> " and " <> head xs
    | otherwise      = (T.concat . intersperse ", " $ tail xs) <> " and " <> head xs

loot2Text :: Loot -> Text
loot2Text = list2Text . toList . Map.mapWithKey res2Text . Map.filter (> 0) . unLoot

dir2Text :: Mechanics.Direction -> Text
dir2Text N = "↑"
dir2Text NE = "↗︎"
dir2Text E = "→"
dir2Text SE = "↘︎"
dir2Text S = "↓"
dir2Text SW = "↙︎"
dir2Text W = "←"
dir2Text NW = "↖︎"

act2Text :: Action -> Text
act2Text (Dir (Throw loot) dir) = dir2Text dir <> " Throw " <> loot2Text loot
act2Text (Dir act dir) = dir2Text dir <> " " <> show act <> " "
act2Text (Undir HealMe) = "Heal Self"
act2Text (Undir ShootMe) = "Shoot Self"
act2Text (Undir act) = show act

draw :: AppState -> [Widget Name]
draw s = let
    me = currActor s
    mySq = middle . middle $ currView s
    nextA = nextActor s

    doneBox = joinBorders $ border $ vBox
        [ clickable (DoneBtn $ currActorID s) $ txtWrap if currDone s
            then "Waiting for other players to finish..."
            else "d: End Turn"
        , hBorder
        , txtWrap $ show (currNumDone s) <> "/" <> show (length $ currNames s)
            <> " players are done"
        ]

    queueBox = borderWithLabel (txt "Action Plan")
        (vBox (defaultElem (txtWrap "Nothing Planned (Yet!)")
        . fmap (txtWrap . act2Text) . reverse . toList . queue $ me))

    worldTable = renderTable . grid2Table $ currView s

    dispDActCost actn = clickable (DirActBtn actn) . txt $ show (cost (Dir actn N))
    dispUActCost actn = clickable (UndirActBtn actn) . txt $ show (cost (Undir actn))

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
    
    inventory = txt ("Your Inventory: " <> loot2Text (maybeToMonoid (contents <$> mySq)))

    selectedAct = vBox . fmap hCenter $
        [ txt ("Selected Action: " <> act2Text (currAction s))
        , case currAction s of
            Dir (Throw _) _ ->
                str ("Selected Resource: " <> show (currResource s))
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

    rSidebar = vBox
        [ doneBox
        , queueBox
        ]

    centerContent = vBox . fmap hCenter $
        [ txt ("You are " <> aname me)
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
        [ txt $ "Ready, " <> aname nextA <> "?"
        , txt "(y/n)"
        , txt "Press . for next player"
        ]
    
    in  [
        if changingPlayers s
            then playerSwitchScreen
            else controlScreen
        ]