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
import           Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import           Mechanics
import           Relude
import           Servant.Client
import           Util

data Resource = Actions | Hearts
    deriving stock (Eq, Enum, Bounded, Show)

singloot :: Resource -> Loot
singloot Actions = Loot {hearts = 0, actions = 1}
singloot Hearts = Loot {hearts = 1, actions = 0}



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
    [fromMaybe "" mname, show hp <> "/" <> show (hp + hearts (maybeToMonoid cont))]



draw :: AppState -> [Widget Name]
draw s = let
    me = currActor s
    mySq = middle . middle $ currView s
    nextA = nextActor s

    doneBox = border $ vBox
        [ clickable (DoneBtn $ currActorID s) $ txtWrap $ "You are " <> (if currDone s
            then "done"
            else "not done"
            ) <> " planning\n(Click to toggle)"
        , hBorder
        , txtWrap $ show (currNumDone s) <> "/" <> show (length $ currNames s)
            <> " players are done"
        ]

    queueBox = borderWithLabel (txt "Action Plan")
        (vBox (defaultElem (txtWrap "Nothing Planned (Yet!)")
        . fmap (strWrap . show) . reverse . toList . queue $ me))

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
            , [clickable (UndirActBtn HealMe) . txt $ "H: Heal getActor", dispUActCost HealMe]
            , [clickable (UndirActBtn ShootMe) . txt $ "S: Shoot getActor", dispUActCost ShootMe]
            , [clickable (UndirActBtn Wait) . txt $ "w: Wait", dispUActCost Wait]
            ])
    
    inventory = str ("Your Inventory: " ++ show (contents =<< mySq))

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