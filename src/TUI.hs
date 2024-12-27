module TUI
    ( draw
    , AppState(..)
    , currActorID
    , nextActorID
    , selfAttr
    , friendlyAttr
    , hostileAttr
    , wallAttr
    , fogAttr
    , UIAction(..)
    , Name(..)
    , Resource(..)
    , singloot
    ) where

import           Brick.AttrMap
import           Brick.Types
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Core
import           Brick.Widgets.Table
import qualified Data.Bimap as Bap
import           Data.Bimap (Bimap)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Graphics.Vty
import           Mechanics
import           Relude
import           Servant.Client
import           Util

data AppState = AppState
    { clientEnv :: ClientEnv
    , actorIDs :: Seq UID

    -- UI State
    , currAction :: Action
    , currResource :: Resource
    , changingPlayers :: Bool
    , viewingMap :: Bool
    , keybinds :: Bimap Key UIAction

    -- Cache
    , currActor :: Actor
    , nextActor :: Actor
    , currView :: Grid
    , currMap :: Grid
    , currDone :: Bool
    , currNumDone :: Int
    , currNames :: [Text]
    }

currActorID :: AppState -> UID
currActorID = fromJust . (Seq.!? 0) . actorIDs

nextActorID :: AppState -> UID
nextActorID = fromJust . (Seq.!? 0) . rotate . actorIDs


selfAttr :: AttrName
selfAttr = "selfAttr"

friendlyAttr :: AttrName
friendlyAttr = "friendlyAttr"

hostileAttr :: AttrName
hostileAttr = "hostileAttr"

wallAttr :: AttrName
wallAttr = "wallAttr"

fogAttr :: AttrName
fogAttr = "fogAttr"


data UIAction
    = SelUndirAct UndirAction
    | SelDirAct DirAction
    | Increment
    | Decrement
    | RotL
    | RotR
    | RotL'
    | RotR'
    | PlayerL
    | PlayerR
    | Yes
    | No
    | ViewMap
    | SwitchTo UID
    | ToggleDone
    | Quit
    | SubmAction
    | DelAction
    | Also UIAction -- ^ Bind another key to this action
    deriving stock (Eq, Ord, Show)

newtype Name = Btn UIAction
    deriving stock (Eq, Ord)

undirActBtn :: UndirAction -> Name
undirActBtn = Btn . SelUndirAct

dirActBtn :: DirAction -> Name
dirActBtn = Btn . SelDirAct



grid2Table :: AppState -> [[Square]] -> Table Name
grid2Table s = table . reverse . fmap (fmap $ renderSquare s)

renderSquare :: AppState -> Square -> Widget Name
renderSquare s sq =
    ( case sq of
        Nothing -> id
        Just (Entity Nothing (Just "?") _ _) -> withAttr fogAttr
        Just (Entity Nothing _ _ _) -> if hittable sq
            then withAttr wallAttr
            else id
        Just (Entity (Just aID) _ _ _) -> if aID `elem` actorIDs s
            then if aID == currActorID s
                then withAttr selfAttr
                else clickable (Btn $ SwitchTo aID) . withAttr friendlyAttr
            else withAttr hostileAttr
    )
    . vLimit 2 . hLimit 5 . center . vBox . sq2Txts $ sq

hpInner :: Int -> Text
hpInner hp
    | hp <= 2   = " "
    | otherwise = "|"

hpOuter :: Int -> Text
hpOuter hp
    | hp <= 0   = " "
    | hp == 1   = "."
    | hp <= 3   = "-"
    | hp <= 5   = "="
    | otherwise = "|"

hp2Text :: Int -> Text
hp2Text hp = hpOuter hp <> hpInner hp
          <> (if hp > 0 then show hp else "")
          <> hpInner hp <> hpOuter hp

sq2Txts :: Square -> [Widget a]
sq2Txts Nothing = [txt " ",txt " "]
sq2Txts (Just (Entity _ mname hp cont)) =
    [ maybe (txt "") (center . txt) mname
    , txt $ hp2Text hp
    , center . txt $ loot2TextShort cont
    ]

resType2Text :: Resource -> Text
resType2Text Actions = "Juice"
resType2Text Hearts  = "Scrap"

resType2TextShort :: Resource -> Text
resType2TextShort Actions = "J"
resType2TextShort Hearts  = "S"

res2Text :: Resource -> Int -> Text
res2Text r n = show n <> " " <> resType2Text r

res2TextShort :: Resource -> Int -> Text
res2TextShort r n = show n <> resType2TextShort r

list2Text :: [Text] -> Text
list2Text = maybe "Nothing" nempty2Text . nonEmpty

list2TextShort :: [Text] -> Text
list2TextShort = maybe "-" (T.concat . intersperse " " . toList) . nonEmpty

nempty2Text :: NonEmpty Text -> Text
nempty2Text xs
    | length xs == 1 = head xs
    | length xs == 2 = maybe "" head (nonEmpty $ tail xs) <> " and " <> head xs
    | otherwise      = (T.concat . intersperse ", " $ tail xs) <> " and " <> head xs

loot2Text :: Loot -> Text
loot2Text = list2Text . toList . Map.mapWithKey res2Text . Map.filter (/= 0) . unLoot

loot2TextShort :: Loot -> Text
loot2TextShort = list2TextShort . toList . Map.mapWithKey res2TextShort . Map.filter (/= 0) . unLoot

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
act2Text actn = basicAct2Text actn

basicAct2Text :: Action -> Text
basicAct2Text (Dir (Throw _) _) = "Throw"
basicAct2Text (Dir act _) = show act
basicAct2Text (Undir HealMe) = "Heal Self"
basicAct2Text (Undir ShootMe) = "Shoot Self"
basicAct2Text (Undir UpRange) = "Upgrade Range"
basicAct2Text (Undir UpVision) = "Upgrade Vision"
basicAct2Text (Undir act) = show act

key2Text :: Key -> Text
key2Text (KChar c) = fromString [c]
key2Text (KFun n)  = fromString $ show n
key2Text KBS       = "Backspace"
key2Text KDel      = "Delete"
key2Text otherKey  = fromString $ drop 1 $ show otherKey

draw :: AppState -> [Widget Name]
draw s = let
    me = currActor s
    mySq = middle . middle $ currView s

    dispBind = key2Text . (keybinds s Bap.!>)

    doneBox = joinBorders $ border $ vBox
        [ clickable (Btn ToggleDone) $ txtWrap if currDone s
            then "Waiting for other players to finish..."
            else dispBind ToggleDone <> ": End Turn"
        , hBorder
        , txtWrap $ show (currNumDone s) <> "/" <> show (length $ currNames s)
            <> " pawns are done"
        ]

    queueBox = borderWithLabel (txt "Action Plan")
        (vBox (defaultElem
            (txtWrap "Nothing Planned (Yet!)")
        . fmap (txtWrap . act2Text) . reverse . toList . queue $ me))

    worldTable = renderTable . grid2Table s $ currView s

    dispDActCost actn = clickable (dirActBtn actn) . txt $ loot2TextShort (cost (Dir actn N))
    dispUActCost actn = clickable (undirActBtn actn) . txt $ loot2TextShort (cost (Undir actn))

    dispDActBinds acts =
        [
            [ clickable (Btn $ SelDirAct act) . txt
                $ dispBind (SelDirAct act)
                    <> ": " <> basicAct2Text (Dir act N)
            , dispDActCost act
            ]
        | act <- acts
        ]
    
    dispUActBinds acts =
        [
            [ clickable (Btn $ SelUndirAct act) . txt
                $ dispBind (SelUndirAct act)
                    <> ": " <> basicAct2Text (Undir act)
            , dispUActCost act
            ]
        | act <- acts
        ]

    actMenu = joinBorders $ border
        (renderTable . alignRight 1 . rowBorders False . columnBorders False . surroundingBorder False . table $
            [txt "_: Action", txt "Cost"]
            :  dispDActBinds universe
            ++ dispUActBinds universe
        )
    
    inventory = txt ("Your Inventory: " <> loot2Text (maybeToMonoid (contents <$> mySq)))

    selectedAct = vBox . fmap hCenter $
        [ txt ("Selected Action: " <> act2Text (currAction s))
        , txt ("Cost: " <> loot2Text (cost $ currAction s))
        , case currAction s of
            Dir (Throw _) _ ->
                txt ("Selected Resource: " <> resType2Text (currResource s))
            _ -> emptyWidget
        ]
    
    lSidebar = vBox
        [ actMenu
        , case currAction s of
            Dir _ _ -> txtWrap $ dispBind RotL <> " and " <> dispBind RotR
                <> " rotate direction"
            _ -> emptyWidget 
        , case currAction s of
            Dir (Throw _) _ -> vBox
                [ txtWrap $ dispBind Decrement <> " and " <> dispBind Increment
                    <> " adjust resource amount"
                , txtWrap $ dispBind RotL' <> " and " <> dispBind RotR'
                    <> " select resource type"
                ]
            _ -> emptyWidget
        ]

    rSidebar = vBox
        [ doneBox
        , queueBox
        , txtWrap $ dispBind SubmAction <> ": Plan selected action"
        , txtWrap $ dispBind DelAction <> ": Delete last action"
        ]
    
    mapTable = renderTable . grid2Table s $ currMap s

    centerContent = vBox . fmap hCenter $
        [ txt ("You are " <> aname me)
        , if viewingMap s
            then txt "Press M to exit map"
            else txt "Press M to view map"
        , if viewingMap s
            then mapTable
            else worldTable
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
        [ txt $ "Ready, " <> aname me <> "?"
        , txt $ "(" <> dispBind Yes <> "/" <> dispBind No <> ")"
        , txt $ "Press " <> dispBind PlayerR <> " for next pawn"
        , txt $ "Press " <> dispBind PlayerL <> " for previous pawn"
        ]
    
    in  [
        if changingPlayers s
            then playerSwitchScreen
            else controlScreen
        ]