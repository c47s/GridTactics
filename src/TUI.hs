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
import           Data.List (elemIndex)
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
    , viewingReplay :: Bool
    , replayIndex :: Int
    , replayTick :: Int
    , keybinds :: Bimap Key UIAction

    -- Cache
    , currActor :: Actor
    , nextActor :: Actor
    , currView :: Grid
    , currMap :: Grid
    , currReplay :: Seq Grid
    , currDone :: Bool
    , currNumDone :: Int
    , currNames :: [Text]
    , currOrder :: [UID]
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
    | ViewReplay
    | Quit
    | ClaimAllPawns
    | RemovePawn
    | DisownPawn
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
            Just (Entity (Just aID) _ _ _) ->
                if aID `elem` actorIDs s
                    then if aID == currActorID s
                        then withAttr selfAttr
                        else clickable (Btn $ SwitchTo aID)
                            . withAttr friendlyAttr
                    else withAttr hostileAttr
        )
        . vLimit 2 . hLimit 5 . center . vBox . sq2Txts $ sq
    where
        sq2Txts :: Square -> [Widget Name]
        sq2Txts Nothing = [txt " ",txt " "]
        sq2Txts (Just (Entity mID mname hp cont)) =
            [ if hp <= 0 && isJust ((`elemIndex` currOrder s) =<< mID)
                then txt ""
                else maybe (txt "") txt mname
            , hBox [showOrder mID, hpTxt]
            , loot2TxtShort cont
            ]
            where
                showOrder :: Maybe UID -> Widget Name
                showOrder Nothing = txt ""
                showOrder (Just aID) = if viewingReplay s then txt "#?"
                                       else maybe (txt "?!")
                                            (txt . ("#" <>) . show . (+ 1)) $
                                            elemIndex aID (currOrder s)
                
                hpTxt :: Widget Name
                hpTxt = if hp > 0 || isJust ((`elemIndex` currOrder s) =<< mID)
                    then padLeft Max . txt $ "♥︎" <> show hp
                    else txt " "

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

list2TxtShort :: [Text] -> Widget Name
list2TxtShort [] = txt ""
list2TxtShort (t:ts) = hBox $ txt t : (padLeft Max . txt <$> ts)

nempty2Text :: NonEmpty Text -> Text
nempty2Text xs
    | length xs == 1 = head xs
    | length xs == 2 = maybe "" head (nonEmpty $ tail xs) <> " and " <> head xs
    | otherwise      = (T.concat . intersperse ", " $ tail xs) <> " and " <> head xs

loot2Text :: Loot -> Text
loot2Text = list2Text . toList . Map.mapWithKey res2Text . Map.filter (/= 0) . unLoot

loot2TextShort :: Loot -> Text
loot2TextShort = list2TextShort . toList . Map.mapWithKey res2TextShort . Map.filter (/= 0) . unLoot

loot2TxtShort :: Loot -> Widget Name
loot2TxtShort = list2TxtShort . toList . Map.mapWithKey res2TextShort . Map.filter (/= 0) . unLoot

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
act2Text (Dir (Build n) dir) = dir2Text dir <> " Build ♥︎" <> show n
act2Text (Dir act dir) = dir2Text dir <> " " <> show act <> " "
act2Text actn = basicAct2Text actn

basicAct2Text :: Action -> Text
basicAct2Text (Dir (Throw _) _) = "Throw"
basicAct2Text (Dir (Build _) _) = "Build"
basicAct2Text (Dir act _) = show act
basicAct2Text (Undir RepairMe)  = "Repair Self"
basicAct2Text (Undir ShootMe) = "Scrap Self"
basicAct2Text (Undir Recycle) = "Eat Scrap"
basicAct2Text (Undir UpRange) = "+Range"
basicAct2Text (Undir UpVision) = "+Vision"
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

    dispDActCost actn = clickable (dirActBtn actn) . txt $ loot2TextShort (cost (Dir actn N))
    dispUActCost actn = clickable (undirActBtn actn) . txt $
            loot2TextShort (cost $ Undir actn)
            <> if actn == ShootMe then " ♥︎1" else ""

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
    
    inventory = txt ("Inventory: " <> loot2Text (maybeToMonoid (contents <$> mySq)))

    selectedAct = vBox . fmap hCenter $
        [ txt ("Selected Action: " <> act2Text (currAction s))
        , txt ("Cost: " <> loot2Text (cost $ currAction s))
        , if isRanged $ currAction s
            then txt ("Range: " <> show (range me))
            else emptyWidget
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
            Dir (Build _) _ -> txtWrap $ dispBind Decrement <> " and " <> dispBind Increment
                <> " adjust build health"
            _ -> emptyWidget
        ]

    rSidebar = vBox
        [ doneBox
        , if viewingReplay s
            then txt "V: Exit replay"
            else if not . null $ currReplay s
                then txt "V: View replay"
                else txt " "
        , queueBox
        , txtWrap $ dispBind SubmAction <> ": Plan selected action"
        , txtWrap $ dispBind DelAction <> ": Delete last action"
        ]
    
    worldTable = hCenter (txt "Press M to view map")
             <=> hCenter (renderTable . grid2Table s $ currView s)

    mapTable = hCenter (txt "Press M to exit map")
           <=> hCenter (renderTable . grid2Table s $ currMap s)

    replayTable = fromMaybe (hCenter $ vCenter $ txt ";3") $ do
        grid <- Seq.lookup (max 0 $ replayIndex s) $ currReplay s
        return $ renderTable $ grid2Table s grid

    centerContent = vBox . fmap hCenter $
        if viewingReplay s
            then [ txt "Round Replay"
                 , replayTable
                 ]
            else [ txt ("You are " <> aname me)
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