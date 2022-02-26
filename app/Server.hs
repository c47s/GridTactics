module Server where

import Data.Maybe.HT
import GHC.Data.Maybe
import GridTactics
import Relude
import System.Console.Haskeline
import System.Random

untilValidAnd :: (Read a, MonadIO m) => (a -> Maybe String) -> InputT m (Maybe String) -> InputT m a
untilValidAnd chk getInput = do
    ln <- getInput
    case readMaybe =<< ln of
        Nothing -> do
            outputStrLn "Invalid input."
            untilValid getInput
        Just a -> case chk a of
            Just msg -> do
                outputStrLn msg
                untilValidAnd chk getInput
            Nothing -> return a

check :: msg -> (a -> Bool) -> (a -> Maybe msg)
check msg p a = toMaybe (not $ p a) msg

(>-) :: (a -> Maybe msg) -> (a -> Maybe msg) -> (a -> Maybe msg)
(>-) = liftA2 firstJust

untilValid :: (Read a, MonadIO m) => InputT m (Maybe String) -> InputT m a
untilValid = untilValidAnd (const Nothing)

nonNeg :: (Num a, Ord a) => a -> Maybe String
nonNeg = check "Cannot be negative." (>= 0)

gr0 :: (Num a, Ord a) => a -> Maybe String
gr0 = check "Must be greater than zero." (> 0)

main :: IO ()
main = do
    port <- runInputT defaultSettings $ untilValidAnd nonNeg do
        outputStrLn "Enter port:"
        getInputLineWithInitial "> " ("42069","")
    wSize <- runInputT defaultSettings $ untilValidAnd gr0 do
        outputStrLn "Enter world radius:"
        getInputLineWithInitial "> " ("10","")
    fillPortion <- runInputT defaultSettings $ untilValidAnd
        ( gr0
        >- check "Cannot be greater than 100 percent." (<= 100)
        ) do
            outputStrLn "Enter percentage of the map to fill with scatters:"
            getInputLineWithInitial "> " ("50","")
    scatterHealth <- runInputT defaultSettings $ untilValidAnd gr0 do
        outputStrLn "Enter scatter health:"
        getInputLineWithInitial "> " ("2","")
    scatterActions <- runInputT defaultSettings $ untilValidAnd gr0 do
        outputStrLn "Enter scatter actions:"
        getInputLineWithInitial "> " ("1","")
    startHealth <- runInputT defaultSettings $ untilValidAnd gr0 do
        outputStrLn "Enter starting health:"
        getInputLineWithInitial "> " ("3","")
    startMaxHealth <- runInputT defaultSettings $ untilValidAnd
        (check "Cannot be less than starting health." (>= startHealth))
        do
            outputStrLn "Enter starting maximum health:"
            getInputLineWithInitial "> " (show startHealth,"")
    startActions <- runInputT defaultSettings $ untilValidAnd gr0 do
        outputStrLn "Enter starting actions:"
        getInputLineWithInitial "> " ("1","")
    startRange <- runInputT defaultSettings $ untilValidAnd gr0 do
        outputStrLn "Enter starting range:"
        getInputLineWithInitial "> " ("2","")
    startVision <- runInputT defaultSettings $ untilValidAnd gr0 do
        outputStrLn "Enter starting vision distance:"
        getInputLineWithInitial "> " ("3","")
    gen <- getStdGen
    let scatterE = (Entity
            { actorID = Nothing
            , health = scatterHealth
            , contents = Just Loot
                { actions = scatterActions
                , hearts = 0
                }
            })
    let w = fillFraction (fillPortion / 100 :: Double) scatterE
            $ mkWorld gen wSize
    let conf = Config 
            { pawnTemplate = Entity
                { actorID = Nothing
                , health = startHealth
                , contents = Just Loot
                    { actions = startActions
                    , hearts = startMaxHealth - startMaxHealth
                    }
                }
            , actorTemplate = Actor
                { name = "TEMPLATE"
                , coords = (0,0)
                , range = startRange
                , vision = startVision
                , queue = fromList []
                , done = False
                }
            }
    runServer port (w :: SeqWorld) conf