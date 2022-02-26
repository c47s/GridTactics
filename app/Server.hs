module Server where

import API
import Mechanics
import Relude
import SeqWorld
import System.Console.Haskeline
import System.Random

untilValidAnd :: (Read a, MonadIO m) => (a -> Either String a) -> InputT m (Maybe String) -> InputT m a
untilValidAnd check getInput = do
    ln <- getInput
    case readMaybe =<< ln of
        Nothing -> do
            outputStrLn "Invalid input."
            untilValid getInput
        Just a -> case check a of
            Left msg -> do
                outputStrLn msg
                untilValid getInput
            Right a' -> return a

untilValid :: (Read a, MonadIO m) => InputT m (Maybe String) -> InputT m a
untilValid = untilValidAnd Right

main :: IO ()
main = do
    port <- runInputT defaultSettings $ untilValid do
        outputStrLn "Enter port:"
        getInputLineWithInitial "> " ("42069","")
    wSize <- runInputT defaultSettings $ untilValid do
        outputStrLn "Enter world radius:"
        getInputLineWithInitial "> " ("10","")
    startHealth <- runInputT defaultSettings $ untilValid do
        outputStrLn "Enter starting health:"
        getInputLineWithInitial "> " ("3","")
    startMaxHealth <- runInputT defaultSettings $ untilValid do
        outputStrLn "Enter starting maximum health:"
        getInputLineWithInitial "> " (show startHealth,"")
    startActions <- runInputT defaultSettings $ untilValid do
        outputStrLn "Enter starting actions:"
        getInputLineWithInitial "> " ("1","")
    startRange <- runInputT defaultSettings $ untilValid do
        outputStrLn "Enter starting range:"
        getInputLineWithInitial "> " ("2","")
    startVision <- runInputT defaultSettings $ untilValid do
        outputStrLn "Enter starting vision distance:"
        getInputLineWithInitial "> " ("3","")
    gen <- getStdGen
    let w = mkWorld gen wSize :: SeqWorld
    runServer port w (Config 
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
        })