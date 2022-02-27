module Server (main) where

import GridTactics
import Relude
import System.Console.Haskeline
import System.Random
import Util

nonNeg :: (Num a, Ord a) => a -> Maybe String
nonNeg = check "Cannot be negative." (>= 0)

gr0 :: (Num a, Ord a) => a -> Maybe String
gr0 = check "Must be greater than zero." (> 0)

main :: IO ()
main = runInputT defaultSettings do
    outputStrLn "Enter port:"
    port <- untilValidAnd nonNeg do
        outputStrLn "Enter port:"
        getInputLineWithInitial "> " ("42069","")
    wSize <- untilValidAnd gr0 do
        outputStrLn "Enter world radius:"
        getInputLineWithInitial "> " ("10","")
    fillPortion <- untilValidAnd
        ( gr0
        &>- check "Cannot be greater than 100 percent." (<= 100)
        ) do
            outputStrLn "Enter percentage of the map to fill with scatters:"
            getInputLineWithInitial "> " ("50","")
    scatterHealth <- untilValidAnd nonNeg do
        outputStrLn "Enter scatter health:"
        getInputLineWithInitial "> " ("2","")
    scatterActions <- untilValidAnd nonNeg do
        outputStrLn "Enter scatter actions:"
        outputStrLn "(Action points that can be looted from a scatter)"
        getInputLineWithInitial "> " ("1","")
    startHealth <- untilValidAnd gr0 do
        outputStrLn "Enter starting health:"
        getInputLineWithInitial "> " ("3","")
    startMaxHealth <- untilValidAnd
        (check "Cannot be less than starting health." (>= startHealth))
        do
            outputStrLn "Enter starting maximum health:"
            getInputLineWithInitial "> " (show startHealth,"")
    startActions <- untilValidAnd gr0 do
        outputStrLn "Enter starting actions:"
        getInputLineWithInitial "> " ("1","")
    startRange <- untilValidAnd gr0 do
        outputStrLn "Enter starting range:"
        getInputLineWithInitial "> " ("2","")
    startVision <- untilValidAnd gr0 do
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
    liftIO $ runServer port (w :: SeqWorld) conf