module Server (main) where

import GridTactics
import Relude
import System.Console.Haskeline
import System.Random
import Brick.Util (clamp)

main :: IO ()
main = runInputT defaultSettings do
    outputStrLn "Hello!"
    outputStrLn $ "Welcome to the " ++ productName ++ " server."
    
    outputStrLn ""
    port <- untilValidAnd nonNeg do
        outputStrLn "Enter port:"
        getInputLineWithInitial "> " ("42069","")

    outputStrLn ""
    wSize <- untilValidAnd gr0 do
        outputStrLn "Enter world width:"
        getInputLineWithInitial "> " ("10","")

    outputStrLn ""
    fillPortion <- untilValidAnd
        ( nonNeg
        &>- check "Cannot be greater than 100 percent." (<= 100)
        ) do
            outputStrLn "Enter percentage of the map to fill with scatters:"
            getInputLineWithInitial "> " ("50","")

    outputStrLn ""
    scatterHealth <- untilValidAnd nonNeg do
        outputStrLn "Enter scatter health:"
        getInputLineWithInitial "> " ("2","")

    outputStrLn ""
    scatterActions <- untilValidAnd nonNeg do
        outputStrLn "Enter scatter actions:"
        outputStrLn "(Action points that can be looted from a scatter)"
        getInputLineWithInitial "> " ("1","")

    outputStrLn ""
    startHealth <- untilValidAnd gr0 do
        outputStrLn "Enter starting health:"
        getInputLineWithInitial "> " ("3","")

    outputStrLn ""
    startMaxHealth <- untilValidAnd
        (check "Cannot be less than starting health." (>= startHealth))
        do
            outputStrLn "Enter starting maximum health:"
            getInputLineWithInitial "> " (show startHealth,"")

    outputStrLn ""
    startActions <- untilValidAnd gr0 do
        outputStrLn "Enter starting actions:"
        getInputLineWithInitial "> " ("1","")

    outputStrLn ""
    startRange <- untilValidAnd gr0 do
        outputStrLn "Enter starting range:"
        getInputLineWithInitial "> " ("2","")

    outputStrLn ""
    startVision <- untilValidAnd
        ( gr0 
        &>- check ("Must be smaller than " ++ show (wSize `div` 2) ++ " (half the world width).")
            (< wSize `div` 2)
        ) do
            outputStrLn "Enter starting vision distance:"
            getInputLineWithInitial "> " (show $ clamp 0 3 $ wSize `div` 2,"")

    gen <- getStdGen

    let scatterE = (Entity
            { actorID = Nothing
            , ename = Nothing
            , health = scatterHealth
            , contents = singloot Actions scatterActions
            })

    let w = fillFraction (fillPortion / 100 :: Double) scatterE
            $ mkWorld gen wSize

    let conf = Config 
            { pawnTemplate = Entity
                { actorID = Nothing
                , ename = Nothing
                , health = startHealth
                , contents = singloot Actions startActions
                          <> singloot Hearts (startMaxHealth - startHealth)
                }
            , actorTemplate = Actor
                { aname = "TEMPLATE"
                , coords = (0,0)
                , range = startRange
                , vision = startVision
                , queue = fromList []
                , done = False
                }
            }

    outputStrLn "\nStarting Server..."
    liftIO $ runServer port (w :: SeqWorld) conf
    outputStrLn "\nGoodbye!"