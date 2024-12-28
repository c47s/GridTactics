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
        getInputLineWithInitial "> " ("11","")
    
    outputStrLn ""
    pawnsPerClient' <- untilValidAnd gr0 do
        outputStrLn "Enter number of pawns per client:"
        getInputLineWithInitial "> " ("3", "")

    outputStrLn ""
    fillPortion <- untilValidAnd
        ( nonNeg
        &>- check "Cannot be greater than 100 percent." (<= 100)
        ) do
            outputStrLn "Enter percentage of the map to fill with scatters:"
            getInputLineWithInitial "> " ("30","")

    outputStrLn ""
    scatterHealth <- untilValidAnd nonNeg do
        outputStrLn "Enter scatter Health:"
        getInputLineWithInitial "> " ("2","")

    outputStrLn ""
    scatterHearts <- untilValid do
        outputStrLn "Enter scatter scrap:"
        outputStrLn "(Extra scrap placed inside scatters)"
        getInputLineWithInitial "> " ("2","")

    outputStrLn ""
    scatterActions <- untilValid do
        outputStrLn "Enter scatter juice:"
        outputStrLn "(Juice placed inside scatters)"
        getInputLineWithInitial "> " ("0","")

    outputStrLn ""
    startHealth <- untilValidAnd gr0 do
        outputStrLn "Enter starting Health:"
        getInputLineWithInitial "> " ("2","")

    outputStrLn ""
    startHearts <- untilValidAnd nonNeg do
        outputStrLn "Enter starting scrap:"
        getInputLineWithInitial "> " ("2","")

    outputStrLn ""
    startActions <- untilValidAnd nonNeg do
        outputStrLn "Enter starting juice:"
        getInputLineWithInitial "> " ("1","")

    gen <- getStdGen

    let scatterE = (Entity
            { actorID = Nothing
            , ename = Nothing
            , health = scatterHealth
            , contents = singloot Actions scatterActions
                      <> singloot Hearts scatterHearts
            })

    let w = fillFraction (fillPortion / 100 :: Double) scatterE
            $ mkWorld gen wSize

    let constrange = check ("Must be at most " ++ show (maxRange w))
            (<= maxRange w)
    
    let constrvis = check ("Must be at most " ++ show (maxVision w))
            (<= maxVision w)

    outputStrLn ""
    startRange <- untilValidAnd (nonNeg &>- constrange) do
        outputStrLn "Enter starting range:"
        getInputLineWithInitial "> " (show $ clamp 0 3 $ min (2 * maxVision w) (maxRange w),"")

    outputStrLn ""
    startVision <- untilValidAnd (nonNeg &>- constrvis) do
        outputStrLn "Enter starting vision distance:"
        getInputLineWithInitial "> " (show $ clamp 0 2 $ maxVision w,"")

    let conf = Config 
            { pawnTemplate = Entity
                { actorID = Nothing
                , ename = Nothing
                , health = startHealth
                , contents = singloot Actions startActions
                          <> singloot Hearts startHearts
                }
            , actorTemplate = Actor
                { aname = "TEMPLATE"
                , coords = (0,0)
                , range = startRange
                , baseVision = startVision
                , queue = fromList []
                , done = False
                }
            , pawnsPerClient = pawnsPerClient'
            }

    outputStrLn "\nStarting Server..."
    liftIO $ runServer port (w :: SeqWorld) conf
    outputStrLn "\nGoodbye!"