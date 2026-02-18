module Server (main) where

import Brick.Util (clamp)
import Data.Aeson
import Data.List.Extra (upper)
import Data.Time.Format
import Data.Time.LocalTime
import GridTactics
import Relude
import System.Console.Haskeline
import System.Directory (doesFileExist)
import System.Random

buildWorld :: (World w) => InputT IO w
buildWorld = do
    outputStrLn ""
    wSize <- untilValidAnd gr0 do
        outputStrLn "Enter world radius:"
        getInputLineWithInitial "> " ("5","")

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
        getInputLineWithInitial "> " ("1","")

    outputStrLn ""
    scatterActions <- untilValid do
        outputStrLn "Enter scatter juice:"
        outputStrLn "(Juice placed inside scatters)"
        getInputLineWithInitial "> " ("0","")

    gen <- getStdGen

    let scatterE = (Entity
            { actorID = Nothing
            , ename = Nothing
            , health = scatterHealth
            , contents = singloot Actions scatterActions
                      <> singloot Hearts scatterHearts
            , sealed = False
            })

    let w = fillFraction (fillPortion / 100 :: Double) scatterE
            $ mkWorld gen wSize
    return w



data YesNo = Y | N
    deriving stock (Eq, Read)

main :: IO ()
main = runInputT defaultSettings do
    outputStrLn "Hello!"
    outputStrLn $ "Welcome to the " ++ productName ++ " server."
    
    outputStrLn ""
    port <- untilValidAnd nonNeg do
        outputStrLn "Enter port:"
        getInputLineWithInitial "> " ("42069","")
    
    outputStrLn ""
    loadBak <- untilValid do
        outputStrLn "Attempt to load autosaved game?"
        upper <<$>> getInputLineWithInitial "> " ("n","")
    
    bakExists <- liftIO $ doesFileExist ".gridtac_bak.json"
    when (loadBak == Y) $
        outputStrLn if bakExists then "Found autosave." else "No autosave found."

    bakW <- if loadBak /= Y || not bakExists
        then return Nothing
        else liftIO $ decodeFileStrict ".gridtac_bak.json"
    when (isJust bakW) $ outputStrLn "Loaded autosave."

    newW <- buildWorld

    let w = fromMaybe newW bakW


    
    outputStrLn ""
    pawnsPerClient' <- untilValidAnd gr0 do
        outputStrLn "Enter number of pawns per client:"
        getInputLineWithInitial "> " ("3", "")

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


        
    outputStrLn ""
    runDaily <- untilValid do
        outputStrLn "Evaluate just one round per day?"
        upper <<$>> getInputLineWithInitial "> " ("n", "")
    
    runDailyAt' <- if runDaily /= Y
        then return Nothing
        else Just <$> do
            outputStrLn ""
            untilJust do
                outputStrLn "Enter time of day to evaluate round:"
                line <- getInputLineWithInitial "> " ("5:00", "")

                let parseTime = parseTimeM True defaultTimeLocale "%-H:%0M"
                tz <- liftIO getCurrentTimeZone
                return $ timeOfDayToTime . snd . localToUTCTimeOfDay tz <$>
                         (parseTime =<< line)

    let conf = Config 
            { pawnTemplate = Entity
                { actorID = Nothing
                , ename = Nothing
                , health = startHealth
                , contents = singloot Actions startActions
                          <> singloot Hearts startHearts
                , sealed = False
                }
            , actorTemplate = Actor
                { aname = "TEMPLATE"
                , owner = "NOBODY"
                , ownID = UID (-1)
                , coords = (0,0)
                , range = startRange
                , baseVision = startVision
                , queue = fromList []
                , done = False
                }
            , pawnsPerClient = pawnsPerClient'
            , runDailyAt = runDailyAt'
            }

    outputStrLn "\nStarting Server..."
    liftIO $ runServer port (w :: SeqWorld) conf
    outputStrLn "\nGoodbye!"
