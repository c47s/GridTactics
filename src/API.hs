-- {-# LANGUAGE OverloadedStrings #-}

module API
   ( API
   , runServer
   ) where

import Mechanics
import Network.Wai.Handler.Warp
import Relude
import Servant
import WebInstances

type Modify ts a = Get ts a
              :<|> ReqBody ts a :> PostNoContent

type ActorAPI = Get '[JSON] Actor
           :<|> "view" :> Get '[JSON] Grid -- View the World
           :<|> "act"  :> ReqBody '[JSON] Action :> Post '[JSON] Grid
           :<|> "done" :> Modify '[JSON] Bool

type ActorsAPI = "names" :> Get '[JSON] [Text]
            :<|> "new"   :> ReqBody '[JSON] Text :> Post '[JSON] UID -- Provide a name
            :<|> "done"  :> Get '[JSON] Int -- Number marked as done

type API = "actor"  :> Capture "aID" UID :> ActorAPI
      :<|> "actors" :> ActorsAPI

runServer :: (World w) => w -> IO ()
runServer w = do
   wRef <- newIORef w
   
   let hView = undefined
   let hAct = undefined
   let hDone = undefined

   let hNames = undefined
   let hNew = undefined
   let hDoneNum = undefined

   let hActor aID = hView aID
               :<|> hAct aID
               :<|> hDone aID

   let hActors = hNames
            :<|> hNew
            :<|> hDoneNum

   let hAPI = hActor
         :<|> hActors

   run 8081 (serve (Proxy :: Proxy API) hAPI)