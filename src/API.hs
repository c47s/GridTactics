-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE OverloadedStrings #-}

module API
   ( API
   ) where

import Mechanics
import Relude
import Servant.API
-- import Web.HttpApiData

-- Wrap types in Res to have them automatically enc/decoded to HttpApiData
-- using their Show, Bounded, and Enum instances.
-- newtype Res a = Res { unRes :: a }

-- instance (Show a) => ToHttpApiData (Res a) where
--    toUrlPiece = showTextData . unRes
-- instance (Show a, Bounded a, Enum a) => FromHttpApiData (Res a) where
--    parseUrlPiece = fmap Res . parseBoundedTextData

type Modify ts a = Get ts a
              :<|> ReqBody ts a :> Put ts ()

type ActorAPI = Get '[JSON] Actor
           :<|> "view" :> Get '[JSON] Grid
           :<|> "act"  :> ReqBody '[JSON] Action :> Post '[JSON] Grid
           :<|> "done" :> Modify '[JSON] Bool

type ActorsAPI = "names" :> Get '[JSON] [Text]
            :<|> "new"   :> ReqBody '[JSON] Text :> Post '[JSON] UID

type API = "actor"  :> Capture "aID" UID :> ActorAPI
      :<|> "actors" :> ActorsAPI