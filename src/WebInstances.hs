{-# OPTIONS_GHC -Wno-orphans #-}
-- It's a VERITABLE ORPHANAGE in here!

module WebInstances () where

import Data.Aeson (FromJSONKey, ToJSON, FromJSON, ToJSONKey)
import Deque.Lazy (Deque)
import Mechanics
import Relude
import Web.HttpApiData


-- Automatically infer to/fromJSON instances for Deques of AutoDequeJSON instances
class AutoDequeJSON a where
instance (AutoDequeJSON a, ToJSON a) => ToJSON (Deque a)
instance (AutoDequeJSON a, FromJSON a) => FromJSON (Deque a)

instance ToJSON Entity
instance FromJSON Entity
instance AutoDequeJSON Entity

instance ToJSON Resource
instance ToJSONKey Resource
instance FromJSONKey Resource
instance FromJSON Resource
instance AutoDequeJSON Resource

instance ToJSON Loot
instance FromJSON Loot
instance AutoDequeJSON Loot

instance ToJSON UID
instance FromJSON UID
instance ToJSONKey UID
instance FromJSONKey UID
instance AutoDequeJSON UID
instance ToHttpApiData UID where
   toUrlPiece = showTextData . unwrapUID
instance FromHttpApiData UID where
   parseUrlPiece = fmap UID . readTextData

instance ToJSON Actor
instance FromJSON Actor
instance AutoDequeJSON Actor

instance ToJSON Direction
instance FromJSON Direction
instance AutoDequeJSON Direction

instance ToJSON Action
instance FromJSON Action
instance AutoDequeJSON Action

instance ToJSON DirAction
instance FromJSON DirAction
instance AutoDequeJSON DirAction

instance ToJSON UndirAction
instance FromJSON UndirAction
instance AutoDequeJSON UndirAction