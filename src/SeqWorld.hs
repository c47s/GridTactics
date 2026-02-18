{-# LANGUAGE DeriveAnyClass #-}

module SeqWorld
    ( SeqWorld
    ) where


import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq(..))
import qualified Data.Set as Set
import           Data.Tuple.Extra (both)
import           Mechanics
import           Relude
import           Relude.Unsafe (fromJust)
import           System.Random
import           Util
import           WebInstances ()

data SeqWorld = SeqWorld
  { gen' :: StdGen
  , radius :: Int
  , worldMap :: Seq (Seq Square)
  , empties' :: Set Coords
  , actors' :: IntMap Actor
  , turnOrder' :: [UID]
  , nextUID :: Int
  , snapshots' :: Seq (Seq Snapshot)
  } deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

width :: SeqWorld -> Int
width w = 2 * radius w + 1

absoluteCoords :: SeqWorld -> Bool
absoluteCoords _w = False -- width w <= 11

wrap :: Int -> Coords -> Coords
wrap n = both (`mod` n)

_wrapCoords :: SeqWorld -> Coords -> Coords
_wrapCoords = wrap . width

instance World SeqWorld where
  mkWorld gen r = SeqWorld
    { gen' = gen
    , radius = r
    , worldMap = fromList . replicate sz . fromList . replicate sz $ Nothing
    , empties' = fromList [(x,y) | x <- [0..sz - 1], y <- [0..sz - 1]]
    , actors' = IM.empty
    , turnOrder' = []
    , nextUID = 0
    , snapshots' = mempty
    }
    where sz = 2 * r + 1
  
  origin w = if absoluteCoords w
    then Just (0, 0)
    else Nothing
  extent w = if absoluteCoords w
    then Just (width w - 1, width w - 1)
    else Nothing


  snapshots = snapshots'

  takeSnapshot w = w { snapshots' = snapshots' w &
      \case
        (snss :|> sns) -> snss :|> (sns :|> snapshot)
        Empty -> fromList [fromList [snapshot]]
    }
    where
      snapshot = Snapshot
        { actorStates = Map.fromList [(aID, lookupActor aID w) | aID <- actors w]
        , actorCoords = Map.fromList [(aID, findActor aID w) | aID <- actors w]
        , actorOrder = fromList . reverse $ turnOrder w
        , gridState = multiView [(radius w, (radius w + 1, radius w + 1))] w
        }
  
  newSnapTurn w = w { snapshots' = snapshots' w :|> mempty }


  splitGen = do
    (myNewGen,outGen) <- gets (split . gen')
    modify \w -> w {gen' = myNewGen}
    return outGen
  
  empties = toList . empties'
  
  actors = fmap UID . IM.keys . actors'

  turnOrder = turnOrder'

  shuffleTurnOrder = execState do
    gen <- splitGen
    everyone <- gets actors
    modify \w -> w {turnOrder' = safeShuf' everyone (length everyone) gen}

  wrapAround (cx, cy) w (x, y) = bimap (+ offset cx) (+ offset cy) $
    _wrapCoords w (x - offset cx, y - offset cy)
      where offset cz = cz - radius w

  wrapCoords = _wrapCoords

  -- Applies circular mean to x and y coords.
  -- Seems like a good approximation of centroid on a toroidally wrapping map,
  -- but might introduce minor inaccuracies, maybe near the edges?
  -- Because they say on StackOverflow that minimizing distance to a set of points
  -- is not easy on a toroid!
  -- Time will tell. Maybe.
  avgPos w = both (round . circMean (fromIntegral $ width w :: Double) . fmap fromIntegral) . unzip
    where circMean r = (* (r/(2*pi))) . uncurry atan2 . sumPairs . fmap ((sin &&& cos) . (* ((2*pi)/r)))
  
  -- index is justified because wrap forces the Coords within the index range of worldMap
  getSquare c w = flip Seq.index x . flip Seq.index y $ worldMap w
    where (x, y) = _wrapCoords w c

  -- fromJust is justified because other functions should only be able to get valid UIDs from a World.
  -- As long as they aren't constructing their own UIDs
  -- or taking them from one World and using them in another, everything should be just fine. JUST FINE
  lookupActor aID = fromJust . IM.lookup (unwrapUID aID) . actors'

  updateActor f aID w = w {actors' = IM.adjust (wrapActor . f) (unwrapUID aID) . actors' $ w}
    where wrapActor a = a {coords = wrapCoords w $ coords a}

  _addActor a = do
    aID <- gets nextUID
    modify \w -> w {nextUID = aID + 1}
    let a' = a {ownID = UID aID}
    modify \w -> w {actors' = IM.insert aID a' $ actors' w}
    modify shuffleTurnOrder
    return $ UID aID

  _unaddActor (UID aID) = execState do
    modify \w -> w {actors' = IM.delete aID $ actors' w}
    modify shuffleTurnOrder

  updateSquare f c w = (\w' -> w' {empties' = (
    if passable $ getSquare c w'
      then Set.insert
      else Set.delete
    ) (_wrapCoords w c) $ empties' w'
    } )
    $ let (x, y) = _wrapCoords w c
      in w {worldMap = Seq.adjust (Seq.adjust f x) y $ worldMap w}
  
  maxRange = const maxBound
  maxVision = (`div` 2) . subtract 1 . width
