module SeqWorld
    ( SeqWorld
    ) where


import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Tuple.Extra (both)
import           Mechanics
import           Relude
import           Relude.Unsafe (fromJust)
import           System.Random
import           System.Random.Shuffle

data SeqWorld = SeqWorld
  { gen' :: StdGen
  , width :: Int
  , worldMap :: Seq (Seq Square)
  , empties' :: Set Coords
  , actors' :: IntMap Actor
  , turnOrder' :: [UID]
  , nextUID :: Int
  }

wrap :: Int -> Coords -> Coords
wrap n = both (`mod` n)

instance World SeqWorld where
  mkWorld gen sz = SeqWorld
    { gen' = gen
    , width = sz
    , worldMap = fromList . replicate sz . fromList . replicate sz $ Nothing
    , empties' = fromList [(x,y) | x <- [0..sz - 1], y <- [0..sz - 1]]
    , actors' = IM.empty
    , turnOrder' = []
    , nextUID = 0
    }

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
    modify \w -> w {turnOrder' = shuffle' everyone (length everyone) gen}

  wrapCoords w = wrap (width w)
  
  -- index is justified because wrap forces the Coords within the index range of worldMap
  getSquare c w = flip Seq.index x . flip Seq.index y $ worldMap w
    where (x, y) = wrapCoords w c

  -- fromJust is justified because other functions should only be able to get valid UIDs from a World.
  -- As long as they aren't constructing their own UIDs
  -- or taking them from one World and using them in another, everything should be just fine. JUST FINE
  lookupActor aID = fromJust . IM.lookup (unwrapUID aID) . actors'

  updateActor f aID w = w {actors' = IM.adjust f (unwrapUID aID) . actors' $ w}

  _addActor a = do
    aID <- gets nextUID
    modify \w -> w {nextUID = aID + 1}
    modify \w -> w {actors' = IM.insert aID a $ actors' w}
    modify shuffleTurnOrder
    return $ UID aID

  _unaddActor (UID aID) = execState do
    modify \w -> w {actors' = IM.delete aID $ actors' w}
    modify shuffleTurnOrder

  updateSquare f c w = (\w' -> w' {empties' = (
    if passable $ getSquare c w'
      then Set.insert
      else Set.delete
    ) (wrap (width w) c) $ empties' w'
    } )
    $ let (x, y) = wrap (width w) c
      in w {worldMap = Seq.adjust (Seq.adjust f x) y $ worldMap w}
  
  maxRange = const maxBound
  maxVision = (`div` 2) . subtract 1 . width