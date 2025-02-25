module Mechanics
    ( Entity (..)
    , Square
    , Grid
    , passable
    , dead
    , hittable
    , grabbable

    , Resource (..)
    , res
    , Loot (..)
    , singloot
    , contains
    , without

    , UID (..)
    , Actor (..)
    , vision
    , pushAct

    , Coords
    , Direction (..)
    , step

    , Action (..)
    , DirAction (..)
    , UndirAction (..)
    , cost
    , getDir
    , isRanged

    , Snapshot (..)
    , World (..)
    ) where

import           Brick.Util (clamp)
import           Control.Monad.Morph
import           Control.Zipper
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Composition
import           Data.List.HT (sieve)
import           Data.Map.Merge.Strict
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq(..), lookup)
import qualified Data.Set as Set
import           Data.Tuple.Extra (both)
import qualified Deque.Lazy as D
import           Deque.Lazy (Deque)
import           GHC.Utils.Misc (nTimes)
import           Prelude (maximum, minimum, until)
import           Relude
import           Relude.Unsafe ((!!))
import           System.Random
import           System.Random.Shuffle
import           Util



{- {- {- ENTITY -} -} -}

data Entity = Entity
  { actorID :: Maybe UID
  , ename :: Maybe Text
  , health :: Int
  , contents :: Loot
  } deriving stock (Eq, Show, Generic)

type Square = Maybe Entity

type Grid = [[Square]]

passable :: Square -> Bool
passable Nothing = True -- An empty square is passable.
passable (Just e) = (not . isJust $ actorID e) && (health e <= 0)

dead :: Square -> Bool
dead = maybe True $ (<= 0) . health

-- Does it stop bullets and thrown loot?
hittable :: Square -> Bool
hittable = not . dead

grabbable :: Square -> Bool
grabbable = maybe False $ (/= mempty) . contents

instance Semigroup Entity where
  e <> e' = Entity
    { actorID = asum [actorID e, actorID e'] -- Bad bad! When merging 2 Entities with Actors, only one Actor remains. This is why Actor Entities must be impassable.
    , ename = asum [
      do
        n <- ename e
        n' <- ename e'
        return $ mixText -- Merging 2 named Entities? This'll be fun!
          (mkStdGen . sumText -- Make a stdGen based on details of these Entities
            $ show e <> show e')
          n n'
      , ename e
      , ename e'
      ]
    , health = health e + health e'
    , contents = contents e <> contents e'
    }

instance Monoid Entity where
  mempty = Entity Nothing Nothing 0 mempty



{- {- {- LOOT -} -} -}

data Resource = Actions | Hearts
    deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)

res :: Resource -> Loot -> Int
res r = Map.findWithDefault 0 r . unLoot

newtype Loot = Loot {unLoot :: Map Resource Int}
    deriving newtype (Eq, Ord, Show)
    deriving stock Generic

{- HLINT ignore "Use one" -} -- Spurious hint.
singloot :: Resource -> Int -> Loot
singloot = Loot .: Map.singleton 

contains :: Loot -> Loot -> Bool
contains = isJust .: without

-- Handle removing a resource type that isn't in the Loot.
-- Outer Maybe controls whether we scrap the whole Loot,
-- inner Maybe controls whether we insert the value into the Loot.
removeMissingResource :: Int -> Maybe (Maybe Int)
removeMissingResource n
  | n < 0     = Just $ Just $ negate n -- Removing negative = adding opposite.
  | n == 0    = Just Nothing -- Take away nothing
  | otherwise = Nothing -- Can't take away >0 amount of a resource that isn't there!

without :: Loot -> Loot -> Maybe Loot
without = fmap Loot .: mergeA
  (traverseMissing $ const pure)
  (traverseMaybeMissing $ const removeMissingResource)
  (zipWithAMatched $ const minusGeq0)
  `on` unLoot

instance Semigroup Loot where
  (<>) = Loot .: Map.unionWith (+) `on` unLoot

instance Monoid Loot where
  mempty = Loot mempty



{- {- {- ACTOR -} -} -}

newtype UID = UID {unwrapUID :: Int}
  deriving stock (Eq, Ord, Show, Generic)

-- An Actor should always correspond to exactly one Entity in a World
data Actor = Actor
  { aname :: Text
  , owner :: Text
  , ownID :: UID
  , coords :: Coords
  , range :: Int -- Shooting & throwing distance
  , baseVision :: Int -- Seeing distance (when alive!)
  , queue :: Deque Action
  , done :: Bool -- Player is done entering actions
  } deriving stock (Eq, Generic)

vision :: (World w) => w -> Actor -> Int
vision w a = if actorAlive w a
  then baseVision a
  else 0

pushAct :: Action -> Actor -> Actor
pushAct actn a = a {queue = D.cons actn (queue a)}


{- {- {- COORDS -} -} -}

type Coords = (Int, Int)

data Direction = N | NE | E | SE | S | SW | W | NW
  deriving stock (Eq, Enum, Bounded, Show, Generic)

step :: Direction -> Coords -> Coords
step N = second (+ 1)
step S = second (subtract 1)
step E = first (+ 1)
step W = first (subtract 1)
step NE = stepTwice N E
step NW = stepTwice N W
step SE = stepTwice S E
step SW = stepTwice S W

stepTwice :: Direction -> Direction -> Coords -> Coords
stepTwice = (.) `on` step

ringAround :: (Enum a, Num a) => (a, a) -> a -> [(a, a)]
ringAround (cx, cy) r = [(x, cy - r) | x <- [cx-r .. cx+r-1]]
                     ++ [(cx + r, y) | y <- [cy-r .. cy+r-1]]
                     ++ [(x, cy + r) | x <- [cx+r,cx+r-1 .. cx-r+1]]
                     ++ [(cx - r, y) | y <- [cy+r,cx+r-1 .. cy-r+1]]


{- {- {- ACTION -} -} -}

data Action
  = Dir DirAction Direction
  | Undir UndirAction
  deriving stock (Eq, Show, Generic)

data DirAction
  = Move
  | Shoot
  | Blast
  | Throw Loot
  | Grab
  | Repair
  | Build Int
  deriving stock (Eq, Ord, Show, Generic)

instance Enum DirAction where
  fromEnum Move      = 0
  fromEnum Shoot     = 1
  fromEnum Blast     = 2
  fromEnum (Throw _) = 3
  fromEnum Grab      = 4
  fromEnum Repair    = 5
  fromEnum (Build _) = 6
  toEnum 0 = Move
  toEnum 1 = Shoot
  toEnum 2 = Blast
  toEnum 3 = Throw mempty
  toEnum 4 = Grab
  toEnum 5 = Repair
  toEnum 6 = Build 1
  toEnum n = toEnum (n `mod` 7)

instance Bounded DirAction where
  minBound = Move
  maxBound = Build 1

data UndirAction
  = RepairMe
  | ShootMe
  | Recycle
  | UpRange
  | UpVision
  | Wait
  deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)

cost :: Action -> Loot
cost (Dir Move _)      = mempty
cost (Dir Shoot _)     = singloot Actions 1 <> singloot Hearts 1
cost (Dir Blast _)     = singloot Actions 2 <> singloot Hearts 4
cost (Dir (Throw l) _) = l
cost (Dir Grab _)      = singloot Actions 1
cost (Dir Repair _)    = singloot Actions 4 <> singloot Hearts 1
cost (Dir (Build n) _) = singloot Hearts n
cost (Undir RepairMe)  = singloot Actions 6 <> singloot Hearts 1
cost (Undir ShootMe)   = singloot Actions (-1)
cost (Undir Recycle)   = singloot Actions (-1) <> singloot Hearts 2
cost (Undir UpRange)   = singloot Actions 2
cost (Undir UpVision)  = singloot Actions 4
cost (Undir Wait)      = mempty

getDir :: Action -> Maybe Direction
getDir (Dir _ d) = Just d
getDir _ = Nothing

isRanged :: Action -> Bool
isRanged (Undir _) = False
isRanged (Dir Shoot _) = True
isRanged (Dir Blast _) = True
isRanged (Dir (Throw _) _) = True
isRanged (Dir _ _) = False


{- {- {- WORLD -} -} -}

data Snapshot = Snapshot
  { actorStates :: Map UID Actor
  , actorCoords :: Map UID Coords
  , actorOrder :: Seq UID
  , gridState :: Grid
  } deriving stock (Generic)

-- A bunch of squares that may contain at most one Entity each.
-- Should also have a lookup table that finds the location of a given Actor,
--   and a list of all empty squares,
--   and a StdGen,
--   and a log of previous game states!
class (FromJSON w, ToJSON w) => World w where
  mkWorld :: StdGen -> Int -> w -- ^ Given a StdGen and the desired size, make a World.
  splitGen :: State w StdGen -- ^ Get a StdGen by splitting the World's internal StdGen
  empties :: w -> [Coords] -- ^ All empty squares
  actors :: w -> [UID] -- ^ All actors
  wrapAround :: Coords -> w -> Coords -> Coords -- ^ Wrap coords, placing seam(s) as far as possible from the center
  wrapCoords :: w -> Coords -> Coords -- ^ Wrap to the canonical coordinates
  avgPos :: w -> [Coords] -> Coords -- ^ Get a position in the middle of some coords
  getSquare :: Coords -> w -> Square
  lookupActor :: UID -> w -> Actor
  updateActor :: (Actor -> Actor) -> UID -> w -> w
  _addActor :: Actor -> State w UID -- ^ Register an Actor WITHOUT assigning it to its square. Only exists to be implemented by World instances. Use register instead.
  _unaddActor :: UID -> w -> w -- ^ Delete an Actor from actors. Only exists to be implemented by World instances. Use delActor instead.
  updateSquare :: (Square -> Square) -> Coords -> w -> w
  maxRange :: w -> Int
  maxVision :: w -> Int
  turnOrder :: w -> [UID] -- ^ Order in which pawns will act next round
  shuffleTurnOrder :: w -> w -- ^ Shuffle the turnOrder
  origin :: w -> Maybe Coords -- ^ (0, 0) - Top left corner
  extent :: w -> Maybe Coords -- ^ Bottom right corner. With origin, draws a bounding box, if applicable.
  snapshots :: w -> Seq (Seq Snapshot) -- ^ Views of the world after each action, divided by turn.
  takeSnapshot :: w -> w -- ^ Take a snapshot
  newSnapTurn :: w -> w -- ^ Start the next subsequence of snapshots  

  register :: Actor -> StateT w Maybe UID -- Register this Actor in the World.
  register a = do
    aID <- hoist generalize $ _addActor a
    let c = coords a
    s <- gets $ getSquare c
    e <- lift s
    lift . guard . isNothing $ actorID e
    modify $ updateSquare (fmap \e' -> e' {actorID = Just aID}) c
    return aID

  findActor :: UID -> w -> Coords
  findActor = coords .: lookupActor

  actorAlive :: w -> Actor -> Bool
  actorAlive w a = any ((> 0) . health) $ (`getSquare` w) $ coords a

  chActorPos :: Coords -> UID -> w -> w -- Point the Actor to a different Square
  chActorPos c = updateActor \a -> a {coords = c}

  delActor :: UID -> w -> w
  delActor aID w = _unaddActor aID . 
      updateSquare (const Nothing) (findActor aID w) $ w

  actor :: w -> Entity -> Maybe Actor
  actor w e = flip lookupActor w <$> actorID e

  numDone :: w -> Int
  numDone = sum . map fromEnum
    . \w -> done . flip lookupActor w <$> actors w

  -- Get a view of the World with given radius centered at the given Coords.
  view :: Int -> Coords -> w -> Grid
  view r c w = (`getSquare` w) <<$>> ([[bimap (+ x) (+ y) c | x <- [- r .. r]] | y <- [- r .. r]])

  multiView :: [(Int, Coords)] -> w -> Grid
  multiView = multiViewVia getSquare

  roundView :: [UID] -> w -> Seq Grid
  roundView aIDs w = let
    replay = fromMaybe Empty $ lookup (length (snapshots w) - 1) (snapshots w)
    in
      if or . fmap (or . \s -> (`Map.notMember` actorStates s) <$> aIDs) $ replay
        then Empty -- This replay predates our joining the game. Nothing to see.
        else
          ( \s -> let
              -- Why do we need to offset wrapAround avgPos-based grids?
              -- And why only for roundView?
              -- What gives !
              offset = if isJust $ origin w
                then id
                else both (subtract 1)
              aMap = actorStates s
              cMap = actorCoords s
              g = gridState s
              vision' aID = do
                (x, y) <- wrapCoords w <$> cMap Map.!? aID
                if maybe 10 health (g !! y !! x) > 0
                  then baseVision <$> (aMap Map.!? aID)
                  else return 0
              views = catMaybes $
                (\aID -> do
                  r <- vision' aID
                  c <- offset <$> cMap Map.!? aID
                  return (r, c)
                ) <$> aIDs
            in
              multiViewVia (\c _ -> let c' = wrapCoords w c in g !! snd c' !! fst c') views w
          ) <$> replay

  multiViewVia :: (Coords -> w -> Square) -> [(Int, Coords)] -> w -> Grid
  multiViewVia getSquare' views w = let
      wrap = fromMaybe (wrapCoords w) $ do
        guard $ origin w == Nothing
        let ctr = avgPos w $ snd <$> views
        return $ wrapAround ctr w
      visibleCoords = Set.fromList $ views >>= (\(r, c) -> concat [[wrap $ bimap (+ x) (+ y) c | x <- [- r .. r]] | y <- [- r .. r]])
      xs = Set.map (fst :: Coords -> Int) visibleCoords
      ys = Set.map (snd :: Coords -> Int) visibleCoords
      minX = maybe (minimum xs) fst $ origin w
      minY = maybe (minimum ys) snd $ origin w
      maxX = maybe (maximum xs) fst $ extent w
      maxY = maybe (maximum ys) snd $ extent w
      getIfVisible c = if c `Set.member` visibleCoords
        then getSquare' c w
        else Just (Entity Nothing (Just "?") 0 mempty)
        -- else Just (Entity Nothing Nothing 0 (singloot Actions x <> singloot Hearts y))
    in
      getIfVisible <<$>> ([[(x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]])

  getEmptySpace :: StateT w Maybe Coords
  getEmptySpace = do
    spaces <- gets empties
    lift . guard . not . null $ spaces
    gen <- hoist generalize splitGen
    return $ spaces !! fst (randomR (0, length spaces - 1) gen)

  -- Place an Entity in a random empty Square. Fails if all Squares are full.
  scatter :: Entity -> w -> Maybe w
  scatter e = execStateT $ getEmptySpace >>= (modify . putSquare (Just e))

  fill :: Entity -> w -> w
  fill e = farthest $ scatter e

  fillFraction :: (RealFrac n) => n -> Entity -> w -> w
  fillFraction portionToFill e w = tugs (scatter e)
    (floor $ (fromIntegral . length $ empties w) * portionToFill) w
  
  putPawn :: Entity -> Actor -> Coords -> StateT w Maybe UID
  putPawn e a c = do
    modify $ putSquare (Just e) c
    register $ a {coords = c}

  scatterPawn :: Entity -> Actor -> StateT w Maybe UID
  scatterPawn e a = getEmptySpace >>= putPawn e a

  clusterPawns :: Int -> [(Entity, Actor)] -> StateT w Maybe [UID]
  clusterPawns maxRadius pawns = do
    center <- getEmptySpace

    w0 <- get
    let radii = [2,4..maxRadius]
        positionRings = sieve 2 . ringAround center <$> radii
        spaceRings = filter ((== Nothing) . flip getSquare w0) <$> positionRings
        clusters = scanl (++) [] spaceRings
    spaces <- lift $ find ((> length pawns) . length) clusters

    gen <- hoist generalize splitGen
    forM
      ( zip pawns $
        shuffle' spaces (length spaces) gen )
      (uncurry $ uncurry putPawn)

  -- Search in a line from the Coords in the given direction until we find what we're looking for or we reach the given max range.
  -- The predicate takes the tuple (this Square, the next Square). This allows stopping before OR upon the desired Square.
  project :: Coords -> Direction -> Int -> ((Square, Square) -> Bool) -> w -> Coords
  project c d r p w
    | r == 0 || p (getSquare c w, getSquare nextSq w) = c
    | otherwise = project nextSq d (r - 1) p w
    where
      nextSq = step d c
  
  -- Start the search 1 square out. (To avoid hitting yourself!)
  project1 :: Coords -> Direction -> Int -> ((Square, Square) -> Bool) -> w -> Coords
  project1 = projectN 1

  projectN :: Int -> Coords -> Direction -> Int -> ((Square, Square) -> Bool) -> w -> Coords
  projectN n c d r p w = project (nTimes n (step d) c) d (r - 1) p w

  -- Merge the given Entity's contents with the Square at the given Coords.
  putSquare :: Square -> Coords -> w -> w
  putSquare s c = updateSquare (s <>) c . fromMaybe id do
    e <- s
    aID <- actorID e
    return $ chActorPos c aID
  
  -- Empty out one Square, and add its contents to those of another.
  -- If both have Actors, the second one's Actor is DESTROYED! bad bad bad
  move :: Coords -> Coords -> w -> w
  move a b w = w & putSquare a' b . updateSquare (const Nothing) a
    where
      a' = getSquare a w

  -- Update a shot Square.
  hit :: Coords -> w -> w
  hit = updateSquare (fmap \e -> if health e > 0
    then e
      { health = health e - 1
      , contents = contents e <> singloot Hearts 1
      }
    else e
    )

  takeLoot :: Coords -> StateT w Maybe Loot
  takeLoot c = do
    s <- gets $ getSquare c
    e <- lift s
    modify $ updateSquare (fmap \e' -> e' {contents = mempty}) c
    return $ contents e
  
  grab :: Coords -> Coords -> w -> Maybe w
  grab grabbee grabber w = do
    let grabbee' = getSquare grabbee w
    guard $ grabbable grabbee' && dead grabbee'
    if passable grabbee'
      then return $ move grabbee grabber w
      else flip execStateT w do
        l <- takeLoot grabbee
        modify $ putLoot l grabber

  repair :: Coords -> w -> Maybe w
  repair c w = do
      e <- getSquare c w
      return $ updateSquare (const . Just $ e {health = health e + 1}) c w
  
  build :: Int -> Coords -> w -> w
  build n = updateSquare
    \case Nothing -> Just (Entity Nothing Nothing n mempty)
          Just e  -> if health e > 0 || isJust (actorID e)
            then Just e
            else Just $ e {health = health e + n}
  
  -- Dump Loot into a Square.
  putLoot :: Loot -> Coords -> w -> w
  putLoot l = putSquare $ Just (Entity Nothing Nothing 0 l)

  spendLoot :: Loot -> Coords -> w -> Maybe w
  spendLoot l c w = do
    let s = getSquare c w
    me <- s
    cont' <- contents me `without` l
    return $ updateSquare (fmap \e -> e {contents = cont'}) c w

  spendFor :: Action -> Coords -> w -> Maybe w
  spendFor = spendLoot . cost
  
  doAct :: Action -> Coords -> w -> Maybe w
  doAct a c w = do
    let s = getSquare c w
    me <- s
    let _cont = contents me
    aID <- actorID me
    let act = lookupActor aID w
    case a of
      Dir a' d -> case a' of
        Move -> do
          let destination = step d c
          guard . passable . getSquare destination $ w
          return . move c destination $ w
        Shoot -> let target = project1 c d (range act) (hittable . fst) w
          in return $ putLoot (singloot Hearts 1) target $ hit target w
        Blast -> flip execStateT w do
          let target = project1 c d (range act) (hittable . fst) w
          let splash = ringAround target 1
          modify $ foldr (.) id (fmap hit $ target:splash)
          modify $ putLoot (singloot Hearts 2) target
        Throw l -> do
          let throwee = project1 c d (range act) (\(thisSq, nextSq) ->
                (hittable nextSq && isNothing (join . fmap actorID $ nextSq))
                || hittable thisSq || isJust (join . fmap actorID $ thisSq)
                ) w
          return $ putLoot l throwee w
        Grab -> do
          let grabbee = step d c
          grab grabbee c w
        Repair -> repair (step d c) w
        Build n -> flip execStateT w do
          modify $ build n (step d c)
          -- cont' <- lift $ cont `without` singloot Hearts n
          -- modify $ updateSquare (fmap \e -> e {contents = cont'}) c
      Undir RepairMe -> repair c w
      Undir ShootMe -> return $ hit c w
      Undir Recycle -> return w
      Undir UpRange -> return $ updateActor (\act' -> act' {range = 
        clamp 0 (maxRange w) $ range act' + 1
        }) aID w
      Undir UpVision -> return $ updateActor (\act' -> act' {baseVision = 
        baseVision act' + 1 -- clamp 0 (maxVision w) $ vision act' + 1
        }) aID w
      Undir Wait -> return w

  -- Pop an Action from the Actor's queue, and do it.
  popAct :: UID -> w -> w
  popAct aID w =
    updateActor (\a -> a -- Always pop an action (if any) from the queue
      { queue = maybe (queue a) snd . D.unsnoc . queue $ a }) aID
    $ fromMaybe w do
      let findMeIn = findActor aID
      (actn, _) <- D.unsnoc . queue $ lookupActor aID w -- Maybe get the action
      ( if Just True == fmap ((> 0) . health) (getSquare (findMeIn w) w)
            then Just w else Nothing -- Nothing if pawn is missing/dead
        ) >>= (\w' -> doAct actn (findMeIn w') w')
          >>= (\w' -> spendFor actn (findMeIn w') w')
          >>= Just . takeSnapshot

  giveAllLoot :: Loot -> w -> w
  giveAllLoot l w = foldl' (&) w
    . fmap (
      (\c -> if fromMaybe False . fmap ((> 0) . health) $ getSquare c w
        then putLoot l c
        else id
      )
      . (`findActor` w)
      )
    . actors $ w
  
  runRound :: w -> w
  runRound = execState do
        order <- reverse <$> gets turnOrder
        modify (foldr (.) id $ popAct <$> order)

  runTurn :: w -> w 
  runTurn = execState do
    let queuesEmpty w = all (D.null . queue . flip lookupActor w) . actors $ w
    modify newSnapTurn
    modify takeSnapshot
    modify $ until queuesEmpty runRound
    modify . giveAllLoot $ singloot Actions 1
    everyone <- gets actors
    modify $ foldr (.) id $
      ( \aID w -> updateActor
          (\a -> a {done = not $ actorAlive w a})
          aID w
      )
      <$> everyone
    modify shuffleTurnOrder
