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
    , lonly
    , contains
    , without
    , multloot

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
  , sealed :: Bool
  } deriving stock (Eq, Show, Generic)

type Square = Maybe Entity

type Grid = [[Square]]

passable :: Square -> Bool
passable Nothing = True -- An empty square is passable.
passable (Just e) = isNothing (actorID e) && (health e <= 0)

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
        return $ mixText -- Merging 2 named Entities? This'll be fun! (This should never happen?)
          (mkStdGen . sumText -- Make a stdGen based on details of these Entities
            $ show e <> show e')
          n n'
      , ename e
      , ename e'
      ]
    , health = health e + health e'
    , contents = contents e <> contents e'
    , sealed = sealed e || sealed e'
    }

instance Monoid Entity where
  mempty = Entity Nothing Nothing 0 mempty False



{- {- {- LOOT -} -} -}

data Resource = Juice | Scrap
    deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)

res :: Resource -> Loot -> Int
res r = Map.findWithDefault 0 r . unLoot

newtype Loot = Loot {unLoot :: Map Resource Int}
    deriving newtype (Eq, Ord, Show)
    deriving stock Generic

{- HLINT ignore "Use one" -}
singloot :: Resource -> Int -> Loot
singloot = Loot .: Map.singleton 

lonly :: Resource -> Loot -> Loot
lonly r l = singloot r $ res r l

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

multloot :: Float -> Loot -> Loot
multloot n l = Loot $ ceiling . (* n) . fromIntegral <$> unLoot l

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

ringAround :: Coords -> Int -> [Coords]
ringAround (cx, cy) 0 = [(cx, cy)]
ringAround (cx, cy) r = [(x, cy - r) | x <- [cx-r .. cx+r-1]]
                     ++ [(cx + r, y) | y <- [cy-r .. cy+r-1]]
                     ++ [(x, cy + r) | x <- [cx+r,cx+r-1 .. cx-r+1]]
                     ++ [(cx - r, y) | y <- [cy+r,cy+r-1 .. cy-r+1]]


{- {- {- ACTION -} -} -}

data Action
  = Dir DirAction Direction
  | Undir UndirAction
  deriving stock (Eq, Show, Generic)

data DirAction
  = Move
  | Jump
  | Shoot
  | Blast
  | Throw Loot
  | Build Int
  | Hurl
  | Grab
  | Repair
  deriving stock (Eq, Ord, Show, Generic)

instance Enum DirAction where
  fromEnum Move      = 0
  fromEnum Jump      = 1
  fromEnum Shoot     = 2
  fromEnum Blast     = 3
  fromEnum (Throw _) = 4
  fromEnum (Build _) = 5
  fromEnum Hurl      = 6
  fromEnum Grab      = 7
  fromEnum Repair    = 8
  toEnum 0 = Move
  toEnum 1 = Jump
  toEnum 2 = Shoot
  toEnum 3 = Blast
  toEnum 4 = Throw mempty
  toEnum 5 = Build 1
  toEnum 6 = Hurl
  toEnum 7 = Grab
  toEnum 8 = Repair
  toEnum n = toEnum (n `mod` 7) {- HLINT ignore "Use safeToEnum" -}

instance Bounded DirAction where
  minBound = Move
  maxBound = Repair

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
cost (Dir Jump _)        = singloot Juice 2
cost (Dir Shoot _)     = singloot Juice 1 <> singloot Scrap 1
cost (Dir Blast _)     = singloot Juice 2 <> singloot Scrap 4
cost (Dir (Throw l) _) = l
cost (Dir Grab _)      = singloot Juice 1
cost (Dir Repair _)    = singloot Juice 4 <> singloot Scrap 1
cost (Dir (Build n) _) = singloot Scrap n
cost (Dir Hurl _)      = singloot Juice 1
cost (Undir RepairMe)  = singloot Juice 6 <> singloot Scrap 1
cost (Undir ShootMe)   = singloot Juice (-1)
cost (Undir Recycle)   = singloot Juice (-1) <> singloot Scrap 2
cost (Undir UpRange)   = singloot Juice 2
cost (Undir UpVision)  = singloot Juice 4
cost (Undir Wait)      = mempty

getDir :: Action -> Maybe Direction
getDir (Dir _ d) = Just d
getDir _ = Nothing

isRanged :: Action -> Bool
isRanged (Undir UpRange) = True
isRanged (Undir _) = False
isRanged (Dir Jump _)  = True
isRanged (Dir Shoot _) = True
isRanged (Dir Blast _) = True
isRanged (Dir (Throw _) _) = True
isRanged (Dir (Build _) _) = True
isRanged (Dir Hurl _) = True
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
      if any (or . \s -> (`Map.notMember` actorStates s) <$> aIDs) replay
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
              views = mapMaybe
                (\aID -> do
                  r <- vision' aID
                  c <- offset <$> cMap Map.!? aID
                  return (r, c)
                ) aIDs
            in
              multiViewVia (\c _ -> let c' = wrapCoords w c in g !! snd c' !! fst c') views w
          ) <$> replay

  multiViewVia :: (Coords -> w -> Square) -> [(Int, Coords)] -> w -> Grid
  multiViewVia getSquare' views w = let
      wrap = fromMaybe (wrapCoords w) $ do
        guard $ isNothing $ origin w
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
        else Just (Entity Nothing (Just "?") 0 mempty False)
        -- else Just (Entity Nothing Nothing 0 (singloot Juice x <> singloot Scrap y))
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
  -- Return a tuple of the square and the number of spaces traveled.
  project :: Coords -> Direction -> Int -> ((Square, Square) -> Bool) -> w -> (Coords, Int)
  project c d r p w
    | r == 0 || p (getSquare c w, getSquare nextSq w) = (c, 0)
    | otherwise = second (+1) $ project nextSq d (r - 1) p w
    where
      nextSq = step d c

  project_ :: Coords -> Direction -> Int -> ((Square, Square) -> Bool) -> w -> Coords
  project_ c d r p w = fst $ project c d r p w
  
  -- Start the search 1 square out. (To avoid hitting yourself!)
  project1 :: Coords -> Direction -> Int -> ((Square, Square) -> Bool) -> w -> Coords
  project1 = projectN 1

  projectN :: Int -> Coords -> Direction -> Int -> ((Square, Square) -> Bool) -> w -> Coords
  projectN n c d r = project_ (nTimes n (step d) c) d (r - 1)

  -- Empty out a square and return its contents
  takeSquare :: Coords -> State w Square
  takeSquare c = do
    sq <- gets $ getSquare c
    modify $ updateSquare (const Nothing) c
    return sq

  -- Merge the given Entity's contents with the Square at the given Coords.
  putSquare :: Square -> Coords -> w -> w
  putSquare s c = updateSquare (s <>) c . fromMaybe id do
    e <- s
    aID <- actorID e
    return $ chActorPos c aID
  
  -- Empty out one Square, and add its contents to those of another.
  -- If both have Actors, the second one's Actor is DESTROYED! bad bad bad
  move :: Coords -> Coords -> w -> w
  move a b = execState do
    sq <- takeSquare a
    modify $ putSquare sq b

  -- Update a damaged Square.
  hit :: Coords -> w -> w
  hit = updateSquare (fmap \e -> if health e > 0
    then e
      { health = health e - 1
      , contents = contents e <> singloot Scrap 1
      , sealed = sealed e && health e - 1 > 0
      }
    else e
    )

  blast :: Coords -> w -> w
  blast c = foldr ((.) . hit) id (c : ringAround c 1)

  takeLoot :: Coords -> StateT w Maybe Loot
  takeLoot c = do
    s <- gets $ getSquare c
    e <- lift s
    guard $ health e <= 0 && not (sealed e)
    let takings = if passable s then contents e
                  else multloot 0.5 $ contents e
    leavings <- lift $ contents e `without` takings
    modify $ updateSquare (fmap \e' -> e' {contents = leavings, sealed = sealed e' || not (passable s)}) c
    return takings
  
  grab :: Coords -> Coords -> w -> Maybe w
  grab grabbee grabber w = do
    let grabbee' = getSquare grabbee w
    guard $ grabbable grabbee'
    if passable grabbee'
      then return $ move grabbee grabber w
      else flip execStateT w do
        l <- takeLoot grabbee
        modify $ putLoot l grabber

  repair :: Coords -> w -> Maybe w
  repair c w = do
      e <- getSquare c w
      return $ updateSquare (const . Just $ e {health = health e + 1}) c w

  hurl :: Coords -> Direction -> Int -> w -> Maybe w
  hurl c d r = execStateT do
    trgSq <- gets $ getSquare c
    ball <- lift trgSq
    modify $ updateSquare (const Nothing) c
    modify $ launch ball c d r

  launch :: Entity -> Coords -> Direction -> Int -> w -> w
  launch ball c d r w =
      let hitPredicate (curSq, nextSq)
              = hittable curSq
             || (not . passable) (Just ball) && (not . passable) nextSq
          r' = max 0 $ r - health ball
          (target, dist) = project c d r' hitPredicate w
          r'' = r' - dist
          clack = fromMaybe <$> id <*> execStateT do guard (r'' > 0)
                                                     nextSq <- gets $ getSquare $ step d target
                                                     guard $ (not . passable) nextSq
                                                     modifyM $ hurl (step d target) d r''
      in clack $ putSquare (Just ball) target w

  -- Dump Loot into a Square.
  putLoot :: Loot -> Coords -> w -> w
  putLoot l = putSquare $ Just (Entity Nothing Nothing 0 l False)

  spendLoot :: Loot -> Coords -> w -> Maybe w
  spendLoot l c w = do
    let s = getSquare c w
    me <- s
    cont' <- contents me `without` l
    return $ updateSquare (fmap \e -> e {contents = cont'}) c w

  spendFor :: Action -> Coords -> w -> Maybe w
  spendFor = spendLoot . cost
  
  doAct :: Action -> Coords -> w -> Maybe w
  doAct act c = execStateT do
    myE <- getsM $ getSquare c
    aID <- lift $ actorID myE
    r <- gets $ range . lookupActor aID
    case act of
      Dir dirAct d -> case dirAct of
        Move -> modifyM $ hurl c d (health myE + 1)
        Jump -> modifyM $ hurl c d (r + 1)
        Shoot -> do target <- gets $ project1 c d r (hittable . fst)
                    modify $ putLoot (singloot Scrap 1) target . hit target
        Blast -> do
          target <- gets $ project1 c d r (hittable . fst)
          modify $ blast target
          modify $ putLoot (singloot Scrap 2) target
        Throw l -> do modify $ putLoot l (step d c)
                      modifyM $ hurl (step d c) d (r - 1)
        Grab -> do
          let grabbee = step d c
          modifyM $ grab grabbee c
        Repair -> modifyM $ repair (step d c)
        Build n -> do let wall = Just $ Entity Nothing Nothing n mempty False
                      space <- gets $ getSquare $ step d c
                      guard $ passable space
                      modify $ putSquare wall (step d c)
                      modifyM $ hurl (step d c) d r
          -- cont' <- lift $ cont `without` singloot Scrap n
          -- modify $ updateSquare (fmap \e -> e {contents = cont'}) c
        Hurl -> modifyM $ hurl (step d c) d r
      Undir RepairMe -> modifyM $ repair c
      Undir ShootMe -> modify $ hit c
      Undir Recycle -> pass
      Undir UpRange -> modify $ updateActor (\a -> a {range = range a + 1}) aID
      Undir UpVision -> modify $ updateActor (\a -> a {baseVision = baseVision a + 1}) aID
      Undir Wait -> pass

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
      (\c -> if maybe False ((> 0) . health) $ getSquare c w
        then putLoot l c
        else id
      )
      . (`findActor` w)
      )
    . actors $ w
  
  runRound :: w -> w
  runRound = execState do
        order <- reverse <$> gets turnOrder
        modify $ foldr ((.) . popAct) id order

  runTurn :: w -> w 
  runTurn = execState do
    let queuesEmpty w = all (D.null . queue . flip lookupActor w) . actors $ w
    modify newSnapTurn
    modify takeSnapshot
    modify $ until queuesEmpty runRound
    modify . giveAllLoot $ singloot Juice 1
    everyone <- gets actors
    modify $ foldr ((.) . (\ aID w ->
        updateActor (\ a ->
            a {done = not $ actorAlive w a}
        ) aID w
      )) id everyone
    modify shuffleTurnOrder
