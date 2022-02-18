module Mechanics
    ( Entity (..)
    , Square
    , passable
    , hittable
    , grabbable

    , Loot (..)
    , contains
    , without

    , UID (..)
    , Actor (..)
    , pushAct

    , Coords
    , Direction (..)
    , step

    , Action (..)
    , DirAction (..)
    , UndirAction (..)
    , cost
    , getDir

    , World (..)
    ) where

import           Data.Bool.HT (if')
import           Data.Composition
import qualified Deque.Lazy as D
import           Deque.Lazy (Deque)
import           Prelude (until)
import           Relude
import           Relude.Unsafe ((!!))
import           System.Random
import           System.Random.Shuffle



{- {- {- ENTITY -} -} -}

data Entity = Entity
  { actorID :: Maybe UID
  , health :: Int
  , contents :: Maybe Loot
  }

type Square = Maybe Entity

passable :: Square -> Bool
passable Nothing = True -- An empty square is passable.
passable (Just (Entity (Just _) _ _)) = False -- Entities that have an Actor are always impassable.
passable (Just (Entity Nothing hp _)) = hp <= 0 -- Entities that have positive health are impassable.
-- Otherwise, entities are passable, are looted when stepped on, and are destroyed when looted.

-- Empty squares and Entities with zero health can be shot and thrown through.
hittable :: Square -> Bool
hittable = maybe False $ (> 0) . health

-- A square is grabbable only if it contains loot.
grabbable :: Square -> Bool
grabbable = maybe False $ (/= mempty) . contents

instance Semigroup Entity where
  e <> e' = Entity
    (asum . (actorID <$>) $ [e, e']) -- Bad bad! When merging 2 Entities with Actors, only one Actor remains. This is why Actor Entities must be impassable.
    (health e + health e') 
    (contents e <> contents e')

instance Monoid Entity where
  mempty = Entity Nothing 0 Nothing



{- {- {- LOOT -} -} -}

data Loot = Loot
  { hearts :: Int
  , actions :: Int
  } deriving stock (Eq, Ord, Show)

contains :: Loot -> Loot -> Bool
l `contains` l' = hearts l >= hearts l' && actions l >= actions l'

without :: Loot -> Loot -> Maybe Loot
l `without` l' = do
  guard $ l `contains` l'
  return $ Loot (hearts l - hearts l') (actions l - actions l')

instance Semigroup Loot where
  l <> l' = Loot (hearts l + hearts l') (actions l + actions l')

instance Monoid Loot where
  mempty = Loot 0 0



{- {- {- ACTOR -} -} -}

newtype UID = UID {unwrapUID :: Int}

-- An Actor should always correspond to exactly one Entity in a World
data Actor = Actor
  { name :: Text
  , coords :: Coords
  , range :: Int -- Shooting & throwing distance
  , vision :: Int -- Seeing distance
  , queue :: Deque Action
  } deriving stock Eq

pushAct :: Action -> Actor -> Actor
pushAct actn a = a {queue = D.cons actn (queue a)}


{- {- {- COORDS -} -} -}

type Coords = (Int, Int)

data Direction = N | NE | E | SE | S | SW | W | NW
  deriving stock (Eq, Enum, Bounded, Show)

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


{- {- {- ACTION -} -} -}

data Action
  = Dir DirAction Direction
  | Undir UndirAction
  deriving stock (Eq, Show)

data DirAction
  = Move
  | Shoot
  | Throw Loot 
  | Grab
  deriving stock (Eq, Ord, Show)

data UndirAction
  = Die
  | Hearts2HP
  | HP2Hearts
  deriving stock (Eq, Ord, Show)

cost :: Action -> Int
cost (Dir Move _)       = 1
cost (Dir Shoot _)      = 1
cost (Dir (Throw _) _)  = 0
cost (Dir Grab _)       = 1
cost (Undir Die)        = 0
cost (Undir Hearts2HP)  = 3
cost (Undir HP2Hearts)  = 0

getDir :: Action -> Maybe Direction
getDir (Dir _ d) = Just d
getDir _ = Nothing



{- {- {- WORLD -} -} -}

generalizeState :: (Monad m) => StateT s Identity a -> StateT s m a
generalizeState = StateT . fmap (return . runIdentity) . runStateT

-- A bunch of squares that may contain at most one Entity each.
-- Should also have a lookup table that finds the location of a given Actor,
--   and a list of all empty squares,
--   and a StdGen.
class World w where
  mkWorld :: StdGen -> Int -> w -- Given a StdGen and the desired size, make a World.
  splitGen :: State w StdGen -- Get a StdGen by splitting the World's internal StdGen
  empties :: w -> [Coords] -- All empty squares
  actors :: w -> [UID] -- All actors
  getSquare :: Coords -> w -> Square
  lookupActor :: UID -> w -> Actor
  updateActor :: (Actor -> Actor) -> UID -> w -> w
  addActor :: Actor -> State w UID -- Register an Actor WITHOUT assigning it to its square. Only exists to be implemented by World instances. Use register instead.
  updateSquare :: (Square -> Square) -> Coords -> w -> w

  register :: Actor -> StateT w Maybe UID -- Register this Actor in the World.
  register a = do
    aID <- generalizeState $ addActor a
    let c = coords a
    s <- gets $ getSquare c
    e <- lift s
    lift . guard . isNothing $ actorID e
    modify $ updateSquare (fmap \e' -> e' {actorID = Just aID}) c
    return aID

  findActor :: UID -> w -> Coords
  findActor = coords .: lookupActor

  chActorPos :: Coords -> UID -> w -> w -- Point the Actor to a different Square
  chActorPos c = updateActor \a -> a {coords = c}

  actor :: w -> Entity -> Maybe Actor
  actor w e = flip lookupActor w <$> actorID e

  -- Get a view of the World with given radius centered at the given Coords.
  view :: Int -> Coords -> w -> [[Square]]
  view r c w = (`getSquare` w) <<$>> ([[bimap (+ x) (+ y) c | x <- [- r .. r]] | y <- [- r .. r]])

  -- Place an Entity in a random empty Square. Fails if all Squares are full.
  scatter :: Entity -> StateT w Maybe Coords
  scatter e = do
    spaces <- gets empties
    lift . guard . not . null $ spaces
    gen <- generalizeState splitGen
    let c = spaces !! fst (randomR (0, length spaces - 1) gen)
    modify $ putSquare (Just e) c
    return c

  scatterActor :: Entity -> Actor -> StateT w Maybe UID
  scatterActor e a = do
      c <- scatter e
      register $ a {coords = c}
  
  scatterActors :: [Text] -> Entity -> Actor -> w -> Maybe w
  scatterActors names e a w = foldl' (>>=) (Just w) [execStateT $ scatterActor e (a {name = thisName}) | thisName <- names]

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
  project1 c d r p w = project (step d c) d (r - 1) p w

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
  hit c = putLoot (Loot {hearts = 1, actions = 0}) c .
    updateSquare (fmap \e ->
      if health e > 0
      then e {health = health e - 1}
      else e
      ) c

  takeLoot :: Coords -> StateT w Maybe Loot
  takeLoot c = do
    s <- gets $ getSquare c
    e <- lift s
    cont <- lift $ contents e
    lift . guard $ health e <= 0
    modify $ updateSquare (fmap \e' -> e' {contents = Nothing}) c
    return cont
  
  grab :: Coords -> Coords -> w -> Maybe w
  grab grabbee grabber w = do
    let grabbee' = getSquare grabbee w
    guard $ grabbable grabbee'
    if passable grabbee'
      then return $ move grabbee grabber w
      else flip execStateT w do
        l <- takeLoot grabbee
        modify $ putLoot l grabber

  -- Dump Loot into a Square.
  putLoot :: Loot -> Coords -> w -> w
  putLoot l = putSquare $ Just (Entity Nothing 0 $ Just l)

  spendActPts :: Int -> Coords -> w -> Maybe w
  spendActPts n c w = do
    let c' = getSquare c w
    me <- c'
    cont <- contents me
    let actions' = actions cont - n
    guard $ actions' >= 0
    return $ updateSquare (fmap \e -> e {contents = Just $ cont {actions = actions'}}) c w

  spendFor :: Action -> Coords -> w -> Maybe w
  spendFor = spendActPts . cost
  
  doAct :: Action -> Coords -> w -> Maybe w
  doAct a c w = do
    let c' = getSquare c w
    me <- c'
    cont <- contents me
    act <- actor w me
    case a of
      Dir a' d -> case a' of
        Move -> do
          let destination = step d c
          guard . passable . getSquare destination $ w
          return . move c destination $ w
        Shoot -> let target = project1 c d (range act) (hittable . fst) w
          in return . hit target $ w
        Throw l -> do
          let throwee = project1 c d (range act) (hittable . snd) w
          cont' <- cont `without` l
          return .
            putLoot l throwee .
            updateSquare (fmap \e -> e {contents = Just cont' }) c $ w
        Grab -> do
          let grabbee = project1 c d (range act) (grabbable . fst) w
          grab grabbee c w
      Undir Die -> return . putLoot (Loot {hearts = health me, actions = 0}) c .
        updateSquare (fmap \e -> e {health = 0}) c $ w
      Undir Hearts2HP -> do
        cont' <- cont `without` Loot {actions = 0, hearts = 1}
        return . updateSquare (fmap \e -> e {contents = Just cont', health = health e + 1}) c $ w
      Undir HP2Hearts -> return . hit c $ w

  -- Pop an Action from the Actor's queue, and do it.
  -- If the queue is empty, keep the world unchanged.
  popAct :: UID -> w -> w
  popAct aID w = fromMaybe w do
    let findIn = findActor aID
    (actn, queue') <- D.unsnoc . queue $ lookupActor aID w
    return . updateActor (\a -> a {queue = queue'}) aID . -- Always update the queue.
      if'
        (or . fmap ((<= 0) . health) . getSquare (findIn w) $ w)
        w . -- Keep the world unchanged if health WAS ORIGINALLY <= 0, or Actor's square was empty.
      fromMaybe w . -- Keep the World unchanged if the Actor can't afford the Action.
        (\w' -> spendFor actn (findIn w') w') . -- Even if the Action fails, try to pay for it.
      fromMaybe w . -- Keep the World unchanged if the Action fails.
        doAct actn (findIn w) $ w

  giveAllLoot :: Loot -> w -> w
  giveAllLoot l w = foldl' (&) w . fmap (putLoot l . (`findActor` w)) . actors $ w
  
  runTurn :: w -> w 
  runTurn = execState do
    gen <- splitGen
    everyone <- gets actors
    let runRound = foldr (.) id $ popAct <$> shuffle' everyone (length everyone) gen -- :: w -> w
    let queuesEmpty w = all (D.null . queue . flip lookupActor w) . actors $ w
    modify $ until queuesEmpty runRound
    modify . giveAllLoot $ Loot {hearts = 0, actions = 1}