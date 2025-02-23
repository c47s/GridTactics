module Util
    ( module Util -- This module consists entirely of helper functions, so export everything!
    ) where

import           Control.Monad.Except
import           Data.Bool.HT ((?:))
import           Data.Composition
import           Data.Maybe.HT
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq((:<|), (:|>)), (<|), (|>), (><))
import qualified Data.Text as T
import           GHC.Data.Maybe
import           Prelude ((!!))
import           Relude hiding ((?:))
import           Servant
import           System.Console.Haskeline
import           System.Random



-- Haskeline

type Check a msg = (a -> Maybe msg)

untilValidAnd :: (Read a, MonadIO m) => Check a String -> InputT m (Maybe String) -> InputT m a
untilValidAnd chk getInput = do
    ln <- getInput
    case readMaybe =<< ln of
        Nothing -> do
            outputStrLn "Invalid input."
            untilValidAnd chk getInput
        Just a -> case chk a of
            Just msg -> do
                outputStrLn msg
                untilValidAnd chk getInput
            Nothing -> return a

untilValid :: (Read a, MonadIO m) => InputT m (Maybe String) -> InputT m a
untilValid = untilValidAnd (const Nothing)

untilJustAnd :: MonadIO m => Check String String -> InputT m (Maybe String) -> InputT m String
untilJustAnd chk getInput = do
    ln <- getInput
    case ln of
        Nothing -> do
            outputStrLn "Please enter something."
            untilJustAnd chk getInput
        Just s -> case chk s of
            Just msg -> do
                outputStrLn msg
                untilJustAnd chk getInput
            Nothing -> return s

untilJust :: MonadIO m => InputT m (Maybe String) -> InputT m String
untilJust = untilJustAnd (const Nothing)

check :: msg -> (a -> Bool) -> Check a msg
check msg p a = toMaybe (not $ p a) msg

nonNeg :: (Num a, Ord a) => a -> Maybe String
nonNeg = check "Cannot be negative." (>= 0)

gr0 :: (Num a, Ord a) => a -> Maybe String
gr0 = check "Must be greater than zero." (> 0)

nonBlank :: String -> Maybe String
nonBlank = check "Please enter something." (not . null . words . fromString)

-- Compose 2 checks
(&>-) ::  Check a msg -> Check a msg -> Check a msg
(&>-) = liftA2 firstJust



-- State

modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = do
    s <- get
    s' <- lift $ f s
    put s'

maybeState :: StateT s Maybe a -> State s (Maybe a)
maybeState t = state \s -> let
    m = runStateT t s
    a = fst <$> m
    s' = maybe s snd m
    in (a,s')

-- Run a stateful action in IO, using an IORef to store the state.
stateToIO :: (MonadIO io) => IORef s -> State s a -> io a
stateToIO ref s = liftIO do
    before <- readIORef ref
    modifyIORef' ref $ execState s
    return $ evalState s before

-- stateToIO, but flipped, to facilitate eta-reduced functions like
doState :: (MonadIO io) => State s a -> IORef s -> io a
doState = flip stateToIO

-- Access to IO within stateful code manipulating an IORef
statefulIO :: (MonadIO io) => IORef s -> StateT s IO a -> io a
statefulIO ref s = liftIO do
    before <- liftIO $ readIORef ref
    (value, after) <- runStateT s before
    writeIORef ref after
    return value


-- MonadError

hoistExcept :: (MonadError e m) => Except e a -> m a
hoistExcept exc = do
    case runExcept exc of
        Left e  -> throwError e
        Right a -> return a

hoistEither' :: (MonadError e m) => Either e a -> m a
hoistEither' = hoistExcept . hoistEither



-- List

defaultElems :: [a] -> [a] -> [a]
defaultElems xs [] = xs
defaultElems _ ys = ys

defaultElem :: a -> [a] -> [a]
defaultElem = defaultElems . one

-- | Unsafe!
middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

(//) :: (Eq a) => [a] -> a -> [a]
[]     // _ = []
(x:xs) // y | x == y    = xs
          | otherwise = x : (xs // y)



-- Math

minusGt0 :: (Ord n, Num n) => n -> n -> Maybe n
minusGt0 x y = do
    let z = x - y
    guard $ z > 0
    return z

minusGeq0 :: (Ord n, Num n) => n -> n -> Maybe n
minusGeq0 x y = do
    let z = x - y
    guard $ z >= 0
    return z



-- Random

uniforms :: (RandomGen g, Uniform a) => g -> [a]
uniforms = unfoldr (Just . uniform)

mixText :: (RandomGen g) => g -> Text -> Text -> Text
mixText gen = fromString .
    zipWith (?:) (uniforms gen)
    .: T.zip

randElem :: (RandomGen g) => g -> [a] -> a
randElem gen xs = xs !! fst (uniformR (0, length xs - 1) gen)



-- Text

sumText :: (Num n) => Text -> n
sumText = sum . fmap (fromIntegral . fromEnum) . toString



-- Sequence

rotate :: Seq a -> Seq a
rotate Seq.Empty = Seq.Empty
rotate (x:<|xs) = xs|>x

etator :: Seq a -> Seq a
etator Seq.Empty = Seq.Empty
etator (xs:|>x) = x<|xs

rotateTo :: Eq a => a -> Seq a -> Seq a
rotateTo x = uncurry (flip (><)) . Seq.breakl (== x)

etatorTo :: Eq a => a -> Seq a -> Seq a
etatorTo x = uncurry (flip (><)) . Seq.breakr (== x)



-- Servant

type Modify ts a = Get ts a
              :<|> ReqBody ts a :> PostNoContent



-- Tuples

sumPairs :: (Num a, Num b) => [(a, b)] -> (a, b)
sumPairs = foldl' (\(s1, c1) (s2, c2) -> (s1+s2, c1+c2)) (0, 0)