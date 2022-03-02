module Util where

import Control.Monad.Except
import Data.Maybe.HT
import GHC.Data.Maybe
import Relude
import Servant
import System.Console.Haskeline
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|), (:|>)), (<|), (|>), (><))



-- Haskeline

type Check a msg = (a -> Maybe msg)

untilValidAnd :: (Read a, MonadIO m) => Check a String -> InputT m (Maybe String) -> InputT m a
untilValidAnd chk getInput = do
    ln <- getInput
    case readMaybe =<< ln of
        Nothing -> do
            outputStrLn "Invalid input."
            untilValid getInput
        Just a -> case chk a of
            Just msg -> do
                outputStrLn msg
                untilValidAnd chk getInput
            Nothing -> return a

untilValid :: (Read a, MonadIO m) => InputT m (Maybe String) -> InputT m a
untilValid = untilValidAnd (const Nothing)

check :: msg -> (a -> Bool) -> Check a msg
check msg p a = toMaybe (not $ p a) msg

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
-- floop :: (MonadIO io) => IORef s -> io a
-- floop = doState do ...
doState :: (MonadIO io) => State s a -> IORef s -> io a
doState = flip stateToIO



-- MonadError

hoistExcept :: (MonadError e m) => Except e a -> m a
hoistExcept exc = do
    case runExcept exc of
        Left e  -> throwError e
        Right a -> return a

hoistEither' :: (MonadError e m) => Either e a -> m a
hoistEither' = hoistExcept . hoistEither



-- List

defaultElem :: a -> [a] -> [a]
defaultElem x [] = [x]
defaultElem _ xs = xs



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