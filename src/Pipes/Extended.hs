{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Pipes.Extended 
  ( module Pipes.Extended, 
    module Pipes.Monoid, 
    module Pipes.Getter, 
    module Pipes
  ) where

import           Control.Lens hiding (Getter, each)
import           Control.Monad
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.Types as Attoparsec
import           Data.Binary (Binary)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Internal (foldrChunks)
import           Data.Foldable (forM_)
import           Data.Map as Map
import           Data.Monoid
import           Data.Tuple (swap)
import           Pipes
import           Pipes.Attoparsec
import qualified Pipes.Binary as PBIN
import qualified Pipes.ByteString as PB
import           Pipes.Getter
import           Pipes.Internal (unsafeHoist)
import           Pipes.Lift (distribute)
import           Pipes.Monoid
import qualified Pipes.Prelude as Pipes
import           Pipes.Safe as X
import           Prelude hiding (readFile, writeFile, lines)
import           System.IO as IO hiding (readFile)

readFile :: FilePath
         -> Producer' ByteString (SafeT IO) ()
readFile file = bracket (openFile file ReadMode) hClose PB.fromHandle
{-# INLINABLE readFile #-}

writeFile :: FilePath
          -> Consumer' ByteString (SafeT IO) ()
writeFile file = bracket (openFile file WriteMode) hClose
                   -- PB.toHandle
                   (\h -> forever $ do
                      a <- await
                      liftIO $ C.hPut h a)
{-# INLINABLE writeFile #-}

{- $verbose -}
readFile' :: FilePath
          -> Producer' ByteString (SafeT IO) ()
readFile' file = bracket
                   (do
                      h <- openFile file ReadMode
                      C.putStrLn $ C.concat ["{", C.pack file, " open}"]
                      return h)
                   (\h -> do
                      hClose h
                      C.putStrLn $ C.concat ["{", C.pack file, " closed}"])
                   PB.fromHandle
{-# INLINABLE readFile' #-}

writeFile' :: FilePath
           -> Consumer' ByteString (SafeT IO) ()
writeFile' file = bracket
                    (do
                       h <- openFile file WriteMode
                       C.putStrLn $ C.concat ["{", C.pack file, " open}"]
                       return h)
                    (\h -> do
                       hClose h
                       C.putStrLn $ C.concat ["{", C.pack file, " closed}"])
                    -- PB.toHandle
                    (\h -> forever $ do
                       a <- await
                       liftIO $ C.hPut h a)
{-# INLINABLE writeFile' #-}

readLines :: FilePath -> Producer ByteString (SafeT IO) ()
readLines file =
  over PB.lines id (readFile file) >-> Pipes.filter (/= "\n")
{-# INLINABLE readLines #-}

{- $unsafe -}
unsafeReadFile :: FilePath
               -> Producer' ByteString IO ()
unsafeReadFile file = do
  h <- lift $ openFile file ReadMode
  PB.fromHandle h
  lift $ hClose h

unsafeWriteFile :: FilePath -> Consumer ByteString IO ()
unsafeWriteFile f = do
  h <- lift $ openFile f WriteMode
  _ <- forever $ do
         a <- await
         liftIO $ C.hPut h a
  lift $ hClose h

-- using hPutBuilder
writeFileLazy :: FilePath
              -> Consumer' Builder (SafeT IO) ()
writeFileLazy file = bracket (openFile file WriteMode) hClose
                       (\h -> forever $ do
                          a <- await
                          liftIO $ hPutBuilder h a)

-- Binary Files
readBinaryFile :: (Binary a) => FilePath -> Producer a (SafeT IO) (Either (PBIN.DecodingError, Producer ByteString (SafeT IO) ()) ())
readBinaryFile file = bracket (openFile file ReadMode) hClose (view PBIN.decoded . PB.fromHandle)

writeBinaryFile :: (Binary a) => FilePath -> Consumer' a (SafeT IO) ()
writeBinaryFile file = bracket (openFile file WriteMode) hClose
                         (\h -> for cat PBIN.encode >-> PB.toHandle h)

readBinaryFileUnsafe :: Binary a => FilePath -> Producer a IO ()
readBinaryFileUnsafe file = do
  h <- lift $ openFile file ReadMode
  (view PBIN.decoded . PB.fromHandle) h
  lift $ hClose h

writeBinaryFileUnsafe :: (Binary a) => FilePath -> Consumer' a IO ()
writeBinaryFileUnsafe file = do
  h <- lift $ openFile file WriteMode
  for cat PBIN.encode >-> PB.toHandle h
  lift $ hClose h

{- $pparse
   parser pipe assuming no errors

   This is typically used with (almost) no-fail Parsers, like 'pLine' where a error catching parser is included to prevent failure.

-}
parseP :: Monad m => AC.Parser a -> Pipe ByteString (Either String a) m r
parseP parser = goread ""
  where
    goread input = do
      r <- await
      goparse (input <> r)
    goreadc cont = do
      r <- await
      handleParseResult $ cont r
    goparse input = handleParseResult $ AC.parse parser input
    handleParseResult parseresult =
      case parseresult of
        (AC.Fail _ _ e) -> do
          yield (Left e)
          goread ""
        (AC.Partial c) -> goreadc c
        (AC.Done t r) -> do
          (yield . Right) r
          if t == ""
            then goread ""
            else goparse t

parseP' :: AC.Parser a -> Proxy () ByteString () (Either String a) IO r
parseP' parser = goread ""
  where
    goread input = do
      r <- await
      goparse (input <> r)
    goreadc cont = do
      r <- await
      handleParseResult $ cont r
    goparse input = handleParseResult $ AC.parse parser input
    handleParseResult parseresult =
      case parseresult of
        (AC.Fail t ctx e) -> do
          lift $ putStrLn $ "parse fail with error " <> e
          lift $ putStrLn $ "context " <> mconcat ctx
          lift $ putStrLn $ "not yet consumed " <> show t
          yield (Left e)
          goread ""
        (AC.Partial c) -> goreadc c
        (AC.Done t r) -> do
          (yield . Right) r
          if t == ""
            then goread ""
            else goparse t

parseOnlyP :: (Monad m) => AC.Parser a -> Pipe ByteString (Either String a) m ()
parseOnlyP parser = Pipes.map (AC.parseOnly parser)

keepGoing :: (Monad m, ParserInput a)
          => Attoparsec.Parser a b
          -> Producer a m r -> Producer b m ()
keepGoing parser p = do
  (x, p') <- lift $ S.runStateT (Pipes.Attoparsec.parse parser) p
  case x of
    Nothing -> return ()
    Just (Left _) -> return ()
    Just (Right b) -> do
      yield b
      keepGoing parser p'

{- $pparse
   parser pipe

   This is typically used with (almost) no-fail Parsers, like where a error catching parser is included to prevent failure.

   The logic checks first for partials that can be completed.  And this then avoids a partial reaching end of input (and thus dying).

-}
parsePipePartial :: (Monad m) => AC.Parser a -> Pipe ByteString (Either String a) m r
parsePipePartial parser = goread ""
  where
    goread input = do
      r <- await
      -- lift $ putStrLn (show r)
      goparse (input <> r)
    goreadc cont = do
      r <- await
      handleParseResult $ cont r
    goparse input = handleParseResult $ AC.parse parser input
    handleParseResult parseresult =
      case parseresult of
        (AC.Fail _ _ e) -> do
          yield (Left e)
          goread ""
        (AC.Partial c) ->
          case c "" of
            (AC.Done t r) -> do
              (yield . Right) r
              if t == ""
                then goread ""
                else goparse t
            _ -> goreadc c
        (AC.Done t r) -> do
          (yield . Right) r
          if t == ""
            then goread ""
            else goparse t

parsePipePartial' :: (Show a) => AC.Parser a -> Pipe ByteString (Either String a) IO r
parsePipePartial' parser = goread ""
  where
    goread input = do
      r <- await
      lift $ putStrLn $ "goread await: " <> show r
      goparse (input <> r)
    goreadc cont = do
      r <- await
      lift $ putStrLn $ "next goreadc await: " <> show r
      handleParseResult $ cont r
    goparse input = handleParseResult $ AC.parse parser input
    handleParseResult parseresult =
      case parseresult of
        (AC.Fail c context e) -> do
          lift $ putStrLn $ "fail: input: " <> show c <> " context: " <> show context <> " error: " <> e
          yield (Left e)
          goread ""
        (AC.Partial c) ->
          case c "" of
            (AC.Done t r) -> do
              lift $ putStrLn $ "Partial yield: " <> show r <> " remaining: " <> show t
              (yield . Right) r
              if t == ""
                then goread ""
                else goparse t
            (AC.Partial _) -> do
              lift $ putStrLn "double partial"
              goreadc c
            (AC.Fail c' context' e') -> do
              lift $ putStrLn $ "partial -> fail: " <> show c' <> " context: " <> show context' <> " error: " <> e'
              goreadc c
        (AC.Done t r) -> do
          lift $ putStrLn $ "Done yield: " <> show r
          (yield . Right) r
          if t == ""
            then goread ""
            else goparse t

-- { error handling } unwrapping `Just`s
justP :: (Monad m) => Pipe (Maybe a) a m r
justP = forever $ do
  a <- await
  Data.Foldable.forM_ a yield

-- passes a Right, discarding Left information
eitherP :: (Monad m) => Pipe (Either a b) b m ()
eitherP = forever $ do
  a <- await
  case a of
    Left _  -> return ()
    Right r -> yield r

-- passes a Right, dealing with Lefts using the supplied function
eitherPM :: (MonadIO m) => (a -> m ()) -> Pipe (Either a b) b m ()
eitherPM f = forever $ do
  a <- await
  case a of
    Left e  -> lift $ f e
    Right r -> yield r

-- fast lazy to strict pipe
toStrictP :: (Monad m) => Pipe L.ByteString ByteString m ()
toStrictP = forever $ do
  a <- await
  foldrChunks (\e a' -> yield e >> a') (return ()) a

-- { $counting }
addCountP :: (Monad m) => Pipe a (Int, a) m ()
addCountP = flip S.evalStateT 0 $ distribute $ do
  lift $ S.put 0
  forever $ do
    s <- await
    lift $ S.modify (+ 1)
    y <- lift S.get
    yield (y, s)

countP :: (Monad m) => Pipe a Int m ()
countP = flip S.evalStateT 0 $ distribute $ forever $ do
  _ <- await
  lift $ S.modify (+ 1)
  y <- lift S.get
  yield y

{- | yield every x awaits
λ> runEffect $ each [1..] >-> modP 100 >-> Pipes.take 2 >-> Pipes.print
1
2
-}
modP :: (Monad m) => Int -> Pipe a a m ()
modP m = flip S.evalStateT 0 $ distribute $ forever $ do
  a <- await
  lift $ S.modify (+ 1)
  n <- lift S.get
  when (m `mod` n == 0) $ yield a

-- count map
countMapP :: (Monad m, Ord a)
          => Producer a m r
          -> m (Map.Map a Int)
countMapP p = Pipes.fold incMap Map.empty id (void p)
  where
    incMap mp k = Map.insertWith (+) k 1 mp

countMapKeyP :: (Monad m, Ord b)
             => Producer a m r
             -> (a -> b)
             -> m (Map.Map b Int)
countMapKeyP p key =
  let fn x a = Map.insertWith (+) (key a) 1 x
  in Pipes.fold fn Map.empty id (void p)

-- ----------------------------------------------------------------------------- branching pipes
dupP :: (Monad m) => Pipe a (a, a) m r
dupP = Pipes.map dup
  where
    dup a = (a, a)

splitP :: (Monad m) => Pipe (a, a) a m r
splitP = forever $ do
  (a1, a2) <- await
  yield a1
  yield a2

firstP :: (Monad m) => Pipe b c m () -> Pipe (b, d) (c, d) m ()
firstP p = flip S.evalStateT undefined $ distribute $ firstSaveP >-> unsafeHoist lift p >-> firstRestoreP

secondP :: (Monad m) => Pipe c d m () -> Pipe (b, c) (b, d) m ()
secondP p = Pipes.map swap >-> firstP p >-> Pipes.map swap

bothP :: (Monad m) => Pipe b c m () -> Pipe b' c' m () -> Pipe (b, b') (c, c') m ()
bothP p1 p2 = firstP p1 >-> secondP p2

zipP :: (Monad m) => Pipe b c m () -> Pipe b c' m () -> Pipe b (c, c') m ()
zipP p1 p2 = dupP >-> firstP p1 >-> secondP p2

zipWithP :: (Monad m) => (c -> c' -> d) -> Pipe b c m () -> Pipe b c' m () -> Pipe b d m ()
zipWithP f p1 p2 = dupP >-> firstP p1 >-> secondP p2 >-> Pipes.map (uncurry f)

dupFirstP :: (Monad m) => Pipe b c m () -> Pipe b (c, b) m ()
dupFirstP p = dupP >-> firstP p

dupSecondP :: (Monad m) => Pipe b c m () -> Pipe b (b, c) m ()
dupSecondP p = dupP >-> secondP p

dupBothP :: (Monad m) => Pipe b c m () -> Pipe b c' m () -> Pipe b (c, c') m ()
dupBothP p1 p2 = dupP >-> bothP p1 p2

fanOutP :: (Monad m) => [Pipe b c m ()] -> Pipe b [c] m ()
fanOutP [p] = p >-> Pipes.map (: [])
fanOutP (p:ps) = dupBothP p (fanOutP ps) >-> Pipes.map (uncurry (:))
fanOutP _ = Pipes.map $ const []

branchP :: (Monad m) => [Pipe b c m ()] -> Pipe b c m ()
branchP l = fanOutP l >-> flatten

firstSaveP :: (Monad m) => Pipe (b, d) b (S.StateT d m) ()
firstSaveP = forever $ do
  (b, d) <- await
  lift $ S.put d
  yield b

firstRestoreP :: (Monad m) => Pipe c (c, d) (S.StateT d m) ()
firstRestoreP = forever $ do
  c <- await
  d <- lift S.get
  yield (c, d)

-- ----------------------------------------------------------------------------- Lens pipes
viewP :: (Monad m) => Lens' a b -> Pipe b c m () -> Pipe a c m ()
viewP l p = Pipes.map (view l) >-> p

viewSetP :: (Monad m) => Lens' a b -> Pipe b b m () -> Pipe a a m ()
viewSetP l p = dupFirstP (viewP l p) >-> Pipes.map (uncurry (set l))

-- debug pipe
debugP :: (MonadIO m, Show a) => String -> Pipe a a m r
debugP s = Pipes.chain (\x -> liftIO $ Prelude.putStrLn $ "debug: " ++ s ++ " " ++ show x)

parseFileP :: AC.Parser a -> FilePath -> Producer (Either String a) (SafeT IO) ()
parseFileP parser file = readFile file >-> parseP parser

-- List pipes
splitListP :: (Monad m) => Pipe [a] a m r
splitListP = forever (await >>= mapM_ yield)

-- Display pipes
displayP :: (MonadIO m, Show a) => Int -> Consumer a m ()
displayP n = Pipes.take n >-> Pipes.print

discardP :: (Monad m) => Consumer a m r
discardP = forever (void await)

-- | Apply a monadic function to the first value of a tuple
mapMFst :: (Monad m) => (a -> m b) -> Pipe (a, c) (b, c) m r
mapMFst f = for cat $ \(a, c) -> do
  b <- lift (f a)
  yield (b, c)

flatten :: (Monad m) => Pipe [a] a m r
flatten = forever $ do
  a <- await
  mapM_ yield a

listP :: (Monad m) => Pipe a [a] m ()
listP = go []
  where
    go l' = do
      a <- await
      let l'' = l' ++ [a]
      yield l''
      go l''

until' :: (Monad m, Eq a) => a -> Pipe a a m ()
until' stop = do
  a <- await
  yield a
  when (a /= stop) (until' stop)

-- | Getter stuff
firstSaveG :: (Monad m) => Producer (b, d) (S.StateT d m) () -> Producer b (S.StateT d m) ()
firstSaveG = loop
  where
    loop p = do
      n <- lift $ next p
      case n of
        Left () -> return ()
        Right ((b, d), p') -> do
          lift $ S.put d
          yield b
          loop p'

firstRestoreG :: (Monad m)
              => Producer c (S.StateT d m) ()
              -> Producer (c, d) (S.StateT d m) ()
firstRestoreG = loop
  where
    loop p = do
      n <- lift $ next p
      case n of
        Left () -> return ()
        Right (c, p') -> do
          d <- lift S.get
          yield (c, d)
          loop p'
