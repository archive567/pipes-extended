{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pipes.Monoid where

import Control.Category (Category(..))
import Control.Monad
import Data.Monoid
import Pipes
import Prelude hiding ((.),id)

{- helpers for moving up and down the transformer stack

Up and Down may well be counter-intuitive

These helpers are always an await followed by a yield (hence the cat prefix)
Just as a convention, I take the `lift` metaphor as up, thus an await, followed by a lifted yield seems to be in an upwards direction, and vice-versa.

The type system doesnt like "split" synonyms, so I have annotated where polymorphic synonym is happening in comments

-}
-- Prod' a (Cons' a)
catDown ::
    Monad m =>
    Producer' a (Proxy () a y' y m) r
catDown = forever $ lift await >>= yield

-- Prod' a (t (Cons' a))
catDown2 ::
    ( Monad (t (Proxy () a y' y m))
    , Monad m
    , MonadTrans t) =>
    Producer' a (t (Proxy () a y' y m)) r
catDown2 = forever $ (lift . lift $ await) >>= yield

-- Prod' a (t (t1 (Cons' a)))
catDown3 ::
    ( Monad (t (t1 (Proxy () a y' y m)))
    , Monad (t1 (Proxy () a y' y m))
    , Monad m
    , MonadTrans t
    , MonadTrans t1) =>
    Producer' a (t (t1 (Proxy () a y' y m))) r
catDown3 = forever $ (lift . lift . lift $ await) >>= yield

-- Consumer' a (Producer' a) 
catUp ::
    Monad m =>
    Proxy () a y' y (Proxy x' x () a m) r
catUp = forever $ await >>= lift . yield

-- Consumer' a (t (Producer' a))
catUp2 ::
    ( Monad (t (Proxy x' x () b m))
    , Monad m
    , MonadTrans t) =>
    Proxy () b y' y (t (Proxy x' x () b m)) r
catUp2 = forever $ await >>= (lift . lift . yield)

-- Consumer' a (t (t1 (Producer' a)))
catUp3 ::
    ( Monad (t (t1 (Proxy x' x () a m)))
    , Monad (t1 (Proxy x' x () a m))
    , Monad m
    , MonadTrans t
    , MonadTrans t1) =>
    Proxy () a y' y (t (t1 (Proxy x' x () a m))) b
catUp3 = forever $ await >>= (lift . lift . lift . yield)


{-

The Davorak Pipe Monoid method for reference

λ> Pipes.toList $ (each [0..]) >-> Pipes.take 10 >-> (Pipes.map (^2) <> Pipes.take 5)
[0,0,1,1,4,2,9,3,16,4]

(<>) is strictly ordered, and the entire pipe closes down if one of the pipes does.

λ> Pipes.toList $ (pd' (Pipes.take 3) (Pipes.map (+10))) <-< each [1..10]
[1,11,2,12,3]

-}

doubler :: Monad m => Pipe a a m r
doubler = forever $ await >>= \x -> replicateM_ 2 (yield x)

routeCycle :: Monad m => Consumer a (Producer a (Producer a m)) b
routeCycle = goF
  where
    goF = do
      x <- await
      lift (yield x)
      goS
    goS = do
      x <- await
      (lift . lift) $ yield x
      goF

maPipe :: Monad m => Pipe a b m r -> Pipe a b m r -> Pipe a b m r
maPipe x0 x1 =
    runEffect
    (runEffect
     (runEffect (catDown3 >-> doubler >-> routeCycle)
      >-> (hoist lift . hoist lift) x0
      >-> catUp2
     )
     >-> hoist lift x1
     >-> catUp)

instance (Monad m) => Monoid (Pipe a b m r) where
  mempty = for cat discard
  mappend = maPipe

ma :: (Monad m1) => Pipe a b m1 r -> Pipe a b m1 r -> Pipe a b m1 r
ma = maPipe

me :: (Monad m1) => Pipe a b m1 r
me = for cat discard

{-

deconstruction of the Davorak example for pipes

-}
-- doubler awaits an a and yields twice
step1 :: (Monad m) => Pipe a a m r
step1 = doubler

-- to feed into the route
-- So we have a Consumer of a's and 2 producers in a three level transformer stack
step2 :: (Monad m) => Consumer a (Producer a (Producer a m)) r
step2 = doubler >-> routeCycle

-- lift3AwaitYield provides a Producer which is connected to the top level Consumer that is the doubler/router.  This Producer is matched to a polymorphic Consumer' layer (the lift x 3 await) which becomes an extra level.
step3 :: (Monad m) => Effect (Producer a (Producer a (Proxy () a y' y m))) r
step3 = catDown3 >-> doubler >-> routeCycle

-- collapsing/fusing the top-level Effect (A pipe between the Router and the lowest level Consumer) gives us 3 levels in the stack.
-- 1. A Producer of a's
-- 2. Another Producer of a's (which are the exact same a's at the first layer)
-- 3. A Consumer' (which can also be filled by a pipe)

-- Prod a (Prod a (Cons' a))
step4 :: (Monad m) => Producer a (Producer a (Proxy () a y' y m)) r
step4 = runEffect (catDown3 >-> doubler >-> routeCycle)

hl2 ::
    ( MFunctor t
    , MonadTrans t2
    , MonadTrans t1
    , Monad (t2 m)
    , Monad m) => t m r -> t (t1 (t2 m)) r
hl2 = hoist lift . hoist lift

hl2Pipe ::
    ( MonadTrans t2
    , MonadTrans t1
    , Monad (t2 m)
    , Monad m) => Pipe a b m r -> Pipe a b (t1 (t2 m)) r
hl2Pipe = hoist lift . hoist lift

-- hl2 x >-> al2y ~ Pipe a b (t (t m)) >-> Cons' b (t (Prod' b m)) 
-- Cons' a (t (Prod' b m))
step5 ::
    ( MonadTrans t1
    , Monad (t1 (Proxy x' x () b m))
    , Monad m) =>
    Pipe a b m r ->
    Proxy () a c' c (t1 (Proxy x' x () b m)) r
step5 x = hl2Pipe x >-> catUp2

-- between step5 and step6 we have altered the dev pipe from a Producer a (Producer a (Consumer' a) to a Producer b (Producer a (Consumer' a).  This is the outgoing part of the new pipe being constructed.


{-

Connecting up step4 and step5 involves the resolution of the respective inner stacks of Consumer' a and Producer' b into a Pipe a b (!).  Highly suggestive.

-}

-- Prod a (Prod a (Cons' a)) >-> Cons' a (t (Prod' b m))
-- Prod a (Pipe a b m)
step6 ::
    Monad m =>
    Pipe a b m r ->
    Producer a (Pipe a b m) r
step6 x = runEffect $ step4 >-> step5 x

-- Prod a (Pipe a b) >-> Pipe a b (t) >-> Cons' b (Prod' b) ~ Effect (Pipe a b)
step7 ::
    Monad m =>
    Pipe a b m r ->
    Pipe a b m r ->
    Pipe a b m r
step7 x x' = runEffect $ step6 x >-> hoist lift x' >-> catUp
