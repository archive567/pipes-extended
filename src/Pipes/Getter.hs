{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Pipes.Getter where

import           Pipes.Monoid

import           Control.Category (Category(..))
import           Control.Monad
import           Data.Functor.Contravariant
import           Pipes
import qualified Pipes.Prelude as Pipes
import           Prelude hiding ((.),id)

newtype Getter m a b = Getter { runGetter :: Monad m => Producer a m () -> Producer b m () }

instance Functor (Getter m a) where
    fmap f (Getter g) = Getter (map' f . g)

instance Category (Getter m) where
    (Getter g0) . (Getter g1) = Getter (g0 . g1)

    id = Getter id

{- experiment at getting a Monoid instance
instance (Monad m) => Monoid (Getter m b a) where
  mempty = Getter (>-> forever await)
  -- mappend (Getter g0) (Getter g1) = Getter (ma' g0 g1)
-}

-- thread a single getter into the stack.  Should be able to reason this down to ($)!
simpleGetter :: ( Monad m) =>
         (Producer a m r -> Producer b m r) ->
          Producer a m r ->
          Producer b m r
simpleGetter g p0 = runEffect $ hoist lift (g p0) >-> catUp

route' ::
    Monad m =>
    Producer a (Producer a (Producer a m)) r ->
    Producer a (Producer a m) r
route' = go
  where
    go p = do
      n <- next p
      case n of
          Left r -> return r
          Right (x,p') -> do
              yield x
              lift $ yield x
              go p'

-- lifting the initial Producer
stepA ::
    Monad m =>
    Producer a m r ->
    Producer a (Producer a m) r
stepA p = route' ((hoist lift . hoist lift) p)

-- applying the first getter
stepB ::
    Monad m =>
    (Producer a (Producer a m) r -> Producer b (Producer b m) r) ->
    Producer a m r ->
    Producer b (Producer b m) r
stepB g0 p = g0 (stepA p)

-- sending the first getter up the stack and closing the level off
stepC ::
    Monad m =>
    (Producer a (Producer a m) r -> Producer b (Producer b m) r) ->
    Producer a m r ->
    Producer b m r
stepC g0 p = runEffect $ stepB g0 p >-> catUp 

-- hooking the second up, cat'ting up, and closing the level 
stepD ::
    ( Monad m1
    , Monad m) =>
    (Producer a (Producer a (Proxy x' x () b1 m)) r1 ->
     Producer b1 (Producer b1 (Proxy x' x () b1 m)) r1) ->
    (Producer b1 (Proxy x' x () b1 m) r1 -> Proxy X () () b (Proxy x'1 x1 () b m1) r) ->
    Producer a (Proxy x' x () b1 m) r1 ->
    Proxy x'1 x1 () b m1 r
stepD g0 g1 p = runEffect (g1 (stepC g0 p) >-> catUp)

{- the raw type that comes out of GHC

   strange types for the getters ...

   but it works:

   λ> Pipes.toList $ mappend' (take' 3 >>> (>-> doubler)) (map'(100+)) (each [1..5])
   [1,1,101,2,2,102,3,3]


-}
mappend' ::
    ( MonadTrans t
    , Monad (t (Proxy x' x () b1 m1))
    , Monad m2
    , Monad m1
    , Monad m) =>
    ( Producer a (Producer a m) r2 -> Producer b1 (t (Proxy x' x () b1 m1)) r1) ->
    (t (Proxy x' x () b1 m1) r1-> Proxy X () () b (Proxy x'1 x1 () b m2) r) ->
    Producer a m r2 ->
    Proxy x'1 x1 () b m2 r
mappend' g0 g1 p =
    runEffect
    (g1
     (runEffect
      (g0 (route' ((hoist lift . hoist lift) p))
       >-> catUp2)) 
     >-> catUp)

-- some simple examples for testing
take' :: Monad m => Int -> Producer a m () -> Producer a m ()
take' n = (<-<) (Pipes.take n)

map' :: Monad m => (a -> b) -> Producer a m r -> Producer b m r
map' f = (<-<) (Pipes.map f)

take'' :: Int -> Getter m a a
take'' n = Getter (take' n)

map'' :: (a -> b) -> Getter m a b
map'' f = Getter (map' f)

toGetter :: Monad m => Pipe a b m () -> Getter m a b
toGetter pipe = Getter (>-> pipe)

toGetter' :: Monad m => Pipe a b m r -> Producer a m r -> Producer b m r
toGetter' = (<-<)

{- some other type experiments -}
newtype Getter' m b a = Getter' { getter' :: Monad m => Producer a m () -> Producer b m () }

instance Contravariant (Getter' m b) where
    contramap f (Getter' g) = Getter' (g . map' f)

{-
instance Applicative (Getter m a) where
    pure b = Getter (const $ forever $ yield b)
    -- (Getter f) <*> (Getter x) = Getter (map' f . x) 
-}

unit :: Getter m a ()
unit = Getter (const $ forever $ yield ())

{-

failed Applicative attempt

(**) :: (Monad m) => (Getter m a b, Getter m a c) -> Getter m a (b,c)
(**) (Getter g0, Getter g1) = Getter (\p -> zip (g0 p) (g1 p))
  where
    zip p0 p1 = runEffect $ hoist (runEffect . hoist runEffect) p0 >-> go

    -- go :: (Monad m) => Consumer a0 (Consumer a1 (Producer (a0, a1) m)) r
    go :: (Monad m0) => Proxy () t y' y (Proxy () t1 y'1 y1 (Proxy x' x () (t, t1) m0)) b1
    go = do
        a0 <- await
        lift $ do
            a1 <- await
            lift $ yield (a0,a1)
        go
-}


