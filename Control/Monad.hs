{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE Safe              #-}

-- |
-- The "Control.Monad" module provides the 'Functor', 'Monad', 'MonadFail', and
-- 'MonadPlus' classes, together with some useful operations on monads.

module Control.Monad (
    -- * Functor and monad classes

      Functor(fmap)
    , Monad((>>=))
    , MonadFail(fail)

    -- * Functions

    -- ** Naming conventions
    -- $naming

    -- ** Basic @Monad@ functions

    , mapM          -- :: (Monad m) => (a -> m b) -> [a] -> m [b]
    , mapM_         -- :: (Monad m) => (a -> m b) -> [a] -> m ()
    , forM          -- :: (Monad m) => [a] -> (a -> m b) -> m [b]
    , forM_         -- :: (Monad m) => [a] -> (a -> m b) -> m ()
    , sequence      -- :: (Monad m) => [m a] -> m [a]
    , sequence_     -- :: (Monad m) => [m a] -> m ()
    , (=<<)         -- :: (Monad m) => (a -> m b) -> m a -> m b
    , (>=>)         -- :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
    , (<=<)         -- :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
    , forever       -- :: (Monad m) => m a -> m b
    , void

    -- ** Generalisations of list functions

    , join          -- :: (Monad m) => m (m a) -> m a
    , filterM       -- :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
    , mapAndUnzipM  -- :: (Monad m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
    , zipWithM      -- :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
    , zipWithM_     -- :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
    , foldM         -- :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
    , foldM_        -- :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m ()
    , replicateM    -- :: (Monad m) => Int -> m a -> m [a]
    , replicateM_   -- :: (Monad m) => Int -> m a -> m ()

    -- ** Conditional execution of monadic expressions

    , guard         -- :: (MonadPlus m) => Bool -> m ()
    , when          -- :: (Monad m) => Bool -> m () -> m ()
    , unless        -- :: (Monad m) => Bool -> m () -> m ()

    -- ** Monadic lifting operators

    , liftM         -- :: (Monad m) => (a -> b) -> (m a -> m b)
    , liftM2        -- :: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)
    , liftM3        -- :: ...
    , liftM4        -- :: ...
    , liftM5        -- :: ...

    , ap            -- :: (Monad m) => m (a -> b) -> m a -> m b

    -- ** Haskell2010 legacy

    , return        -- :: (Monad m) => a -> m a
    , (>>)          -- :: (Monad m) => ma -> m b -> m b

    , MonadPlus
    , mzero     -- :: (MonadPlus m) => m a
    , mplus     -- :: (MonadPlus m) => m a -> m a -> m a
    , msum      -- :: (MonadPlus m) => [m a] -> m a

  ) where

import           "base" Control.Applicative (Alternative (empty, (<|>)),
                                             Applicative (pure, (*>)))
import           "base" Control.Monad       hiding (fail, foldM, foldM_, forM,
                                             forM_, mapM, mapM_, mplus, msum,
                                             mzero, return, sequence, sequence_,
                                             (>>))
import qualified "base" Control.Monad       as CM
import           "base" Control.Monad.Fail



-- | Map each element of a structure to a monadic action, evaluate
-- these actions from left to right, and collect the results. For
-- a version that ignores the results see 'mapM_'.
mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM = CM.mapM

-- | Map each element of a structure to a monadic action, evaluate
-- these actions from left to right, and ignore the results. For a
-- version that doesn't ignore the results see 'mapM'.
mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_ = CM.mapM_

-- | 'forM_' is 'mapM_' with its arguments flipped. For a version that
-- doesn't ignore the results see 'forM'.
forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM = CM.forM

forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ = CM.forM_

-- | Evaluate each monadic action in the structure from left to
-- right, and collect the results. For a version that ignores the
-- results see 'sequence_'.
sequence  :: (Monad m) => [m a] -> m [a]
sequence = CM.sequence

-- | Evaluate each monadic action in the structure from left to right,
-- and ignore the results. For a version that doesn't ignore the
-- results see 'sequence'.
sequence_ :: (Monad m) => [m a] -> m ()
sequence_ = CM.sequence_

-- | The 'foldM' function is analogous to 'foldl', except that its result is
-- encapsulated in a monad. Note that 'foldM' works from left-to-right over
-- the list arguments. This could be an issue where @('>>')@ and the `folded
-- function' are not commutative.
--
-- > foldM f a1 [x1, x2, ..., xm]
-- >
-- > ==
-- >
-- > do
-- >   a2 <- f a1 x1
-- >   a3 <- f a2 x2
-- >   ...
-- >   f am xm
--
-- If right-to-left evaluation is required, the input list should be reversed.
--
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM = CM.foldM

-- | Like 'foldM', but discards the result.
foldM_ :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m ()
foldM_ = CM.foldM_

-- | Inject a value into the monadic type.
--
-- __Note:__ This is a `Monad`-constrained synoym of 'pure' provided
-- for compatibility with Haskell2010.
return :: Monad m => a -> m a
return = pure

infixl 1 >>

-- | Sequentially compose two actions, discarding any value produced
-- by the first, like sequencing operators (such as the semicolon) in
-- imperative languages.
--
-- __Note:__ This is a `Monad`-constrained synoym of '*>' provided for
-- compatibility with Haskell2010.

(>>) :: Monad m => m a -> m b -> m b
(>>) = (*>)


-- | The identity of 'mplus'.
--
-- __Note:__ This is a `MonadPlus`-constrained synoym of 'empty'
-- provided for compatibility with Haskell2010.
mzero :: (MonadPlus m) => m a
mzero = empty

-- | An associative operation.
--
-- __Note:__ This is a `MonadPlus`-constrained synoym of '<|>'
-- provided for compatibility with Haskell2010.
mplus :: (MonadPlus m) => m a -> m a -> m a
mplus = (<|>)

-- | The sum of a collection of actions, generalizing 'concat'.
msum :: (MonadPlus m) => [m a] -> m a
msum = CM.msum

{- $naming

The functions in this library use the following naming conventions:

* A postfix \'@M@\' always stands for a function in the Kleisli category:
  The monad type constructor @m@ is added to function results
  (modulo currying) and nowhere else.  So, for example,

>  filter  ::              (a ->   Bool) -> [a] ->   [a]
>  filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

* A postfix \'@_@\' changes the result type from @(m a)@ to @(m ())@.
  Thus, for example:

>  sequence  :: Monad m => [m a] -> m [a]
>  sequence_ :: Monad m => [m a] -> m ()

* A prefix \'@m@\' generalizes an existing function to a monadic form.
  Thus, for example:

>  sum  :: Num a       => [a]   -> a
>  msum :: MonadPlus m => [m a] -> m a

-}
