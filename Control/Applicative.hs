{-# LANGUAGE PackageImports #-}

-- | This module describes a structure intermediate between a 'Functor' and
-- a 'Monad' (technically, a strong lax monoidal functor).
-- Compared with 'Monad', the 'Applicative' interface lacks the full power of
-- the binding operation '>>=', but
--
-- * it has more instances,
--
-- * it is sufficient for many uses such as context-free parsing, and
--
-- * instances can perform analysis of computations before they are
--   executed, and thus produce shared optimizations.
--
module Control.Applicative (
  -- * Applicative functors
  Applicative(pure, liftA2, (<*>), (*>), (<*)),

  -- * Alternatives
  Alternative(empty, (<|>), some, many),

  -- * Utility functions
  (<**>),  liftA,  liftA3,  optional

  ) where

import           "base" Control.Applicative
