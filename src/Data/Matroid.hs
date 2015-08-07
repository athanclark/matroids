{-# LANGUAGE
    StandaloneDeriving
  , FlexibleContexts
  , UndecidableInstances
  #-}

module Data.Matroid where

import Data.Set.Class
import Data.Commutative
import Data.Foldable as Fold
import Control.Monad.Reader


data Matroid c a = Matroid
  { bases :: c (c a) -- ^ The maximal independent sets (of all the same size)
  , dependents :: c a -- ^ The residual dependent elements
  }

deriving instance (Show (c a), Show (c (c a))) => Show (Matroid c a)
deriving instance (Eq (c a), Eq (c (c a))) => Eq (Matroid c a)


-- | Includes all spans (dependent elements) while keeping the same rank.
closure :: ( MonadReader (Matroid c a) m
           , HasUnion (c a)
           ) => c a -> m (c a)
closure s = do
  d <- dependents <$> ask
  return $ s `union` d

-- | Returns the size of the maximum basis that's a subset of the input.
rank :: ( MonadReader (Matroid c a) m
        , HasIntersection (c a)
        , HasSize (c a)
        , HasEmpty (c a)
        , HasUnion (c a)
        , Fold.Foldable c
        ) => c a -> m Int
rank s = do
  b <- (foldr union empty . bases) <$> ask
  return $ size $ s `intersection` b

-- * Creation

mkMatroid :: ( Fold.Foldable c
             , HasEmpty (c a)
             , HasDifference (c a)
             , HasUnion (c a)
             ) => c a -> c (c a) -> Matroid c a
mkMatroid e bs = let b = unions bs in
  Matroid bs $ e `difference` b

-- * SubMatroids and SuperMatroids

shrink :: ( HasIntersection (c a)
          , Functor c
          ) => Matroid c a
            -> c a -- Smaller ground set
            -> Matroid c a
shrink (Matroid bs ds) sub = Matroid ((`intersection` sub) <$> bs) $
                                ds `intersection` sub

grow :: ( HasUnion (c a)
        ) => Matroid c a
          -> c a -- New ground set
          -> c (c a) -- New bases
          -> Matroid c a
grow (Matroid _ ds) sup bs' = Matroid bs' $ ds `union` sup

-- * Greedy Algorithm

-- | Dummy class to document that @<~>@ should /increase/ strictly positive.
class Commutative c => CommutativePositive c where

greedy :: ( MonadReader (Matroid c a) m
          , CommutativePositive w
          , Foldable c
          , Ord a
          ) => (a -> w) -> m
