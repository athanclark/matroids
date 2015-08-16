{-# LANGUAGE
    StandaloneDeriving
  , FlexibleContexts
  , UndecidableInstances
  #-}

module Data.Matroid where

import Data.Maybe (fromJust, isNothing)
import Data.Set.Class as Sets
import Data.Set.Ordered.Many.With
import qualified Data.Map as Map

import Data.Commutative
import Data.Foldable as Fold
import Control.Monad.Reader
import Control.Monad.State


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
  return $ s `Sets.union` d

-- | Returns the size of the maximum basis that's a subset of the input.
rank :: ( MonadReader (Matroid c a) m
        , HasIntersection (c a)
        , HasSize (c a)
        , HasEmpty (c a)
        , HasUnion (c a)
        , Fold.Foldable c
        ) => c a -> m Int
rank s = do
  b <- (Fold.foldr Sets.union Sets.empty . bases) <$> ask
  return $ Sets.size $ s `Sets.intersection` b

-- * Creation

mkMatroid :: ( Fold.Foldable c
             , HasEmpty (c a)
             , HasDifference (c a)
             , HasUnion (c a)
             ) => c a -> c (c a) -> Matroid c a
mkMatroid e bs = let b = Sets.unions bs in
  Matroid bs $ e `Sets.difference` b

-- * SubMatroids and SuperMatroids

shrink :: ( HasIntersection (c a)
          , Functor c
          ) => Matroid c a
            -> c a -- Smaller ground set
            -> Matroid c a
shrink (Matroid bs ds) sub = Matroid ((`Sets.intersection` sub) <$> bs) $
                                ds `Sets.intersection` sub

grow :: ( HasUnion (c a)
        ) => Matroid c a
          -> c a -- New ground set
          -> c (c a) -- New bases
          -> Matroid c a
grow (Matroid _ ds) sup bs' = Matroid bs' $ ds `Sets.union` sup

-- * Greedy Algorithm

-- | Dummy class to document that @<~>@ should /increase/ strictly positive.
class Commutative c => CommutativePositive c where

takeMaximum :: ( Foldable c
               , Eq (c a)
               , Ord a
               , Ord k
               , Sets.HasEmpty (c a)
               , Sets.HasDelete a (c a)
               ) => SetsWith k c a -> (Maybe a, SetsWith k c a)
takeMaximum xss@(SetsWith (f,xs)) = if Map.size xs > 0
  then let (k, set) = Map.findMax xs
           x = Fold.maximum set

           xs' = Map.alter go k xs
           go Nothing = Nothing
           go (Just set') = if Sets.delete x set' == Sets.empty
                            then Nothing
                            else Just $ Sets.delete x set'
       in (Just x, SetsWith (f,xs'))
  else (Nothing, xss)

pivot :: ( MonadState (SetsWith w c a) m
         , MonadReader (Matroid c a) m
         , CommutativePositive w
         , Foldable c
         , Eq (c a)
         , Ord a
         , Ord w
         , Sets.HasEmpty (c a)
         , Sets.HasDelete a (c a)
         , Sets.HasInsert a (c a)
         , Sets.CanBeSubset (c a)
         ) => c a -> m (c a)
pivot old = do
  temp <- get
  let (mx,temp') = takeMaximum temp
  put temp'
  bs <- bases <$> ask
  if any (Sets.insert (fromJust mx) old `Sets.isSubsetOf`) bs
  then return $ Sets.insert (fromJust mx) old
  else return old

--
-- greedy :: ( MonadReader (Matroid c a) m
--           , CommutativePositive w
--           , Foldable c
--           , Ord a
--           , HasEmpty (c a)
--           , HasDifference (c a)
--           ) => (a -> w) -> m
