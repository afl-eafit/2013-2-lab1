module EAFIT.CB0081.Data.NFA.Emptyness where

import Data.Set
import EAFIT.CB0081.Data.NFA

-- | 'empty_test' is the start function of the algorithm,
-- it begins the process calling 'reachable_step' with a set
-- only containing the initial state
empty_test :: (Ord a) => NFA a b  -- ^ the machine to analyze
              -> Bool             -- ^ True if is not empty, False if empty
empty_test m = reachable_step m initial_state
  where
    initial_state = singleton $ startNFA m

-- | 'reachable_step'
reachable_step :: (Ord state) =>
                  NFA state symbols -- ^ the machine to analyze
                  -> Set state      -- ^ the set of states of the current step
                  -> Bool           -- ^ True if is not empty, False if empty
reachable_step m last_step
  | some_is_final next_step = True
  | step == last_step = False
  | otherwise =  reachable_step m step
  where
    step = union next_step last_step
    next_step = unions [delta x y | x <- toList last_step, y <- toList sym]
    some_is_final a = intersection a finals /= empty
    delta = deltaNFA m
    sym = symbolsNFA m
    finals = finalsNFA m
