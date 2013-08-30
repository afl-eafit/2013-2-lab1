module EAFIT.CB0081.Data.NFA.Tests
       (test1, test2, test3, test4, test5, test6)
       where

import EAFIT.CB0081.Data.NFA     ( NFA(MkNFA) )
import EAFIT.CB0081.Data.States  ( States(A,B,C,D,E,F))
import EAFIT.CB0081.Data.Symbols ( Symbols(S1,S2))
import Data.Set                  ( fromList, Set, singleton, empty)


states :: Set States
states = fromList [A,B,C,D,E]

symbols :: Set Symbols
symbols = fromList [S1,S2]

initialState :: States
initialState = A

finalStates :: Set States
finalStates = fromList [E]

errorState :: Set States
errorState = singleton F

-- Nonempty
test1 :: NFA States Symbols
test1 = MkNFA st symbols delta ini final
  where
    delta :: States -> Symbols -> Set States
    delta _ _  = errorState
    st = singleton A
    ini = A
    final = st

-- Nonempty
test2 :: NFA States Symbols
test2 = MkNFA states symbols delta initialState finalStates
  where
    delta :: States -> Symbols -> Set States
    delta A _ = singleton B
    delta B _ = singleton D
    delta D _ = singleton E
    delta E _ = singleton C
    delta C _ = singleton C
    delta _ _ = errorState

-- Empty
test3 :: NFA States Symbols
test3 = MkNFA states symbols delta initialState finalStates
  where
    delta :: States -> Symbols -> Set States
    delta A S1 = singleton B
    delta A S2 = singleton C
    delta B _  = singleton A
    delta C _  = singleton A
    delta D _  = fromList [E,B,C]
    delta E _  = singleton D
    delta _ _  = errorState

-- nonempty
test4 :: NFA States Symbols
test4 = MkNFA states symbols delta initialState finalStates
  where
    delta :: States -> Symbols -> Set States
    delta A _ = fromList [A,B,C,D,E]
    delta B _ = fromList [A,B,C,D,E]
    delta C _ = fromList [A,B,C,D,E]
    delta D _ = fromList [A,B,C,D,E]
    delta E _ = fromList [A,B,C,D,E]
    delta _ _ = errorState

test5 :: NFA States Symbols
test5 = MkNFA states symbols delta initialState finalStates
  where
    delta :: States -> Symbols -> Set States
    delta A _ = fromList [A,B,C,D]
    delta B _ = fromList [A,B,C,D]
    delta C _ = fromList [A,B,C,D]
    delta D _ = fromList [A,B,C,D]
    delta E _ = fromList [A,B,C,D,E]
    delta _ _ = errorState

test6 :: NFA States Symbols
test6 = MkNFA states symbols delta initialState final
  where
    delta :: States -> Symbols -> Set States
    delta A _ = fromList [A,B,C,D,E]
    delta B _ = fromList [A,B,C,D,E]
    delta C _ = fromList [A,B,C,D,E]
    delta D _ = fromList [A,B,C,D,E]
    delta E _ = fromList [A,B,C,D,E]
    delta _ _ = errorState
    final = empty
