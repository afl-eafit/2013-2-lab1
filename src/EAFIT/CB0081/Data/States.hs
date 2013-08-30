module EAFIT.CB0081.Data.States (States(A,B,C,D,E,F)) where

data States = A | B
            | C | D
            | E | F
            deriving (Eq, Ord)
