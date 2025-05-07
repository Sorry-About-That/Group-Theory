module Group where

import GroupTheory


kleinFourTable :: Table
kleinFourTable = [[0, 1, 2, 3], [1, 0, 3, 2], [2, 3, 0, 1], [3, 2, 1, 0]]

kleinFour :: Group
kleinFour      = Group [0..3] kleinFourTable "V4"

