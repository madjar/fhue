-- | This module contains functions used to nicely display things to the user.

module FHue.Display where

import           FHue.Types
import           Text.PrettyPrint.Boxes


-- | Print a list of items the way you would expect `ls` to.
printItems :: [Item] -> IO ()
printItems is = printBox . hsep 2  center1 $ allCols
  where colFuncs = [ itemRwx
                   , itemUser
                   , itemGroup
                   , show . itemSize
                   , itemMtime
                   , itemName ]
        makeCol f = vcat right $ map (text . f) is
        allCols =  map makeCol colFuncs
