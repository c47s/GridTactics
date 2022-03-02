module GridTactics
    ( module GridTactics
    , productName
    ) where

import API as GridTactics
import Mechanics as GridTactics
import SeqWorld as GridTactics
import Util as GridTactics

import Relude

productName :: IsString a => a
productName = "GridTactics"