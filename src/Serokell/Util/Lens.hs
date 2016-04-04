{-# LANGUAGE Rank2Types #-}

-- | Extra operators on Lens
module Serokell.Util.Lens
       ( (%%=)
       , (%?=)
       ) where

import qualified Control.Lens               as L
import           Control.Monad.State        (State, get, runState)
import           Control.Monad.Trans.Except (ExceptT, mapExceptT)

-- I don't know how to call these operators

-- | Similar to %= operator, but takes State action instead of (a -> a)
infix 4 %%=
(%%=) :: L.Lens' s a -> State a b -> State s b
(%%=) l ma =
  do attr <- L.view l <$> get
     let (res, newAttr) = runState ma attr
     l L..= newAttr
     return res

-- | Like %%= but with possiblity of failure
infix 4 %?=
(%?=) :: L.Lens' s a -> ExceptT t (State a) b -> ExceptT t (State s) b
(%?=) l = mapExceptT $ (l %%=)
