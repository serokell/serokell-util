-- | Common utilities.

module Serokell.Util.Common
       ( enumerate
       ) where

import           Control.Monad.State (evalState, get, modify)

-- | Enumerate function is analogous to python's enumerate. It
-- takes sequences of values and returns sequence of pairs where the
-- first element is index and the second one is corresponding value.
-- It's roughly equivalent to `zip [0..]`.
-- > enumerate "Hello" = [(0,'H'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]
enumerate
    :: (Num i, Enum i, Traversable t)
    => t a -> t (i, a)
enumerate values = evalState action 0
  where
    action = mapM step values
    step v = do
        i <- get
        modify succ
        return (i, v)
