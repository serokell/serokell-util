-- | Compile time assertions
-- Very simple implementation from: http://stackoverflow.com/a/6654903
-- TODO: understand and maybe improve
module Serokell.Util.StaticAssert
       ( staticAssert
       ) where

import Universum hiding (fail)

import Control.Monad (fail)
import Language.Haskell.TH (Q)

staticAssert :: Bool -> String -> Q [a]
staticAssert cond mesg = do
    unless cond $ fail $ "Compile time assertion failed: " ++ mesg
    return [] -- No need to make a dummy declaration
