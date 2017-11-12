{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- | Utility trace-like functions.

module Serokell.Util.Trace
    ( traceIdF
    ) where

import Universum

import Data.Text (Text)
import Formatting (Format, now, sformat)
import Formatting.Internal (runFormat)

-- | Class for supplying various variable-arguments traces
class VarArgTrace fmt f | fmt -> f where
    traceIdFHelper :: fmt -> f

instance {-# OVERLAPPING #-}
         Print p => VarArgTrace (x -> p) (x -> x) where
    traceIdFHelper fmt x = trace (fmt x) x

instance VarArgTrace fmt f => VarArgTrace (a -> fmt) (a -> f) where
    traceIdFHelper fmt a = traceIdFHelper (fmt a)

-- | Generalized version of `traceShowId`.
-- Accepts formatter as first parameter, and then multiple arguments to print.
--
-- Examples:
--
-- >>> traceIdF F.ords 1
-- 1st
-- 1
--
-- >>> traceIdF ("Item no "%F.int%": "%F.build) 5 True
-- Item no 5: True
-- True
{-# WARNING traceIdF "'traceIdF' remains in code" #-}
traceIdF :: VarArgTrace fmt a => Format Text fmt -> a
traceIdF fmt = traceIdFHelper (runFormat fmt (sformat . now))
