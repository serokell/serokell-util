{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Extra operators on Lens
module Serokell.Util.Lens
       ( (%%=)
       , (%?=)
       , zoom'
       , magnify'
       ) where

import Universum

import Control.Monad.Identity (Identity)
import Control.Monad.Trans.Except (ExceptT, mapExceptT)
import Lens.Micro.Mtl ((.=))

import Lens.Micro as L
import Lens.Micro.Mtl as LM
import Lens.Micro.Mtl.Internal as LMI

-- I don't know how to call these operators

-- | Similar to %= operator, but takes State action instead of (a -> a)
infix 4 %%=
(%%=) :: L.Lens' s a -> State a b -> State s b
(%%=) l ma = do
    attr <- LM.view l <$> get
    let (res,newAttr) = runState ma attr
    l .= newAttr
    return res

-- | Like %%= but with possiblity of failure
infix 4 %?=
(%?=) :: L.Lens' s a -> ExceptT t (State a) b -> ExceptT t (State s) b
(%?=) l = mapExceptT (l %%=)

-- | A 'zoom' which works in arbitrary 'MonadState'.
--
-- See <https://github.com/ekmett/lens/issues/580>. You might be surprised
-- but actual 'zoom' doesn't work in any 'MonadState', it only works in a
-- handful of state monads and their combinations defined by 'Zoom'.
zoom'
    :: MonadState s m
    => L.LensLike' (LMI.Zoomed (State s) a) s t
    -> StateT t Identity a
    -> m a
zoom' l = state . runState . LM.zoom l

-- | A 'magnify' which works in arbitrary 'MonadReader'.
magnify'
    :: MonadReader s m
    => L.LensLike' (LMI.Magnified (Reader s) a) s t
    -> ReaderT t Identity a
    -> m a
magnify' l = reader . runReader . LM.magnify l

-- | This isomorphism can be used to convert to or from an instance of 'IsList'.
--
-- Note that this function is quite general but doesn't allow to switch
-- container - in most cases such behavious eliminates need in specifing
-- container type manually.
