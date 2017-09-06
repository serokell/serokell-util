{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Extra operators on Lens
module Serokell.Util.Lens
       ( (%%=)
       , (%?=)
       , WrappedM (..)
       , _UnwrappedM
       , zoom'
       , magnify'
       , listL
       ) where

import qualified Control.Lens               as L
import           Control.Monad.Reader       (MonadReader, Reader, ReaderT, reader,
                                             runReader)
import           Control.Monad.State        (MonadState, State, StateT, get, runState,
                                             state)
import           Control.Monad.Trans.Except (ExceptT, mapExceptT)
import           GHC.Exts                   (IsList (..))
import           System.Wlog                (LoggerName, LoggerNameBox (..))

-- I don't know how to call these operators

-- | Similar to %= operator, but takes State action instead of (a -> a)
infix 4 %%=
(%%=) :: L.Lens' s a -> State a b -> State s b
(%%=) l ma = do
    attr <- L.view l <$> get
    let (res,newAttr) = runState ma attr
    l L..= newAttr
    return res

-- | Like %%= but with possiblity of failure
infix 4 %?=
(%?=) :: L.Lens' s a -> ExceptT t (State a) b -> ExceptT t (State s) b
(%?=) l = mapExceptT (l %%=)

-- | Similar to `Wrapped`, but for `Monad`s.
class Monad m => WrappedM m where
    type UnwrappedM m :: * -> *

    _WrappedM :: L.Iso' (m a) (UnwrappedM m a)
    _WrappedM = L.iso packM unpackM

    packM :: m a -> UnwrappedM m a
    packM = L.view _WrappedM

    unpackM :: UnwrappedM m a -> m a
    unpackM = L.view _UnwrappedM

_UnwrappedM :: WrappedM m => L.Iso' (UnwrappedM m a) (m a)
_UnwrappedM = L.from _WrappedM

instance Monad m => WrappedM (LoggerNameBox m) where
    type UnwrappedM (LoggerNameBox m) = ReaderT LoggerName m
    _WrappedM = L.iso loggerNameBoxEntry LoggerNameBox

-- | A 'zoom' which works in arbitrary 'MonadState'.
--
-- See <https://github.com/ekmett/lens/issues/580>. You might be surprised
-- but actual 'zoom' doesn't work in any 'MonadState', it only works in a
-- handful of state monads and their combinations defined by 'Zoom'.
zoom'
    :: MonadState s m
    => L.LensLike' (L.Zoomed (State s) a) s t
    -> StateT t L.Identity a
    -> m a
zoom' l = state . runState . L.zoom l

-- | A 'magnify' which works in arbitrary 'MonadReader'.
magnify'
    :: MonadReader s m
    => L.LensLike' (L.Magnified (Reader s) a) s t
    -> ReaderT t L.Identity a
    -> m a
magnify' l = reader . runReader . L.magnify l

-- | This isomorphism can be used to convert to or from an instance of 'IsList'.
listL :: IsList l => L.Iso' l [Item l]
listL = L.iso toList fromList
