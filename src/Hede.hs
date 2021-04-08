module Hede where

------------------------------------------------------------------------------

import qualified Control.Monad.Base as Base
import Control.Monad.Trans.Control
import Relude
import qualified Snap.Core as Snap
import UnliftIO

------------------------------------------------------------------------------

class Monad m => MonadHede m where
  doHede :: ByteString -> m ()

instance (MonadHede m) => MonadHede (ReaderT e m) where
  doHede t = lift (doHede t)

------------------------------------------------------------------------------
newtype RealHedeT m a = RealHedeT
  { runRealHedeT :: m a
  }
  deriving
    ( Monad,
      Applicative,
      Functor,
      Alternative,
      MonadPlus,
      Base.MonadBase n,
      MonadIO
    )

instance Snap.MonadSnap m => MonadHede (RealHedeT m) where
  doHede t = do
    Snap.writeBS ("hede:" <> t)
    r <- Snap.getResponse
    Snap.finishWith r

instance MonadTrans RealHedeT where
  lift = RealHedeT
  {-# INLINE lift #-}

instance MonadTransControl RealHedeT where
  type StT RealHedeT a = a
  liftWith f = RealHedeT $ f runRealHedeT
  restoreT = RealHedeT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance (MonadUnliftIO m) => MonadUnliftIO (RealHedeT m) where
  withRunInIO inner = RealHedeT $ withRunInIO $ \run -> inner (run . runRealHedeT)

instance Snap.MonadSnap m => Snap.MonadSnap (RealHedeT m) where
  liftSnap snap = RealHedeT (Snap.liftSnap snap)

instance
  (Base.MonadBase IO m, MonadBaseControl IO m) =>
  MonadBaseControl IO (RealHedeT m)
  where
  type StM (RealHedeT m) a = ComposeSt RealHedeT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

------------------------------------------------------------------------------
newtype DryHedeT m a = DryHedeT
  { runDryHedeT :: m a
  }
  deriving
    ( Monad,
      Applicative,
      Functor,
      Alternative,
      MonadPlus,
      Base.MonadBase n,
      MonadIO
    )

instance MonadIO m => MonadHede (DryHedeT m) where
  doHede t = print t

instance MonadTrans DryHedeT where
  lift = DryHedeT
  {-# INLINE lift #-}

instance MonadTransControl DryHedeT where
  type StT DryHedeT a = a
  liftWith f = DryHedeT $ f runDryHedeT
  restoreT = DryHedeT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance (MonadUnliftIO m) => MonadUnliftIO (DryHedeT m) where
  withRunInIO inner = DryHedeT $ withRunInIO $ \run -> inner (run . runDryHedeT)

instance Snap.MonadSnap m => Snap.MonadSnap (DryHedeT m) where
  liftSnap snap = DryHedeT (Snap.liftSnap snap)

instance
  (Base.MonadBase IO m, MonadBaseControl IO m) =>
  MonadBaseControl IO (DryHedeT m)
  where
  type StM (DryHedeT m) a = ComposeSt DryHedeT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM
