{-# LANGUAGE TypeFamilies, UndecidableInstances, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification, DefaultSignatures #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
module Control.Monad.Runnable where

import Import
import qualified Control.Monad.Trans.State.Strict as SS
import qualified Control.Monad.Trans.State.Lazy as LS
import qualified Control.Monad.Trans.Writer.Strict as SW
import qualified Control.Monad.Trans.Writer.Lazy as LW
import qualified Control.Monad.Trans.RWS.Strict as SR
import qualified Control.Monad.Trans.RWS.Lazy as LR
-- import Control.Monad.Trans.Cont -- may be impossible to write

-- | A class of monads that have a run function.
--
--   The 'runMonad' function gives the result inside of IO. The only reason for this is to allow
--   an instance for IO to be written. Other instances do not perform any aditional IO.
--
--   Instances for 'Identity', 'IO' and
--   @('Runnable' m, 'RunnableTrans' t, 'Monad' (t m)) => 'Runnable' (t m)@ are given so users
--   should only provide additional 'RunnableTrans' instances instead of 'Runnable' ones.
class Monad m => Runnable m where
    -- | The type of value that needs to be provided to run this monad.
    type MonadicState m :: *
    -- | The type of the result you get when you run this monad.
    type MonadicResult m a :: *
    -- | Get the current state value.
    currentMonadicState :: m (MonadicState m)
    -- | If given a result, reconstruct a monadic compitation.
    restoreMonadicState :: MonadicResult m a -> m a
    -- | Given the required state value and a computation, run the computation up to the IO effect.
    --   This should effectively run each layer in the transformer stack. The 'MonadicState' should
    --   hold all the needed information to do so.
    --
    --   A more formal description of what it means to run a transformer is given for the
    --   'runTransformer' function.
    runMonad :: MonadicState m -> m a -> IO (MonadicResult m a)

    default runMonad :: PureRunnable m => MonadicState m -> m a -> IO (MonadicResult m a)
    runMonad s m = return (runPureMonad s m)

class Runnable m => PureRunnable m where
    runPureMonad :: MonadicState m -> m a -> MonadicResult m a

-- | A class of transformers that can run their effects in the underlying monad.
--
-- The following laws need to hold:
--
-- [1]
--     Running a computation that only uses the effects /of the transformer/ (represented here
--     by stating that the computation is polymorphic in the underlying monad) using the current
--     state, and then restoring the result is the same as doing nothing.
--
--     @
--     t :: forall m. Monad m => t m a
--     t == (currentTransState >>= lift . runTransformer t >>= restoreTransState)
--     @
--
-- [2]
--     Running a computation that only uses the effects /of the underlying monad/ (represented here
--     by stating that the computation is polymorphic in the transformer) using /any/ state, and
--     then restoring the result is the same as doing nothing.
--
--     @
--     t :: forall t. MonadTrans t => t m a -> t m a
--     t == (lift (runTransformer t s) >>= restoreTransState)
--     @
class MonadTrans t => RunnableTrans t where
    -- | The type of value that needs to be provided to run this transformer.
    type TransformerState t (m :: * -> *) :: *
    -- | The type of the result you get when you run this transformer.
    type TransformerResult t a :: *
    -- | Get the current state value.
    currentTransState :: Monad m => t m (TransformerState t m)
    -- | Given a result, interpret it as a computation. This restores the state of the transformer.
    restoreTransState :: Monad m => TransformerResult t a -> t m a
    -- | Given the required state value and a computation, run the effects of the transformer
    --   in the underlying monad.
    runTransformer :: Monad m => t m a -> TransformerState t m -> m (TransformerResult t a)

instance Runnable Identity where
    type MonadicState Identity = ()
    type MonadicResult Identity a = a
    currentMonadicState = return ()
    restoreMonadicState = return

instance PureRunnable Identity where
    runPureMonad _ (Identity a) = a

instance Runnable IO where
    type MonadicState IO = ()
    type MonadicResult IO a = a
    currentMonadicState = return ()
    restoreMonadicState = return
    runMonad _ m = m

instance (Runnable m, RunnableTrans t, Monad (t m)) => Runnable (t m) where
    type MonadicState (t m) = (TransformerState t m, MonadicState m)
    type MonadicResult (t m) a = MonadicResult m (TransformerResult t a)
    currentMonadicState = (,) <$> currentTransState <*> lift currentMonadicState
    restoreMonadicState s = lift (restoreMonadicState s) >>= restoreTransState
    runMonad (s, s') t = runMonad s' (runTransformer t s)

instance (PureRunnable m, RunnableTrans t, Monad (t m)) => PureRunnable (t m) where
    runPureMonad (s, s') t = runPureMonad s' (runTransformer t s)

instance RunnableTrans (SS.StateT s) where
    type TransformerState (SS.StateT s) m = s
    type TransformerResult (SS.StateT s) a = (a, s)
    currentTransState = get
    restoreTransState (a, s) = put s >> return a
    runTransformer = SS.runStateT

instance RunnableTrans (LS.StateT s) where
    type TransformerState (LS.StateT s) m = s
    type TransformerResult (LS.StateT s) a = (a, s)
    currentTransState = get
    restoreTransState (a, s) = put s >> return a
    runTransformer = LS.runStateT

instance Monoid s => RunnableTrans (SW.WriterT s) where
    type TransformerState (SW.WriterT s) m = ()
    type TransformerResult (SW.WriterT s) a = (a, s)
    currentTransState = return ()
    restoreTransState (a, s) = SW.tell s >> return a
    runTransformer m _ = SW.runWriterT m

instance Monoid s => RunnableTrans (LW.WriterT s) where
    type TransformerState (LW.WriterT s) m = ()
    type TransformerResult (LW.WriterT s) a = (a, s)
    currentTransState = return ()
    restoreTransState (a, s) = LW.tell s >> return a
    runTransformer m _ = LW.runWriterT m

instance RunnableTrans (ReaderT s) where
    type TransformerState (ReaderT s) m = s
    type TransformerResult (ReaderT s) a = a
    currentTransState = ask
    restoreTransState = return
    runTransformer = runReaderT

instance Monoid w => RunnableTrans (SR.RWST r w s) where
    type TransformerState (SR.RWST r w s) m = (r, s)
    type TransformerResult (SR.RWST r w s) a = (a, s, w)
    currentTransState = (,) <$> ask <*> get
    restoreTransState (a, s, w) = SR.tell w >> put s >> return a
    runTransformer m (r, s) = SR.runRWST m r s

instance Monoid w => RunnableTrans (LR.RWST r w s) where
    type TransformerState (LR.RWST r w s) m = (r, s)
    type TransformerResult (LR.RWST r w s) a = (a, s, w)
    currentTransState = (,) <$> ask <*> get
    restoreTransState (a, s, w) = LR.tell w >> put s >> return a
    runTransformer m (r, s) = LR.runRWST m r s

instance RunnableTrans IdentityT where
    type TransformerState IdentityT m = ()
    type TransformerResult IdentityT a = a
    currentTransState = return ()
    restoreTransState = return
    runTransformer m () = runIdentityT m

instance RunnableTrans (ExceptT e) where
    type TransformerState (ExceptT e) m = ()
    type TransformerResult (ExceptT e) a = Either e a
    currentTransState = return ()
    restoreTransState (Left e) = throwE e
    restoreTransState (Right a) = return a
    runTransformer m () = runExceptT m

instance RunnableTrans MaybeT where
    type TransformerState MaybeT m = ()
    type TransformerResult MaybeT a = Maybe a
    currentTransState = return ()
    restoreTransState Nothing = mzero
    restoreTransState (Just a) = return a
    runTransformer m () = runMaybeT m

instance RunnableTrans ListT where
     type TransformerState ListT m = ()
     type TransformerResult ListT a = [a]
     currentTransState = return ()
     restoreTransState = fromFoldable
     runTransformer m _ = toList m
