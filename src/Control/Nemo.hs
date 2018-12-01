module Control.Nemo where

import           Control.Monad.RWS.Lazy (RWST (RWST), runRWST)

runState :: Monad m => s -> RWST r w s m a -> RWST r w b m a
runState s rwst =
  RWST $ \r b -> do
    (a, _, w) <- runRWST rwst r s
    return (a, b, w)
