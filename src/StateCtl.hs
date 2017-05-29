module StateCtl where

import Data.Set
    ( Set
    , member
    , insert
    , fromList
    )
import Control.Monad.Trans.State
    ( State
    , state
    , runState
    )

transform :: (a -> v -> a) -> v -> State (a, Set c) ()
transform f v = state $ \(acc, ctl) -> ((), (f acc v, ctl))

record :: (Ord c) => c -> State (a, Set c) ()
record c = state $ \(acc, ctl) -> ((), (acc, insert c ctl))

seen :: (Ord c) => c -> State (a, Set c) Bool
seen c = state $ \(acc, ctl) -> (member c ctl, (acc, ctl))

runStateCtl :: (Ord c) => State (a, Set c) b -> (a, Set c) -> a
runStateCtl s init = fst . snd . runState s $ init
