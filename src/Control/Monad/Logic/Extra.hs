module Control.Monad.Logic.Extra
    ( module Control.Monad.Logic
    , module Control.Monad.Logic.Extra
    )
where

import Control.Applicative (empty)
import Control.Monad.Logic (LogicT (LogicT), observeAllT, once, runLogicT)
import Prelude

assume :: Bool -> LogicT m ()
assume True = pure ()
assume False = empty

findall :: (Applicative m) => LogicT m a -> m [a]
findall = observeAllT

fromList :: [a] -> LogicT m a
fromList xs = LogicT $ \cons nil -> foldr cons nil xs

runOnce :: (Monad m) => LogicT m a -> m (Maybe a)
runOnce program =
    runLogicT
        (once program)
        (\a _ -> pure $ Just a)
        (pure Nothing)
