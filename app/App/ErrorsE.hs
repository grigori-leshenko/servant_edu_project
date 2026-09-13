module App.ErrorsE where

import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)

mapError ::
  forall e1 e2 es a. (Error e2 :> es, Show e2) => (e1 -> e2) -> Eff (Error e1 : es) a -> Eff es a
mapError f action = do
  result <- runErrorNoCallStack @e1 action
  either (throwError . f) pure result
