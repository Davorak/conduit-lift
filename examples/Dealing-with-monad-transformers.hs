{- | This is a side by side demonstration how conduit with the current 
-- implmentation of hoist does not quite follow the monad transformer laws.
-- the recommended solution and method to get around the problem for 
-- monad transformers that have a MFunctor instance.
--
-- Look at the note in the source for `transPipe` and the link to the work
-- around example:
-- http://hackage.haskell.org/package/conduit-1.0.8/docs/src/Data-Conduit-Internal.html#transPipe
-- 
-- This issue is also documented at:
-- https://github.com/snoyberg/conduit/wiki/Dealing-with-monad-transformers
-}
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Lift

import Control.Monad.Identity
import qualified Control.Monad.Trans.State.Strict as S

import Control.Monad.Morph (hoist, lift)


--source :: Source IO ()
source = CL.sourceList $ replicate 10 ()

replaceNum :: Conduit () (S.StateT Int IO) Int
replaceNum = awaitForever $ \() -> do
    i <- lift S.get
    lift $ S.put $ i + 1
    yield i


-- | Try to inline `evalStateT` with `transPipe and produce unexpected results
-- due to monad transformer laws not being followed.
--
-- >>> test1
-- [1,1,1,1,1,1,1,1,1,1]
test1 = do
    x <- source $$ transPipe (flip S.evalStateT 1) replaceNum =$ CL.consume
    print x

-- | User `evalStateT` out side of conduit as recommended and achieve expected
-- results.
--
-- >>> test2
-- [1,2,3,4,5,6,7,8,9,10]
test2 = do
    x <- flip S.evalStateT 1 $ hoist lift source $$ replaceNum =$ CL.consume
    print x

-- | Use evalStateC form Conduit.Lift to achieve expected result but still be
-- inline isolating the state transformer to only the section of conduit that
-- needs it.
-- 
-- >>> test3
-- [1,2,3,4,5,6,7,8,9,10]
test3 = do
    x <- source $$ (evalStateC 1) replaceNum =$ CL.consume
    print x

