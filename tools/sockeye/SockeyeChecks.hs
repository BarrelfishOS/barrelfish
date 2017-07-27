module SockeyeChecks where

import Control.Monad.Writer

type Checks f = Writer [f]

newtype CheckFailure f = CheckFailure [f]

instance (Show f) => Show (CheckFailure f) where
    show (CheckFailure fs) = unlines $ "":(map show fs)

failure :: f -> Checks f ()
failure f = tell [f]

runChecks :: Checks f a -> Either (CheckFailure f) a
runChecks checks = do
    let
        (a, fs) = runWriter checks
    case fs of
        [] -> return a
        _  -> Left $ CheckFailure fs
