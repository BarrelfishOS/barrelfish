{-
    SockeyeChecks.hs: Helpers to run checks for Sockeye

    Part of Sockeye

    Copyright (c) 2018, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

module SockeyeChecks where

import Control.Monad.Writer
import Data.List (sortBy)

import SockeyeASTMeta

class CheckFailure a where
    errorLines :: a -> [String]

newtype FailedChecks f = FailedChecks [(ASTMeta,f)]

instance (CheckFailure f) => Show (FailedChecks f) where
    show (FailedChecks fs) = concat (map showFail $ sortBy failCompare fs)
        where
            failCompare a b = compare (fst a) (fst b)
            showFail (m,f) = '\n':(show m) ++ '\n':(unlines $ map ("    " ++) (errorLines f))

type Checks f = Writer [(ASTMeta,f)]

failCheck :: ASTMeta -> f -> Checks f ()
failCheck m f = tell [(m,f)]

runChecks :: Checks f a -> Either (FailedChecks f) a
runChecks checks =
    let (a, fs) = runWriter checks in
    case fs of
        [] -> Right a
        _  -> Left $ FailedChecks fs

foldChecks :: (Foldable t) => (a -> b -> Checks f b) -> b -> t a -> Checks f b
foldChecks fn acc as = foldl foldfn (return acc) as
    where
        foldfn m a = do
            b <- m
            fn a b
