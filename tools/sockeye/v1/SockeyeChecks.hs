{-
    SockeyeChecks.hs: Helpers to run checks for Sockeye

    Part of Sockeye

    Copyright (c) 2017, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

module SockeyeChecks where

import Control.Monad.Writer

import Data.List (nub, sort)

data FailedCheck c t = FailedCheck
    { context :: c
    , failed  :: t
    }

newtype FailedChecks t = FailedChecks [FailedCheck t]

instance (Show c, Show t) => Show (FailedChecks t) where
    show (FailedChecks fs) = 
        let modules = sort  (nub $  map inModule fs)
        in unlines $ concat (map showFailsForModule modules)
        where
            showFailsForModule name =
                let
                    title = "\nIn module '" ++ name ++ "':"
                    fails = filter (\f -> name == inModule f) fs
                in case name of
                    ('@':_) -> "":showFails 0 fails
                    _       -> title:showFails 1 fails
            showFails indentLevel fs =
                let
                    indent = replicate (indentLevel * 4) ' '
                    failStrings = nub $ map showFail fs
                in map (indent ++) failStrings
            showFail f = (show $ failed f)

type Checks f = Writer [FailedCheck f]

failCheck :: String -> t -> Checks t ()
failCheck context f = tell [FailedCheck context f]

runChecks :: Checks f a -> Either (FailedChecks f) a
runChecks checks = do
    let
        (a, fs) = runWriter checks
    case fs of
        [] -> return a
        _  -> Left $ FailedChecks fs

checkDuplicates :: (Eq a) => String  -> (a -> t) -> [a] -> (Checks t) ()
checkDuplicates context fail xs = do
    let
        ds = duplicates xs
    case ds of
        [] -> return ()
        _  -> mapM_ (failCheck context . fail) ds
    where
        duplicates [] = []
        duplicates (x:xs)
            | x `elem` xs = nub $ [x] ++ duplicates xs
            | otherwise = duplicates xs
