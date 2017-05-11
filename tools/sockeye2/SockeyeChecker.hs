{-
  SockeyeChecker.hs: AST checker for Sockeye

  Part of Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeChecker
( checkSockeye ) where

import Control.Monad.Trans.Writer

import Data.Set (Set)
import qualified Data.Set as Set

import qualified SockeyeAST as AST

findUniqueIdentifiers :: AST.NetSpec -> Writer [String] (Set AST.NodeId)
findUniqueIdentifiers (AST.NetSpec nodes) = let allIds = map fst $ nodes
                                            in foldl checkAndAdd (return Set.empty) allIds
                                            where checkAndAdd w id = do
                                                    uids <- w
                                                    tell $ if id `Set.member` uids then
                                                            ["Duplicate identifier " ++ show id]
                                                           else
                                                            []
                                                    return $ id `Set.insert` uids

checkSockeye :: AST.NetSpec -> [String]
checkSockeye ast = snd $ runWriter $ do
    ids <- findUniqueIdentifiers ast
    return ids