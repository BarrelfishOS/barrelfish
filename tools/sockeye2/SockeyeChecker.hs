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

import Control.Monad
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
                                                            ["Duplicate identifier '" ++ show id ++ "'"]
                                                           else
                                                            []
                                                    return $ id `Set.insert` uids

class Checkable a where
    checkReferences :: (Set AST.NodeId) -> a -> Writer[String] ()

instance Checkable AST.NetSpec where
    checkReferences ids (AST.NetSpec nodes) = foldM (\_ -> checkReferences ids . snd) () nodes

instance Checkable AST.NodeSpec where
    checkReferences ids nodeSpec = do
        foldM (\_ -> checkReferences ids) () $ AST.translate nodeSpec
        case AST.overlay nodeSpec of
            Nothing -> return ()
            Just id -> do
                tell $ if id `Set.member` ids then
                        []
                       else
                        ["Reference to undefined node '" ++ show id ++ "' in overlay"]
                return ()

instance Checkable AST.MapSpec where
    checkReferences ids mapSpec = do
        let destNode = AST.destNode mapSpec
        tell $ if destNode `Set.member` ids then
                        []
                       else
                        ["Reference to undefined node '" ++ show destNode ++ "' in map"]


checkSockeye :: AST.NetSpec -> [String]
checkSockeye ast = snd $ runWriter $ do
    ids <- findUniqueIdentifiers ast
    checkReferences ids ast