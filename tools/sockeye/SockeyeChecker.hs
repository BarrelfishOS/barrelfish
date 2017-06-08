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

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char (toLower)

import qualified SockeyeAST as AST

type CheckFailure = (Maybe AST.NodeId, String)

canonicalId :: AST.NodeId -> AST.NodeId
canonicalId (AST.NodeId id) = AST.NodeId $ map toLower id

findUniqueIdentifiers :: AST.NetSpec -> Writer [CheckFailure] (Set AST.NodeId)
findUniqueIdentifiers (AST.NetSpec nodes) = let allIds = map fst $ nodes
                                            in foldl checkAndAdd (return Set.empty) allIds
                                            where checkAndAdd w id = do
                                                    let cId = canonicalId id
                                                    uids <- w
                                                    tell $ if cId `Set.member` uids then
                                                            [(Nothing, "Duplicate identifier '" ++ show id ++ "'")]
                                                           else
                                                            []
                                                    return $ cId `Set.insert` uids

class Checkable a where
    checkReferences :: (Set AST.NodeId) -> a -> Writer [CheckFailure] Bool

instance Checkable AST.NetSpec where
    checkReferences ids (AST.NetSpec nodes) = do
        foldM (checkNode) False nodes
        where checkNode prevError (nodeId, node) = prependId nodeId $ runWriter $ do
                (hasError, errors) <- listen $ checkReferences ids node
                return $ hasError || prevError
              prependId nodeId (hasError, errors) = writer (hasError, map (\(_, e) -> (Just nodeId, e)) errors)

instance Checkable AST.NodeSpec where
    checkReferences ids nodeSpec = do
        foldM checkMap False $ AST.translate nodeSpec
        case AST.overlay nodeSpec of
            Nothing -> return False
            Just id -> do
                let cId  = canonicalId id
                    undefined = cId `Set.notMember` ids
                tell $ if undefined then
                        [(Nothing, "Reference to undefined node '" ++ show id ++ "' in overlay")]
                       else
                        []
                return undefined
        where checkMap prevError mapSpec = do
                hasError <- checkReferences ids mapSpec
                return $ hasError || prevError

instance Checkable AST.MapSpec where
    checkReferences ids mapSpec = do
        let destNode = AST.destNode mapSpec
            cDestNode = canonicalId destNode
            undefined = cDestNode `Set.notMember` ids
        tell $ if undefined then
                [(Nothing, "Reference to undefined node '" ++ show destNode ++ "' in map")]
               else
                []
        return undefined

{- Group the failed checks by their nodeId-}
group :: [CheckFailure] -> [(Maybe AST.NodeId, [String])]
group fs = Map.toList $ foldr addOrAppend Map.empty fs
        where addOrAppend (key, error) m = if key `Map.member` m then
                                                Map.insert key (error:(m Map.! key))  m
                                            else
                                                Map.insert key [error] m

checkSockeye :: AST.NetSpec -> [(Maybe AST.NodeId, [String])]
checkSockeye ast = group $ snd $ runWriter $ do
    ids <- findUniqueIdentifiers ast
    checkReferences ids ast
