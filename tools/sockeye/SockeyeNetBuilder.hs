{-
    SockeyeNetBuilder.hs: Decoding net builder for Sockeye

    Part of Sockeye

    Copyright (c) 2017, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module SockeyeNetBuilder
( sockeyeBuildNet ) where

import Control.Monad.State

import Data.Either
import Data.List (nub, intercalate, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import SockeyeChecks

import qualified SockeyeASTInstantiator as InstAST
import qualified SockeyeASTDecodingNet as NetAST

import Debug.Trace

data NetBuildFails
    = UndefinedOutPort   !String !String
    | UndefinedInPort    !String !String
    | UndefinedReference !String !String

instance Show NetBuildFails where
    show (UndefinedInPort  inst port)    = concat ["Mapping to undefined input port '",   port, "' in module instantiation '", inst, "'"]
    show (UndefinedOutPort inst port)    = concat ["Mapping to undefined output port '",  port, "' in module instantiation '", inst, "'"]
    show (UndefinedReference decl ident) = concat ["Reference to undefined node '", ident, "' in declaration of node '", decl, "'"]

type PortMap = Map InstAST.Identifier NetAST.NodeId

data Context = Context
    { modules      :: Map String InstAST.Module
    , curNamespace :: [String]
    , inPortMap    :: PortMap
    , outPortMap   :: PortMap
    , mappedBlocks :: [InstAST.BlockSpec]
    }

sockeyeBuildNet :: InstAST.SockeyeSpec -> Either (FailedChecks NetBuildFails) NetAST.NetSpec
sockeyeBuildNet ast = do
    let
        context = Context
            { modules      = Map.empty
            , curNamespace = []
            , inPortMap    = Map.empty
            , outPortMap   = Map.empty
            , mappedBlocks = []
            }        
    net <- runChecks $ transform context ast
    -- check Set.empty net
    return net

--            
-- Build net
--
class NetTransformable a b where
    transform :: Context -> a -> Checks NetBuildFails b

instance NetTransformable InstAST.SockeyeSpec NetAST.NetSpec where
    transform context ast = do
        let
            rootInst = InstAST.root ast
            mods = InstAST.modules ast
            specContext = context
                { modules = mods }
        transform specContext rootInst

instance NetTransformable InstAST.Module NetAST.NetSpec where
    transform context ast = do
        let ports = InstAST.ports ast
            nodeDecls = InstAST.nodeDecls ast
            moduleInsts = InstAST.moduleInsts ast
        -- TODO check mappings to non existing port
        portDecls <- transform context ports
        netDecls <- transform context nodeDecls
        netInsts <- transform context moduleInsts     
        return $ Map.unions (portDecls ++ netDecls ++ netInsts)

instance NetTransformable InstAST.Port NetAST.NetSpec where
    transform context ast@(InstAST.InputPort {}) = do
        let portId = InstAST.portId ast
            portWidth = InstAST.portWidth ast
            portMap = inPortMap context
            mappedId = Map.lookup portId portMap
        netPortId <- transform context portId
        case mappedId of
            Nothing    -> return Map.empty
            Just ident -> do
                let node = portNode netPortId portWidth
                return $ Map.fromList [(ident, node)]
    transform context ast@(InstAST.OutputPort {}) = do
        let portId = InstAST.portId ast
            portWidth = InstAST.portWidth ast
            portMap = outPortMap context
            mappedId = Map.lookup portId portMap
        netPortId <- transform context portId
        case mappedId of
            Nothing    -> return $ Map.fromList [(netPortId, portNodeTemplate)]
            Just ident -> do
                let node = portNode ident portWidth
                return $ Map.fromList $ [(netPortId, node)]

portNode :: NetAST.NodeId -> Integer -> NetAST.NodeSpec
portNode destNode width =
    let base = 0
        limit = 2^width - 1
        srcBlock = NetAST.BlockSpec
            { NetAST.base  = base
            , NetAST.limit = limit
            }
        map = NetAST.MapSpec
                { NetAST.srcBlock = srcBlock
                , NetAST.destNode = destNode
                , NetAST.destBase = base
                }
    in portNodeTemplate { NetAST.translate = [map] }

portNodeTemplate :: NetAST.NodeSpec
portNodeTemplate = NetAST.NodeSpec
    { NetAST.nodeType  = NetAST.Other
    , NetAST.accept    = []
    , NetAST.translate = []
    }

instance NetTransformable InstAST.ModuleInst NetAST.NetSpec where
    transform context ast = do
        let name = InstAST.moduleName ast
            namespace = InstAST.namespace ast
            inPortMap = InstAST.inPortMap ast
            outPortMap = InstAST.outPortMap ast
            mod = getModule context name
        netInMap <- transform context inPortMap
        netOutMap <- transform context outPortMap
        let instContext = context
                { curNamespace = namespace:(curNamespace context)
                , inPortMap    = netInMap
                , outPortMap   = netOutMap
                }
        transform instContext mod

instance NetTransformable InstAST.PortMap PortMap where
    transform context ast = do
        mapM (transform context) ast

instance NetTransformable InstAST.NodeDecl NetAST.NetSpec where
    transform context ast = do
        let nodeId = InstAST.nodeId ast
            nodeSpec = InstAST.nodeSpec ast
        netNodeId <- transform context nodeId
        netNodeSpec <- transform context nodeSpec
        return $ Map.fromList [(netNodeId, netNodeSpec)]

instance NetTransformable InstAST.Identifier NetAST.NodeId where
    transform context ast = do
        let namespace = curNamespace context
        return NetAST.NodeId
            { NetAST.namespace = namespace
            , NetAST.name      = ast
            }

instance NetTransformable InstAST.NodeSpec NetAST.NodeSpec where
    transform context ast = do
        let
            nodeType = InstAST.nodeType ast
            accept = InstAST.accept ast
            translate = InstAST.translate ast
            reserved = InstAST.reserved ast
            overlay = InstAST.overlay ast
        netTranslate <- transform context translate
        let
            mapBlocks = map NetAST.srcBlock netTranslate
            nodeContext = context
                { mappedBlocks = accept ++ mapBlocks ++ reserved }
        netOverlay <- case overlay of
                Nothing -> return []
                Just o  -> transform nodeContext o
        return NetAST.NodeSpec
            { NetAST.nodeType  = nodeType
            , NetAST.accept    = accept
            , NetAST.translate = netTranslate ++ netOverlay
            }

instance NetTransformable InstAST.MapSpec NetAST.MapSpec where
    transform context ast = do
        let
            srcBlock = InstAST.srcBlock ast
            destNode = InstAST.destNode ast
            destBase = InstAST.destBase ast
        netDestNode <- transform context destNode
        return NetAST.MapSpec
            { NetAST.srcBlock = srcBlock
            , NetAST.destNode = netDestNode
            , NetAST.destBase = destBase
            }

instance NetTransformable InstAST.OverlaySpec [NetAST.MapSpec] where
    transform context ast = do
        let
            over = InstAST.over ast
            width = InstAST.width ast
            blocks = mappedBlocks context
        netOver <- transform context over
        let maps = overlayMaps netOver width blocks
        return maps

overlayMaps :: NetAST.NodeId -> Integer -> [NetAST.BlockSpec] -> [NetAST.MapSpec]
overlayMaps destId width blocks =
    let
        blockPoints = concat $ map toScanPoints blocks
        maxAddress = 2^width
        overStop  = BlockStart $ maxAddress
        scanPoints = filter ((maxAddress >=) . address) $ sort (overStop:blockPoints)
        startState = ScanLineState
            { insideBlocks    = 0
            , startAddress    = 0
            }
    in evalState (scanLine scanPoints []) startState
    where
        toScanPoints (NetAST.BlockSpec base limit) =
                [ BlockStart base
                , BlockEnd   limit
                ]
        scanLine [] ms = return ms
        scanLine (p:ps) ms = do
            maps <- pointAction p ms
            scanLine ps maps
        pointAction (BlockStart a) ms = do
            s <- get       
            let
                i = insideBlocks s
                base = startAddress s
                limit = a - 1
            maps <- if (i == 0) && (base <= limit)
                then
                    let
                        baseAddress = startAddress s
                        limitAddress = a - 1
                        srcBlock = NetAST.BlockSpec baseAddress limitAddress
                        m = NetAST.MapSpec srcBlock destId baseAddress
                    in return $ m:ms
                else return ms
            modify (\s -> s { insideBlocks = i + 1})
            return maps
        pointAction (BlockEnd a) ms = do
            s <- get
            let
                i = insideBlocks s
            put $ ScanLineState (i - 1) (a + 1)
            return ms

data StoppingPoint
    = BlockStart { address :: !NetAST.Address }
    | BlockEnd   { address :: !NetAST.Address }
    deriving (Eq, Show)

instance Ord StoppingPoint where
    (<=) (BlockStart a1) (BlockEnd   a2)
        | a1 == a2 = True
        | otherwise = a1 <= a2
    (<=) (BlockEnd   a1) (BlockStart a2)
        | a1 == a2 = False
        | otherwise = a1 <= a2
    (<=) sp1 sp2 = (address sp1) <= (address sp2)

data ScanLineState
    = ScanLineState
        { insideBlocks :: !Integer
        , startAddress :: !NetAST.Address
        } deriving (Show)

instance NetTransformable a b => NetTransformable [a] [b] where
    transform context as = mapM (transform context) as

-- instance (Ord k, NetTransformable a b) => NetTransformable (Map k a) (Map k b) where
--     transform context ast = do
--         let
--             ks = Map.keys ast
--             es = Map.elems ast
--         ts <- transform context es
--         return $ Map.fromList (zip ks ts)

-- --
-- -- Checks
-- --
-- class NetCheckable a where
--     check :: Set NetAST.NodeId -> a -> Either CheckFailure ()

-- instance NetCheckable NetAST.NetSpec where
--     check _ (NetAST.NetSpec net) = do
--         let
--             specContext = Map.keysSet net
--         check specContext $ Map.elems net

-- instance NetCheckable NetAST.NodeSpec where
--     check context net = do
--         let
--             translate = NetAST.translate net
--         check context translate

-- instance NetCheckable NetAST.MapSpec where
--     check context net = do
--         let
--            destNode = NetAST.destNode net
--         check context destNode

-- instance NetCheckable NetAST.NodeId where
--     check context net = do
--         if net `Set.member` context
--             then return ()
--             else Left $ CheckFailure [UndefinedReference $ show net]

-- instance NetCheckable a => NetCheckable [a] where
--     check context net = do
--         let
--             checked = map (check context) net
--             fs = lefts $ checked
--         case fs of
--             [] -> return ()
--             _  -> Left $ CheckFailure (concat $ map failures fs)

getModule :: Context -> String -> InstAST.Module
getModule context name = (modules context) Map.! name
