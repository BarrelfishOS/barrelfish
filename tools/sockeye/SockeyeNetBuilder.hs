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
( buildSockeyeNet ) where

import Control.Monad.State

import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import SockeyeChecks

import qualified SockeyeASTInstantiator as InstAST
import qualified SockeyeASTDecodingNet as NetAST

data NetBuildFail
    = UndefinedOutPort   !String !String
    | UndefinedInPort    !String !String
    | UndefinedReference !String !String

instance Show NetBuildFail where
    show (UndefinedInPort  inst port)  = concat ["Undefined input port '",   port, "' in module instantiation '", inst, "'"]
    show (UndefinedOutPort inst port)  = concat ["Undefined output port '",  port, "' in module instantiation '", inst, "'"]
    show (UndefinedReference context ident) = concat ["Reference to undefined node '", ident, "' in ", context]

type PortMap = Map InstAST.Identifier NetAST.NodeId

data Context = Context
    { modules      :: Map InstAST.Identifier InstAST.Module
    , curModule    :: !String
    , curNamespace :: [String]
    , curNode      :: !String
    , inPortMap    :: PortMap
    , outPortMap   :: PortMap
    , nodes        :: Set String
    , mappedBlocks :: [InstAST.BlockSpec]
    }

buildSockeyeNet :: InstAST.SockeyeSpec -> Either (FailedChecks NetBuildFail) NetAST.NetSpec
buildSockeyeNet ast = do
    let
        context = Context
            { modules      = Map.empty
            , curModule    = ""
            , curNamespace = []
            , curNode      = ""
            , inPortMap    = Map.empty
            , outPortMap   = Map.empty
            , nodes        = Set.empty
            , mappedBlocks = []
            }        
    net <- runChecks $ transform context ast
    return net

--            
-- Build net
--
class NetTransformable a b where
    transform :: Context -> a -> Checks NetBuildFail b

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
        let inPorts = InstAST.inputPorts ast
            outPorts = InstAST.outputPorts ast
            moduleInsts = InstAST.moduleInsts ast
            nodeDecls = InstAST.nodeDecls ast
            outPortIds = map InstAST.portId outPorts
            inMapIds = concatMap Map.elems $ map InstAST.inPortMap moduleInsts
            declIds = map InstAST.nodeId nodeDecls
            modContext = context
                { nodes = Set.fromList $ outPortIds ++ inMapIds ++ declIds }
        inPortDecls <- transform modContext inPorts
        outPortDecls <- transform modContext outPorts
        netDecls <- transform modContext nodeDecls
        netInsts <- transform modContext moduleInsts     
        return $ Map.unions (inPortDecls ++ outPortDecls ++ netDecls ++ netInsts)

instance NetTransformable InstAST.Port NetAST.NetSpec where
    transform context ast@(InstAST.InputPort {}) = do
        let portId = InstAST.portId ast
            portWidth = InstAST.portWidth ast
            portMap = inPortMap context
            mappedId = Map.lookup portId portMap
            errorContext = "input port declaration"
        checkReference context (UndefinedReference errorContext) portId
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
            mod = (modules context) Map.! name
            inPortIds = Set.fromList $ map InstAST.portId (InstAST.inputPorts mod)
            outPortIds = Set.fromList $ map InstAST.portId (InstAST.outputPorts mod)
            instString = concat [name, maybe  "" (" as " ++ ) namespace]
            errorContext = concat ["port mapping for '", instString, "'"]
        mapM_ (checkReference context $ UndefinedReference errorContext) $ (Map.elems inPortMap) ++ (Map.elems outPortMap)
        checkAllExist (UndefinedInPort instString) inPortIds $ Map.keysSet inPortMap
        checkAllExist (UndefinedOutPort instString) outPortIds $ Map.keysSet outPortMap
        netInMap <- transform context inPortMap
        netOutMap <- transform context outPortMap
        let curNs = curNamespace context
            instContext = context
                { curModule    = name
                , curNamespace = maybe curNs (:curNs) namespace
                , inPortMap    = netInMap
                , outPortMap   = netOutMap
                }
        transform instContext mod
        where
            checkAllExist fail existing xs = do
                let undef = xs Set.\\ existing
                if Set.null undef
                    then return ()
                    else mapM_ (failCheck (curModule context) . fail) undef

instance NetTransformable InstAST.NodeDecl NetAST.NetSpec where
    transform context ast = do
        let nodeId = InstAST.nodeId ast
            nodeSpec = InstAST.nodeSpec ast
            nodeContext = context
                { curNode = nodeId }
        netNodeId <- transform context nodeId
        netNodeSpec <- transform nodeContext nodeSpec
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
            errorContext = "tranlate set of node '" ++ curNode context ++ "'"
        checkReference context (UndefinedReference errorContext) destNode
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
            errorContext = "overlay of node '" ++ curNode context ++ "'"
        checkReference context (UndefinedReference errorContext) over
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

instance (Traversable t, NetTransformable a b) => NetTransformable (t a)  (t b) where
    transform context as = mapM (transform context) as


---
--- Helpers
---
checkReference :: Context -> (String -> NetBuildFail) -> String -> (Checks NetBuildFail) ()
checkReference context fail name =
    if name `Set.member` (nodes context)
        then return ()
        else failCheck (curModule context) (fail name)
