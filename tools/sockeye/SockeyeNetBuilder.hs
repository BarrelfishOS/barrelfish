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

data Context = Context
    { modules      :: Map String InstAST.Module
    , curNamespace :: [String]
    -- , inPortMaps   :: Map String NetAST.NodeId
    -- , outPortMaps  :: Map String NetAST.NodeId
    , mappedBlocks :: [InstAST.BlockSpec]
    }

sockeyeBuildNet :: InstAST.SockeyeSpec -> Either (FailedChecks NetBuildFails) NetAST.NetSpec
sockeyeBuildNet ast = do
    let
        context = Context
            { modules      = Map.empty
            , curNamespace = []
            -- , inPortMaps   = Map.empty
            -- , outPortMaps  = Map.empty
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
        let inPorts = InstAST.inputPorts ast
            outPorts = InstAST.outputPorts ast
            nodeDecls = InstAST.nodeDecls ast
            moduleInsts = InstAST.moduleInsts ast
        -- inDecls <- transform context inPorts
        -- outDecls <- transform context outPorts
        -- TODO check mappings to non existing port
        netDecls <- transform context nodeDecls
        netInsts <- transform context moduleInsts
        -- return $ concat (inDecls:outDecls:netDecls ++ netInsts)            
        return $ Map.unions [netDecls, netInsts]

-- instance NetTransformable InstAST.Port NetList where
--     transform context (AST.MultiPort for) = do
--         netPorts <- transform context for
--         return $ concat (netPorts :: [NetList])
--     transform context (AST.InputPort portId portWidth) = do
--         netPortId <- transform context portId
--         let
--             portMap = inPortMaps context
--             name = NetAST.name netPortId
--             mappedId = Map.lookup name portMap
--         case mappedId of
--             Nothing    -> return []
--             Just ident -> do
--                 let
--                     node = portNode netPortId portWidth
--                 return [(ident, node)]
--     transform context (AST.OutputPort portId portWidth) = do
--         netPortId <- transform context portId
--         let
--             portMap = outPortMaps context
--             name = NetAST.name netPortId
--             mappedId = Map.lookup name portMap
--         case mappedId of
--             Nothing    -> return [(netPortId, portNodeTemplate)]
--             Just ident -> do
--                 let
--                     node = portNode ident portWidth
--                 return [(netPortId, node)]

-- portNode :: NetAST.NodeId -> Integer -> NetAST.NodeSpec
-- portNode destNode width =
--     let
--         base = NetAST.Address 0
--         limit = NetAST.Address $ 2^width - 1
--         srcBlock = NetAST.BlockSpec
--             { NetAST.base  = base
--             , NetAST.limit = limit
--             }
--         map = NetAST.MapSpec
--                 { NetAST.srcBlock = srcBlock
--                 , NetAST.destNode = destNode
--                 , NetAST.destBase = base
--                 }
--     in portNodeTemplate { NetAST.translate = [map] }

-- portNodeTemplate :: NetAST.NodeSpec
-- portNodeTemplate = NetAST.NodeSpec
--     { NetAST.nodeType  = NetAST.Other
--     , NetAST.accept    = []
--     , NetAST.translate = []
--     }

instance NetTransformable InstAST.ModuleInstMap NetAST.NetSpec where
        transform context ast = do
            let namespaces = Map.keys ast
                modInsts = Map.elems ast
            let
                contexts = map addNamespace namespaces
            netModInsts <- mapM (uncurry transform) $ zip contexts modInsts
            return $ Map.unions netModInsts
            where
                addNamespace n =
                    let ns = curNamespace context
                    in context
                        { curNamespace = n:ns }


instance NetTransformable InstAST.ModuleInst NetAST.NetSpec where
    transform context ast = do
        let name = InstAST.moduleName ast
            inPortMap = InstAST.inPortMap ast
            outPortMap = InstAST.outPortMap ast
            mod = getModule context name
        -- netInMap <- transform context inPortMap
        -- netOutMap <- transform context outPortMap
        transform context mod

-- instance NetTransformable AST.PortMap PortMap where
--     transform context (AST.MultiPortMap for) = do
--         ts <- transform context for
--         return $ concat (ts :: [PortMap])
--     transform context ast = do
--         let
--             mappedId = AST.mappedId ast
--             mappedPort = AST.mappedPort ast
--         netMappedId <- transform context mappedId
--         netMappedPort <- transform context mappedPort
--         return [(NetAST.name netMappedPort, netMappedId)]

-- instance NetTransformable AST.ModuleArg Integer where
--     transform _ (AST.AddressArg value) = return value
--     transform _ (AST.NaturalArg value) = return value
--     transform context (AST.ParamArg name) = return $ getParamValue context name

instance NetTransformable InstAST.NodeDeclMap NetAST.NetSpec where
    transform context ast = do
        let
            idents = Map.keys ast
            nodeSpecs = Map.elems ast
        netNodeIds <- transform context idents
        netNodeSpec <- transform context nodeSpecs
        return $ Map.fromList (zip netNodeIds netNodeSpec)

instance NetTransformable InstAST.Identifier NetAST.NodeId where
    transform context ast = do
        let
            namespace = curNamespace context
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
        let
            maps = overlayMaps netOver width blocks
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
