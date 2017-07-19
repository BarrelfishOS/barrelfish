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

import qualified SockeyeAST as AST
import qualified SockeyeASTDecodingNet as NetAST

type NetNodeDecl = (NetAST.NodeId, NetAST.NodeSpec)
type NetList = [NetNodeDecl]
type PortMap = [(String, NetAST.NodeId)]

data FailedCheck
    = ModuleInstLoop [String]
    | DuplicateInPort !String !String
    | DuplicateInMap !String !String
    | UndefinedInPort !String !String
    | DuplicateOutPort !String !String
    | DuplicateOutMap !String !String
    | UndefinedOutPort !String !String
    | DuplicateIdentifer !String
    | UndefinedReference !String

instance Show FailedCheck where
    show (ModuleInstLoop loop) = concat ["Module instantiation loop:'", intercalate "' -> '" loop, "'"]
    show (DuplicateInPort  modName port) = concat ["Multiple declarations of input port '", port, "' in '", modName, "'"]
    show (DuplicateInMap   ns      port) = concat ["Multiple mappings for input port '", port, "' in '", ns, "'"]
    show (UndefinedInPort  modName port) = concat ["'", port, "' is not an input port in '", modName, "'"]
    show (DuplicateOutPort modName port) = concat ["Multiple declarations of output port '", port, "' in '", modName, "'"]
    show (DuplicateOutMap   ns      port) = concat ["Multiple mappings for output port '", port, "' in '", ns, "'"]
    show (UndefinedOutPort modName port) = concat ["'", port, "' is not an output port in '", modName, "'"]
    show (DuplicateIdentifer ident)   = concat ["Multiple declarations of node '", show ident, "'"]
    show (UndefinedReference ident)   = concat ["Reference to undefined node '", show ident, "'"]

newtype CheckFailure = CheckFailure
    { failures :: [FailedCheck] }

instance Show CheckFailure where
    show (CheckFailure fs) = unlines $ "":(map show fs)

data Context = Context
    { spec         :: AST.SockeyeSpec
    , modulePath   :: [String]
    , curNamespace :: NetAST.Namespace
    , paramValues  :: Map String Integer
    , varValues    :: Map String Integer
    , inPortMaps   :: Map String NetAST.NodeId
    , outPortMaps  :: Map String NetAST.NodeId
    , mappedBlocks :: [NetAST.BlockSpec]
    }

sockeyeBuildNet :: AST.SockeyeSpec -> Either CheckFailure NetAST.NetSpec
sockeyeBuildNet ast = do
    let
        context = Context
            { spec         = AST.SockeyeSpec Map.empty
            , modulePath   = []
            , curNamespace = NetAST.Namespace []
            , paramValues  = Map.empty
            , varValues    = Map.empty
            , inPortMaps   = Map.empty
            , outPortMaps  = Map.empty
            , mappedBlocks = []
            }        
    net <- transform context ast
    check Set.empty net
    return net
--            
-- Build net
--
class NetTransformable a b where
    transform :: Context -> a -> Either CheckFailure b

instance NetTransformable AST.SockeyeSpec NetAST.NetSpec where
    transform context ast = do
        let
            rootInst = AST.ModuleInst
                { AST.namespace  = AST.SimpleIdent ""
                , AST.moduleName = "@root"
                , AST.arguments  = Map.empty
                , AST.inPortMap  = []
                , AST.outPortMap = []
                }
            specContext = context
                { spec = ast }
        netList <- transform specContext rootInst
        let
            nodeIds = map fst netList
        checkDuplicates nodeIds DuplicateIdentifer
        let
            nodeMap = Map.fromList netList
        return $ NetAST.NetSpec nodeMap

instance NetTransformable AST.Module NetList where
    transform context ast = do
        let
            inPorts = AST.inputPorts ast
            outPorts = AST.outputPorts ast
            nodeDecls = AST.nodeDecls ast
            moduleInsts = AST.moduleInsts ast
        inDecls <- do
            net <- transform context inPorts
            return $ concat (net :: [NetList])
        outDecls <- do
            net <- transform context outPorts
            return $ concat (net :: [NetList])
        -- TODO check duplicate ports
        -- TODO check mappings to non existing port
        netDecls <- transform context nodeDecls
        netInsts <- transform context moduleInsts
        return $ concat (inDecls:outDecls:netDecls ++ netInsts)            

instance NetTransformable AST.Port NetList where
    transform context (AST.MultiPort for) = do
        netPorts <- transform context for
        return $ concat (netPorts :: [NetList])
    transform context (AST.InputPort portId portWidth) = do
        netPortId <- transform context portId
        let
            portMap = inPortMaps context
            name = NetAST.name netPortId
            mappedId = Map.lookup name portMap
        case mappedId of
            Nothing    -> return []
            Just ident -> do
                let
                    node = portNode netPortId portWidth
                return [(ident, node)]
    transform context (AST.OutputPort portId portWidth) = do
        netPortId <- transform context portId
        let
            portMap = outPortMaps context
            name = NetAST.name netPortId
            mappedId = Map.lookup name portMap
        case mappedId of
            Nothing    -> return [(netPortId, portNodeTemplate)]
            Just ident -> do
                let
                    node = portNode ident portWidth
                return [(netPortId, node)]

portNode :: NetAST.NodeId -> Integer -> NetAST.NodeSpec
portNode destNode width =
    let
        base = NetAST.Address 0
        limit = NetAST.Address $ 2^width - 1
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

instance NetTransformable AST.ModuleInst NetList where
    transform context (AST.MultiModuleInst for) = do
        net <- transform context for
        return $ concat (net :: [NetList])
    transform context ast = do
        let
            namespace = AST.namespace ast
            name = AST.moduleName ast
            args = AST.arguments ast
            inPortMap = AST.inPortMap ast
            outPortMap = AST.outPortMap ast
            mod = getModule context name
        checkSelfInst name
        netNamespace <- transform context namespace
        netArgs <- transform context args
        netInMap <- transform context inPortMap
        netOutMap <- transform context outPortMap
        let
            inMaps = concat (netInMap :: [PortMap])
            outMaps = concat (netOutMap :: [PortMap])
        checkDuplicates (map fst inMaps) (DuplicateInMap $ show netNamespace) 
        checkDuplicates (map fst outMaps) (DuplicateOutMap $ show netNamespace)
        let
            modContext = moduleContext name netNamespace netArgs inMaps outMaps
        transform modContext mod
            where
                moduleContext name namespace args inMaps outMaps =
                    let
                        path = modulePath context
                        base = NetAST.ns $ NetAST.namespace namespace
                        newNs = case NetAST.name namespace of
                            "" -> NetAST.Namespace base
                            n  -> NetAST.Namespace $ n:base
                    in context
                        { modulePath   = name:path
                        , curNamespace = newNs
                        , paramValues  = args
                        , varValues    = Map.empty
                        , inPortMaps   = Map.fromList inMaps
                        , outPortMaps  = Map.fromList outMaps
                        }
                checkSelfInst name = do
                    let
                        path = modulePath context
                    case loop path of
                        [] -> return ()
                        l  -> Left $ CheckFailure [ModuleInstLoop (reverse $ name:l)]
                        where
                            loop [] = []
                            loop path@(p:ps)
                                | name `elem` path = p:(loop ps)
                                | otherwise = []


instance NetTransformable AST.PortMap PortMap where
    transform context (AST.MultiPortMap for) = do
        ts <- transform context for
        return $ concat (ts :: [PortMap])
    transform context ast = do
        let
            mappedId = AST.mappedId ast
            mappedPort = AST.mappedPort ast
        netMappedId <- transform context mappedId
        netMappedPort <- transform context mappedPort
        return [(NetAST.name netMappedPort, netMappedId)]

instance NetTransformable AST.ModuleArg Integer where
    transform _ (AST.AddressArg value) = return value
    transform _ (AST.NaturalArg value) = return value
    transform context (AST.ParamArg name) = return $ getParamValue context name

instance NetTransformable AST.Identifier NetAST.NodeId where
    transform context ast = do
        let
            namespace = curNamespace context
            name = identName ast
        return NetAST.NodeId
            { NetAST.namespace = namespace
            , NetAST.name      = name
            }
            where
                identName (AST.SimpleIdent name) = name
                identName ident =
                    let
                        prefix = AST.prefix ident
                        varName = AST.varName ident
                        suffix = AST.suffix ident
                        varValue = show $ getVarValue context varName
                        suffixName = case suffix of
                            Nothing -> ""
                            Just s  -> identName s
                    in prefix ++ varValue ++ suffixName

instance NetTransformable AST.NodeDecl NetList where
    transform context (AST.MultiNodeDecl for) = do
        ts <- transform context for
        return $ concat (ts :: [NetList])
    transform context ast = do
        let
            ident = AST.nodeId ast
            nodeSpec = AST.nodeSpec ast
        nodeId <- transform context ident
        netNodeSpec <- transform context nodeSpec
        return [(nodeId, netNodeSpec)]

instance NetTransformable AST.NodeSpec NetAST.NodeSpec where
    transform context ast = do
        let
            nodeType = AST.nodeType ast
            accept = AST.accept ast
            translate = AST.translate ast
            overlay = AST.overlay ast
        netNodeType <- maybe (return NetAST.Other) (transform context) nodeType
        netAccept <- transform context accept
        netTranslate <- transform context translate
        let
            mapBlocks = map NetAST.srcBlock netTranslate
            nodeContext = context
                { mappedBlocks = netAccept ++ mapBlocks }
        netOverlay <- case overlay of
                Nothing -> return []
                Just o  -> transform nodeContext o
        return NetAST.NodeSpec
            { NetAST.nodeType  = netNodeType
            , NetAST.accept    = netAccept
            , NetAST.translate = netTranslate ++ netOverlay
            }

instance NetTransformable AST.NodeType NetAST.NodeType where
    transform _ AST.Memory = return NetAST.Memory
    transform _ AST.Device = return NetAST.Device

instance NetTransformable AST.BlockSpec NetAST.BlockSpec where
    transform context (AST.SingletonBlock address) = do
        netAddress <- transform context address
        return NetAST.BlockSpec
            { NetAST.base  = netAddress
            , NetAST.limit = netAddress
            }
    transform context (AST.RangeBlock base limit) = do
        netBase <- transform context base
        netLimit <- transform context limit
        return NetAST.BlockSpec
            { NetAST.base  = netBase
            , NetAST.limit = netLimit
            }
    transform context (AST.LengthBlock base bits) = do
        netBase <- transform context base
        let
            baseAddress = NetAST.address netBase
            limit = baseAddress + 2^bits - 1
            netLimit = NetAST.Address limit
        return NetAST.BlockSpec
            { NetAST.base  = netBase
            , NetAST.limit = netLimit
            }

instance NetTransformable AST.MapSpec NetAST.MapSpec where
    transform context ast = do
        let
            block = AST.block ast
            destNode = AST.destNode ast
            destBase = fromMaybe (AST.LiteralAddress 0) (AST.destBase ast)
        netBlock <- transform context block
        netDestNode <- transform context destNode
        netDestBase <- transform context destBase
        return NetAST.MapSpec
            { NetAST.srcBlock = netBlock
            , NetAST.destNode = netDestNode
            , NetAST.destBase = netDestBase
            }

instance NetTransformable AST.OverlaySpec [NetAST.MapSpec] where
    transform context ast = do
        let
            over = AST.over ast
            width = AST.width ast
            blocks = mappedBlocks context
        netOver <- transform context over
        let
            maps = overlayMaps netOver width blocks
        return maps

overlayMaps :: NetAST.NodeId -> Integer ->[NetAST.BlockSpec] -> [NetAST.MapSpec]
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
                [ BlockStart $ NetAST.address base
                , BlockEnd   $ NetAST.address limit
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
                        baseAddress = NetAST.Address $ startAddress s
                        limitAddress = NetAST.Address $ a - 1
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
    = BlockStart { address :: !Integer }
    | BlockEnd   { address :: !Integer }
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
        , startAddress :: !Integer
        } deriving (Show)

instance NetTransformable AST.Address NetAST.Address where
    transform _ (AST.LiteralAddress value) = do
        return $ NetAST.Address value
    transform context (AST.ParamAddress name) = do
        let
            value = getParamValue context name
        return $ NetAST.Address value

instance NetTransformable a b => NetTransformable (AST.For a) [b] where
    transform context ast = do
        let
            body = AST.body ast
            varRanges = AST.varRanges ast
        concreteRanges <- transform context varRanges
        let
            valueList = Map.foldWithKey iterations [] concreteRanges
            iterContexts = map iterationContext valueList
            ts = map (\c -> transform c body) iterContexts
            fs = lefts ts
            bs = rights ts
        case fs of
            [] -> return $ bs
            _  -> Left $ CheckFailure (concat $ map failures fs)
        where
            iterations k vs [] = [Map.fromList [(k,v)] | v <- vs]
            iterations k vs ms = concat $ map (f ms k) vs
                where
                    f ms k v = map (Map.insert k v) ms
            iterationContext varMap =
                let
                    values = varValues context
                in context
                    { varValues = values `Map.union` varMap }

instance NetTransformable AST.ForRange [Integer] where
    transform context ast = do
        let
            start = AST.start ast
            end = AST.end ast
        startVal <- transform context start
        endVal <- transform context end
        return [startVal..endVal]

instance NetTransformable AST.ForLimit Integer where
    transform _ (AST.LiteralLimit value) = return value
    transform context (AST.ParamLimit name) = return $ getParamValue context name

instance NetTransformable a b => NetTransformable [a] [b] where
    transform context ast = do
        let
            ts = map (transform context) ast
            fs = lefts ts
            bs = rights ts
        case fs of
            [] -> return bs
            _  -> Left $ CheckFailure (concat $ map failures fs)

instance (Ord k, NetTransformable a b) => NetTransformable (Map k a) (Map k b) where
    transform context ast = do
        let
            ks = Map.keys ast
            es = Map.elems ast
        ts <- transform context es
        return $ Map.fromList (zip ks ts)

--
-- Checks
--
class NetCheckable a where
    check :: Set NetAST.NodeId -> a -> Either CheckFailure ()

instance NetCheckable NetAST.NetSpec where
    check _ (NetAST.NetSpec net) = do
        let
            specContext = Map.keysSet net
        check specContext $ Map.elems net

instance NetCheckable NetAST.NodeSpec where
    check context net = do
        let
            translate = NetAST.translate net
        check context translate

instance NetCheckable NetAST.MapSpec where
    check context net = do
        let
           destNode = NetAST.destNode net
        check context destNode

instance NetCheckable NetAST.NodeId where
    check context net = do
        if net `Set.member` context
            then return ()
            else Left $ CheckFailure [UndefinedReference $ show net]

instance NetCheckable a => NetCheckable [a] where
    check context net = do
        let
            checked = map (check context) net
            fs = lefts $ checked
        case fs of
            [] -> return ()
            _  -> Left $ CheckFailure (concat $ map failures fs)

getModule :: Context -> String -> AST.Module
getModule context name =
    let
        modules = AST.modules $ spec context
    in modules Map.! name

getParamValue :: Context -> String -> Integer
getParamValue context name =
    let
        params = paramValues context
    in params Map.! name

getVarValue :: Context -> String -> Integer
getVarValue context name =
    let
        vars = varValues context
    in vars Map.! name

checkDuplicates :: (Eq a, Show a) => [a] -> (String -> FailedCheck) -> Either CheckFailure ()
checkDuplicates nodeIds fail = do
    let
        duplicates = duplicateNames nodeIds
    case duplicates of
        [] -> return ()
        _  -> Left $ CheckFailure (map (fail . show) duplicates)
    where
        duplicateNames [] = []
        duplicateNames (x:xs)
            | x `elem` xs = nub $ [x] ++ duplicateNames xs
            | otherwise = duplicateNames xs
