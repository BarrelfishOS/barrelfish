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
import Data.List (nub, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Numeric (showHex)

import qualified SockeyeAST as AST
import qualified SockeyeASTDecodingNet as NetAST

import Text.Groom (groom)
import Debug.Trace

type NetNodeDecl = (NetAST.NodeId, NetAST.NodeSpec)
type NetList = [NetNodeDecl]
type PortList = [NetAST.NodeId]
type PortMap = [(NetAST.NodeId, NetAST.NodeId)]

data FailedCheck
    = DuplicateInPort AST.ModuleInst NetAST.NodeId
    | UndefinedInPort AST.ModuleInst NetAST.NodeId
    | DuplicateOutPort AST.ModuleInst NetAST.NodeId
    | UndefinedOutPort AST.ModuleInst NetAST.NodeId
    | DuplicateIdentifer NetAST.NodeId
    | UndefinedReference NetAST.NodeId

instance Show FailedCheck where
    show (DuplicateInPort modInst ident) = concat ["Multiple declarations of input port '", NetAST.name ident, "' in '", show modInst, "'"]
    show (UndefinedInPort modInst ident) = concat ["'", NetAST.name ident, "' is not an input port in '", show modInst, "'"]
    show (DuplicateOutPort modInst ident) = concat ["Multiple declarations of output port '", NetAST.name ident, "' in '", show modInst, "'"]
    show (UndefinedOutPort modInst ident) = concat ["'", NetAST.name ident, "' is not an output port in '", show modInst, "'"]
    show (DuplicateIdentifer ident)   = concat ["Multiple declarations of node '", show ident, "'"]
    show (UndefinedReference ident)   = concat ["Reference to undefined node '", show ident, "'"]

newtype CheckFailure = CheckFailure
    { failures :: [FailedCheck] }

instance Show CheckFailure where
    show (CheckFailure fs) = unlines $ map show fs

data Context = Context
    { spec         :: AST.SockeyeSpec
    , curNamespace :: NetAST.Namespace
    , paramValues  :: Map String Word
    , varValues    :: Map String Word
    }

sockeyeBuildNet :: AST.SockeyeSpec -> Either CheckFailure NetAST.NetSpec
sockeyeBuildNet ast = do
    let
        context = Context
            { spec         = ast
            , curNamespace = NetAST.Namespace []
            , paramValues  = Map.empty
            , varValues    = Map.empty
            }
        emptySpec = AST.SockeyeSpec Map.empty
        rootInst = AST.ModuleInst
            { AST.namespace  = AST.SimpleIdent ""
            , AST.moduleName = "@root"
            , AST.arguments  = Map.empty
            , AST.inPortMap  = []
            , AST.outPortMap = []
            }
        (netRoot:[], simpleAST) = runState (simplify context rootInst) emptySpec
    trace (groom simpleAST) $ return ()
    net <- transform context (netRoot :: AST.ModuleInst)
    let
        nodeIds = map fst net
    checkDuplicates nodeIds DuplicateIdentifer
    let
        nodeMap = Map.fromList net
        symbols = Map.keysSet nodeMap
        netSpec = NetAST.NetSpec $ nodeMap
    check symbols netSpec
    return netSpec

--
-- Simplify AST (instantiate module templates, expand for constructs)
--
class ASTSimplifiable a b where
    simplify :: Context -> a -> State AST.SockeyeSpec b

instance ASTSimplifiable AST.Module AST.Module where
    simplify context ast = do
        let
            inPorts = AST.inputPorts ast
            outPorts = AST.outputPorts ast
            nodeDecls = AST.nodeDecls ast
            moduleInsts = AST.moduleInsts ast
        simpleInPorts <- simplify context inPorts
        simpleOutPorts <- simplify context outPorts
        simpleDecls <- simplify context nodeDecls
        simpleInsts <- simplify context moduleInsts
        return AST.Module
            { AST.paramNames   = []
            , AST.paramTypeMap = Map.empty
            , AST.inputPorts   = concat (simpleInPorts :: [[AST.Port]])
            , AST.outputPorts  = concat (simpleOutPorts :: [[AST.Port]])
            , AST.nodeDecls    = concat (simpleDecls :: [[AST.NodeDecl]])
            , AST.moduleInsts  = concat (simpleInsts :: [[AST.ModuleInst]])
            }

instance ASTSimplifiable AST.Port [AST.Port] where
    simplify context (AST.MultiPort for) = do
        simpleFor <- simplify context for
        return $ concat (simpleFor :: [[AST.Port]])
    simplify context (AST.Port ident) = do
        simpleIdent <- simplify context ident
        return [AST.Port simpleIdent]

instance ASTSimplifiable AST.ModuleInst [AST.ModuleInst] where
    simplify context (AST.MultiModuleInst for) = do 
        simpleFor <- simplify context for
        return $ concat (simpleFor :: [[AST.ModuleInst]])
    simplify context ast = do
        let
            namespace = AST.namespace ast
            name = AST.moduleName ast
            args = AST.arguments ast
            inPortMap = AST.inPortMap ast
            outPortMap = AST.outPortMap ast
            mod = getModule context name
        simpleNS <- simplify context namespace
        simpleInMap <- simplify context inPortMap
        simpleOutMap <- simplify context outPortMap
        simpleArgs <- simplify context args
        simpleModule <- simplify (moduleContext simpleArgs) mod
        let
            simpleName = concat [name, "(", intercalate ", " $ argValues simpleArgs mod, ")"]
            simplified = AST.ModuleInst
                { AST.namespace  = simpleNS
                , AST.moduleName = simpleName
                , AST.arguments  = Map.empty
                , AST.inPortMap  = concat (simpleInMap :: [[AST.PortMap]])
                , AST.outPortMap = concat (simpleOutMap :: [[AST.PortMap]])
                }
        modify $ addModule simpleName simpleModule
        return [simplified]
        where
            moduleContext paramValues = context
                    { paramValues = paramValues
                    , varValues   = Map.empty
                    }
            argValues args mod =
                let
                    paramNames = AST.paramNames mod
                    paramTypes = AST.paramTypeMap mod
                    params = map (\p -> (p, paramTypes Map.! p)) paramNames
                in map showValue params
                    where
                        showValue (name, AST.AddressParam)  = "0x" ++ showHex (args Map.! name) ""
                        showValue (name, AST.NaturalParam) = show (args Map.! name)
            addModule k m spec =
                let
                    prevMods = AST.modules spec
                    newMods = Map.insert k m prevMods
                in spec
                    { AST.modules = newMods }

instance ASTSimplifiable AST.ModuleArg Word where
    simplify _ (AST.AddressArg v) = return v
    simplify _ (AST.NaturalArg v) = return v
    simplify context (AST.ParamArg name) = return $ getParamValue context name

instance ASTSimplifiable AST.PortMap [AST.PortMap] where
    simplify context (AST.MultiPortMap for) = do
        simpleFor <- simplify context for
        return $ concat (simpleFor :: [[AST.PortMap]])
    simplify context ast = do
        let
            mappedId = AST.mappedId ast
            mappedPort = AST.mappedPort ast
        simpleId <- simplify context mappedId
        simplePort <- simplify context mappedPort
        let
            simplePortMap = AST.PortMap
                { AST.mappedId   = simpleId
                , AST.mappedPort = simplePort
                }
        return [simplePortMap]

instance ASTSimplifiable AST.NodeDecl [AST.NodeDecl] where
    simplify context (AST.MultiNodeDecl for) = do
        simpleFor <- simplify context for
        return $ concat (simpleFor :: [[AST.NodeDecl]])
    simplify context ast = do
        let
            nodeId = AST.nodeId ast
            nodeSpec = AST.nodeSpec ast
        simpleNodeId <- simplify context nodeId
        simpleNodeSpec <- simplify context nodeSpec
        let
            simpleNodeDecl = AST.NodeDecl
                { AST.nodeId   = simpleNodeId
                , AST.nodeSpec = simpleNodeSpec
                }
        return [simpleNodeDecl]

instance ASTSimplifiable AST.Identifier AST.Identifier where
    simplify context ast = do
        let
            name = simpleName ast
        return $ AST.SimpleIdent name
        where
            simpleName (AST.SimpleIdent name) = name
            simpleName ident =
                let
                    prefix = AST.prefix ident
                    varName = AST.varName ident
                    suffix = AST.suffix ident
                    varValue = show $ getVarValue context varName
                    suffixName = case suffix of
                        Nothing -> ""
                        Just s  -> simpleName s
                in prefix ++ varValue ++ suffixName

instance ASTSimplifiable AST.NodeSpec AST.NodeSpec where
    simplify context ast = do
        let
            nodeType = AST.nodeType ast
            accept = AST.accept ast
            translate = AST.translate ast
            overlay = AST.overlay ast
        simpleAccept <- simplify context accept
        simpleTranslate <- simplify context translate
        simpleOverlay <- maybe (return Nothing) simplifyOverlay overlay
        return AST.NodeSpec
            { AST.nodeType  = nodeType
            , AST.accept    = simpleAccept
            , AST.translate = simpleTranslate
            , AST.overlay   = simpleOverlay
            }
        where
            simplifyOverlay ident = do
                simpleIdent <- simplify context ident
                return $ Just simpleIdent

instance ASTSimplifiable AST.BlockSpec AST.BlockSpec where
    simplify context (AST.SingletonBlock base) = do
        simpleBase <- simplify context base
        return $ AST.SingletonBlock simpleBase
    simplify context (AST.RangeBlock base limit) = do
        simpleBase <- simplify context base
        simpleLimit <- simplify context limit
        return AST.RangeBlock
            { AST.base  = simpleBase
            , AST.limit = simpleLimit
            }
    simplify context (AST.LengthBlock base bits) = do
        simpleBase <- simplify context base
        return AST.LengthBlock
            { AST.base = simpleBase
            , AST.bits = bits
            }

instance ASTSimplifiable AST.MapSpec AST.MapSpec where
    simplify context ast = do
        let
            block = AST.block ast
            destNode = AST.destNode ast
            destBase = fromMaybe (AST.base block) (AST.destBase ast)
        simpleBlock <- simplify context block
        simpleDestNode <- simplify context destNode
        simpleDestBase <- simplify context destBase
        return AST.MapSpec
            { AST.block    = simpleBlock
            , AST.destNode = simpleDestNode
            , AST.destBase = Just simpleDestBase
            }

instance ASTSimplifiable AST.Address AST.Address where
    simplify context (AST.ParamAddress name) = do
        let value = getParamValue context name
        return $ AST.LiteralAddress value
    simplify _ ast = return ast

instance ASTSimplifiable a b => ASTSimplifiable (AST.For a) [b] where
    simplify context ast = do
        let
            body = AST.body ast
            varRanges = AST.varRanges ast
        concreteRanges <- simplify context varRanges
        let
            valueList = Map.foldWithKey iterations [] concreteRanges
            iterContexts = map iterationContext valueList
        mapM (\c -> simplify c body) iterContexts
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

instance ASTSimplifiable AST.ForRange [Word] where
    simplify context ast = do
        let
            start = AST.start ast
            end = AST.end ast
        simpleStart <- simplify context start
        simpleEnd <- simplify context end
        return [simpleStart..simpleEnd]

instance ASTSimplifiable AST.ForLimit Word where
    simplify _ (AST.LiteralLimit value) = return value
    simplify context (AST.ParamLimit name) = return $ getParamValue context name

instance (Traversable t, ASTSimplifiable a b) => ASTSimplifiable (t a) (t b) where
    simplify context ast = mapM (simplify context) ast

--            
-- Build net
--
class NetTransformable a b where
    transform :: Context -> a -> Either CheckFailure b

instance NetTransformable AST.Module NetList where
    transform context ast = return []

instance NetTransformable AST.ModuleInst NetList where
    transform context ast = do
        let
            namespace = AST.namespace ast
            name = AST.moduleName ast
            inPortMap = AST.inPortMap ast
            outPortMap = AST.outPortMap ast
            mod = getModule context name
            modContext = moduleContext namespace
        transform modContext mod
            where
                moduleContext namespace =
                    let
                        curNS = NetAST.ns $ curNamespace context
                        newNS = case namespace of
                            AST.SimpleIdent "" -> NetAST.Namespace curNS
                            AST.SimpleIdent ns  -> NetAST.Namespace $ ns:curNS
                    in context
                        { curNamespace = newNS }

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
--         return [(netMappedId, netMappedPort)]

-- instance NetTransformable AST.ModuleArg Word where
--     transform context (AST.AddressArg value) = return value
--     transform context (AST.NaturalArg value) = return value
--     transform context (AST.ParamArg name) = return $ getParamValue context name

-- instance NetTransformable AST.Identifier NetAST.NodeId where
--     transform context ast = do
--         let
--             namespace = curNamespace context
--             name = identToName context ast
--         return NetAST.NodeId
--             { NetAST.namespace = namespace
--             , NetAST.name      = name
--             }

-- instance NetTransformable AST.NodeDecl NetList where
--     transform context (AST.MultiNodeDecl for) = do
--         ts <- transform context for
--         return $ concat (ts :: [NetList])
--     transform context ast = do
--         let
--             ident = AST.nodeId ast
--             nodeSpec = AST.nodeSpec ast
--         nodeId <- transform context ident
--         netNodeSpec <- transform context nodeSpec
--         return [(nodeId, netNodeSpec)]

-- instance NetTransformable AST.NodeSpec NetAST.NodeSpec where
--     transform context ast = do
--         let
--             nodeType = AST.nodeType ast
--             accept = AST.accept ast
--             translate = AST.translate ast
--             overlay = AST.overlay ast
--         netNodeType <- maybe (return NetAST.Other) (transform context) nodeType
--         netAccept <- transform context accept
--         netTranslate <- transform context translate
--         netOverlay <- case overlay of
--                 Nothing -> return Nothing
--                 Just o  -> do 
--                     t <- transform context o
--                     return $ Just t
--         return NetAST.NodeSpec
--             { NetAST.nodeType  = netNodeType
--             , NetAST.accept    = netAccept
--             , NetAST.translate = netTranslate
--             , NetAST.overlay   = netOverlay
--             }

-- instance NetTransformable AST.NodeType NetAST.NodeType where
--     transform _ AST.Memory = return NetAST.Memory
--     transform _ AST.Device = return NetAST.Device

-- instance NetTransformable AST.BlockSpec NetAST.BlockSpec where
--     transform context (AST.SingletonBlock address) = do
--         netAddress <- transform context address
--         return NetAST.BlockSpec
--             { NetAST.base  = netAddress
--             , NetAST.limit = netAddress
--             }
--     transform context (AST.RangeBlock base limit) = do
--         netBase <- transform context base
--         netLimit <- transform context limit
--         return NetAST.BlockSpec
--             { NetAST.base  = netBase
--             , NetAST.limit = netLimit
--             }
--     transform context (AST.LengthBlock base bits) = do
--         netBase <- transform context base
--         let
--             baseAddress = NetAST.address netBase
--             limit = baseAddress + 2^bits - 1
--             netLimit = NetAST.Address limit
--         return NetAST.BlockSpec
--             { NetAST.base  = netBase
--             , NetAST.limit = netLimit
--             }

-- instance NetTransformable AST.MapSpec NetAST.MapSpec where
--     transform context ast = do
--         let
--             block = AST.block ast
--             destNode = AST.destNode ast
--             destBase = fromMaybe (AST.base block) (AST.destBase ast)
--         netBlock <- transform context block
--         netDestNode <- transform context destNode
--         netDestBase <- transform context destBase
--         return NetAST.MapSpec
--             { NetAST.srcBlock = netBlock
--             , NetAST.destNode = netDestNode
--             , NetAST.destBase = netDestBase
--             }

-- instance NetTransformable AST.Address NetAST.Address where
--     transform _ (AST.LiteralAddress value) = do
--         return $ NetAST.Address value
--     transform context (AST.ParamAddress name) = do
--         let
--             value = getParamValue context name
--         return $ NetAST.Address value

-- instance NetTransformable a b => NetTransformable (AST.For a) [b] where
--     transform context ast = do
--         let
--             body = AST.body ast
--             varRanges = AST.varRanges ast
--         concreteRanges <- transform context varRanges
--         let
--             valueList = Map.foldWithKey iterations [] concreteRanges
--             iterContexts = map iterationContext valueList
--             ts = map (\c -> transform c body) iterContexts
--             fs = lefts ts
--             bs = rights ts
--         case fs of
--             [] -> return $ bs
--             _  -> Left $ CheckFailure (concat $ map failures fs)
--         where
--             iterations k vs [] = [Map.fromList [(k,v)] | v <- vs]
--             iterations k vs ms = concat $ map (f ms k) vs
--                 where
--                     f ms k v = map (Map.insert k v) ms
--             iterationContext varMap =
--                 let
--                     values = varValues context
--                 in context
--                     { varValues = values `Map.union` varMap }

-- instance NetTransformable AST.ForRange [Word] where
--     transform context ast = do
--         let
--             start = AST.start ast
--             end = AST.end ast
--         startVal <- transform context start
--         endVal <- transform context end
--         return [startVal..endVal]

-- instance NetTransformable AST.ForLimit Word where
--     transform _ (AST.LiteralLimit value) = return value
--     transform context (AST.ParamLimit name) = return $ getParamValue context name

-- instance NetTransformable a b => NetTransformable [a] [b] where
--     transform context ast = do
--         let
--             ts = map (transform context) ast
--             fs = lefts ts
--             bs = rights ts
--         case fs of
--             [] -> return bs
--             _  -> Left $ CheckFailure (concat $ map failures fs)

-- instance (Ord k, NetTransformable a b) => NetTransformable (Map k a) (Map k b) where
--     transform context ast = do
--         let
--             ks = Map.keys ast
--             es = Map.elems ast
--         ts <- transform context es
--         return $ Map.fromList (zip ks ts)

--
-- Checks
--
class NetCheckable a where
    check :: Set NetAST.NodeId -> a -> Either CheckFailure ()

instance NetCheckable NetAST.NetSpec where
    check context (NetAST.NetSpec net) = do
        check context $ Map.elems net

instance NetCheckable NetAST.NodeSpec where
    check context (NetAST.AliasSpec alias) = do
        case alias of
            Nothing -> return ()
            Just ident -> check context ident
    check context net = do
        let
            translate = NetAST.translate net
            overlay = NetAST.overlay net
        check context translate
        maybe (return ()) (check context) overlay

instance NetCheckable NetAST.MapSpec where
    check context net = do
        let
           destNode = NetAST.destNode net
        check context destNode

instance NetCheckable NetAST.NodeId where
    check context net = do
        if net `Set.member` context
            then return ()
            else Left $ CheckFailure [UndefinedReference net]

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

getParamValue :: Context -> String -> Word
getParamValue context name =
    let
        params = paramValues context
    in params Map.! name

getVarValue :: Context -> String -> Word
getVarValue context name =
    let
        vars = varValues context
    in vars Map.! name

checkDuplicates :: [NetAST.NodeId] -> (NetAST.NodeId -> FailedCheck) -> Either CheckFailure ()
checkDuplicates nodeIds fail = do
    let
        duplicates = duplicateNames nodeIds
    case duplicates of
        [] -> return ()
        _  -> Left $ CheckFailure (map fail duplicates)
    where
        duplicateNames [] = []
        duplicateNames (x:xs)
            | x `elem` xs = nub $ [x] ++ duplicateNames xs
            | otherwise = duplicateNames xs
        msg (NetAST.NodeId namespace name) =
            let
                m = concat ["Multiple declarations of node '", name, "'"]
            in case NetAST.ns namespace of
                [] -> m
                _  -> m ++ concat [" in namespace '", show namespace, "'"]
