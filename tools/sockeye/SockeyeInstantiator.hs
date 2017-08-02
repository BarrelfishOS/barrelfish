{-
    SockeyeModuleInstantiator.hs: Module instantiator for Sockeye

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

module SockeyeInstantiator
( instantiateSockeye ) where

import Control.Monad.State

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Numeric (showHex)

import SockeyeChecks

import qualified SockeyeASTTypeChecker as CheckAST
import qualified SockeyeASTInstantiator as InstAST

data InstFail
    = ModuleInstLoop     [String]
    | DuplicateNamespace !String
    | DuplicateIdentifer !String
    | DuplicateInPort    !String
    | DuplicateOutPort   !String
    | DuplicateInMap     !String !String
    | DuplicateOutMap    !String !String

instance Show InstFail where
    show (ModuleInstLoop     loop)       = concat ["Module instantiation loop: '", intercalate "' -> '" loop, "'"]
    show (DuplicateInPort    port)       = concat ["Multiple declarations of input port '", port, "'"]
    show (DuplicateOutPort   port)       = concat ["Multiple declarations of output port '", port, "'"]
    show (DuplicateNamespace ident)      = concat ["Multiple usage of namespace '", ident, "'"]
    show (DuplicateIdentifer ident)      = concat ["Multiple declarations of node '", ident, "'"]
    show (DuplicateInMap   inst port)    = concat ["Multiple mappings for input port '",  port, "' in module instantiation '", inst, "'"]
    show (DuplicateOutMap  inst port)    = concat ["Multiple mappings for output port '", port, "' in module instantiation '", inst, "'"]

type PortMapping = (InstAST.Identifier, InstAST.Identifier)

data Context = Context
    { modules     :: Map String CheckAST.Module
    , modulePath  :: [String]
    , paramValues :: Map String Integer
    , varValues   :: Map String Integer
    }

instantiateSockeye :: CheckAST.SockeyeSpec -> Either (FailedChecks InstFail) InstAST.SockeyeSpec
instantiateSockeye ast = do
    let context = Context
            { modules     = Map.empty
            , modulePath  = []
            , paramValues = Map.empty
            , varValues   = Map.empty
            }
    runChecks $ evalStateT (instantiate context ast) Map.empty

--
-- Instantiate Module Templates
--
class Instantiatable a b where
    instantiate :: Context -> a -> StateT (Map String InstAST.Module) (Checks InstFail) b

instance Instantiatable CheckAST.SockeyeSpec InstAST.SockeyeSpec where
    instantiate context ast = do
        let root = CheckAST.root ast
            mods  = CheckAST.modules ast
            specContext = context
                { modules = mods }
        [instRoot] <- instantiate specContext root
        modules <- get
        return InstAST.SockeyeSpec
            { InstAST.root = instRoot
            , InstAST.modules = modules
            }

instance Instantiatable CheckAST.Module InstAST.Module where
    instantiate context ast = do
        let ports = CheckAST.ports ast
            moduleInsts = CheckAST.moduleInsts ast
            nodeDecls = CheckAST.nodeDecls ast
            modName = head $ modulePath context
        modules <- get
        if modName `Map.member` modules
            then do
                return $ modules Map.! modName
            else do
                let sentinel = InstAST.Module
                        { InstAST.inputPorts  = []
                        , InstAST.outputPorts = []
                        , InstAST.nodeDecls   = []
                        , InstAST.moduleInsts = []
                        }
                modify $ Map.insert modName sentinel
                (instInPorts, instOutPorts) <- do
                    instPorts <- instantiate context ports
                    let allPorts = concat (instPorts :: [[InstAST.Port]])
                        inPorts = filter isInPort allPorts
                        outPorts = filter isOutPort allPorts
                    return (inPorts, outPorts)
                instInsts <- do
                    insts <- instantiate context moduleInsts
                    return $ concat (insts :: [[InstAST.ModuleInst]])
                instDecls <- do
                    decls <- instantiate context nodeDecls
                    return $ concat (decls :: [[InstAST.NodeDecl]])
                let
                    inPortIds = map InstAST.portId instInPorts
                    outPortIds = map InstAST.portId instOutPorts
                    inMapNodeIds = concat $ map (Map.elems . InstAST.inPortMap) instInsts
                    declNodeIds = map InstAST.nodeId instDecls
                lift $ checkDuplicates modName DuplicateInPort  inPortIds
                lift $ checkDuplicates modName DuplicateOutPort outPortIds
                lift $ checkDuplicates modName DuplicateNamespace $ (map InstAST.namespace instInsts)
                lift $ checkDuplicates modName DuplicateIdentifer $ outPortIds ++ inMapNodeIds ++ declNodeIds
                return InstAST.Module
                    { InstAST.inputPorts  = instInPorts
                    , InstAST.outputPorts = instOutPorts
                    , InstAST.nodeDecls   = instDecls
                    , InstAST.moduleInsts = instInsts
                    }
        where
            isInPort  (InstAST.InputPort  {}) = True
            isInPort  (InstAST.OutputPort {}) = False
            isOutPort (InstAST.InputPort  {}) = False
            isOutPort (InstAST.OutputPort {}) = True

instance Instantiatable CheckAST.Port [InstAST.Port] where
    instantiate context (CheckAST.MultiPort for) = do
        instFor <- instantiate context for
        return $ concat (instFor :: [[InstAST.Port]])
    instantiate context ast@(CheckAST.InputPort {}) = do
        let ident = CheckAST.portId ast
            width = CheckAST.portWidth ast
        instIdent <- instantiate context ident
        let instPort = InstAST.InputPort
                { InstAST.portId    = instIdent
                , InstAST.portWidth = width
                }
        return [instPort]
    instantiate context ast@(CheckAST.OutputPort {}) = do
        let ident = CheckAST.portId ast
            width = CheckAST.portWidth ast
        instIdent <- instantiate context ident
        let instPort = InstAST.OutputPort
                { InstAST.portId    = instIdent
                , InstAST.portWidth = width
                }
        return [instPort]

instance Instantiatable CheckAST.ModuleInst [InstAST.ModuleInst] where
    instantiate context (CheckAST.MultiModuleInst for) = do 
        simpleFor <- instantiate context for
        return $ concat (simpleFor :: [[InstAST.ModuleInst]])
    instantiate context ast = do
        let namespace = CheckAST.namespace ast
            name = CheckAST.moduleName ast
            args = CheckAST.arguments ast
            inPortMap = CheckAST.inPortMap ast
            outPortMap = CheckAST.outPortMap ast
            modPath = modulePath context
            mod = getModule context name
        instNs <- instantiate context namespace
        instInMap <- do
            inMaps <- instantiate context inPortMap
            return $ concat (inMaps :: [[PortMapping]])
        instOutMap <- do
            outMaps <- instantiate context outPortMap
            return $ concat (outMaps :: [[PortMapping]])
        instArgs <- instantiate context args
        let instName = concat [name, "(", intercalate ", " $ argStrings instArgs mod, ")"]
            moduleContext = context
                    { modulePath  = instName:modPath
                    , paramValues = instArgs
                    , varValues   = Map.empty
                    }
        lift $ checkSelfInst modPath instName
        lift $ checkDuplicates (head modPath) (DuplicateInMap  instName) $ map fst instInMap
        lift $ checkDuplicates (head modPath) (DuplicateOutMap instName) $ map fst instOutMap
        let instantiated = InstAST.ModuleInst
                { InstAST.moduleName = instName
                , InstAST.namespace  = instNs
                , InstAST.inPortMap  = Map.fromList instInMap
                , InstAST.outPortMap = Map.fromList instOutMap
                }
        instModule <- instantiate moduleContext mod
        modify $ Map.insert instName instModule
        return [instantiated]
        where
            argStrings args mod =
                let paramNames = CheckAST.paramNames mod
                    paramTypes = CheckAST.paramTypeMap mod
                    params = map (\p -> (p, paramTypes Map.! p)) paramNames
                in map showValue params
                    where
                        showValue (name, CheckAST.AddressParam)  = "0x" ++ showHex (args Map.! name) ""
                        showValue (name, CheckAST.NaturalParam) = show (args Map.! name)
            checkSelfInst path name = do
                case loop path of
                    [] -> return ()
                    l  -> failCheck "@all" $ ModuleInstLoop (reverse $ name:l)
                    where
                        loop [] = []
                        loop path@(p:ps)
                            | name `elem` path = p:(loop ps)
                            | otherwise = []

instance Instantiatable CheckAST.ModuleArg Integer where
    instantiate _ (CheckAST.AddressArg value) = return value
    instantiate _ (CheckAST.NaturalArg value) = return value
    instantiate context (CheckAST.ParamArg name) = return $ getParamValue context name

instance Instantiatable CheckAST.PortMap [PortMapping] where
    instantiate context (CheckAST.MultiPortMap for) = do
        instFor <- instantiate context for
        return $ concat (instFor :: [[PortMapping]])
    instantiate context ast = do
        let mappedId = CheckAST.mappedId ast
            mappedPort = CheckAST.mappedPort ast
        instId <- instantiate context mappedId
        instPort <- instantiate context mappedPort
        return [(instPort, instId)]

instance Instantiatable CheckAST.NodeDecl [InstAST.NodeDecl] where
    instantiate context (CheckAST.MultiNodeDecl for) = do
        instFor <- instantiate context for
        return $ concat (instFor :: [[InstAST.NodeDecl]])
    instantiate context ast = do
        let nodeId = CheckAST.nodeId ast
            nodeSpec = CheckAST.nodeSpec ast
        instNodeId <- instantiate context nodeId
        instNodeSpec <- instantiate context nodeSpec
        let instDecl = InstAST.NodeDecl
                { InstAST.nodeId   = instNodeId
                , InstAST.nodeSpec = instNodeSpec
                }
        return $ [instDecl]

instance Instantiatable CheckAST.Identifier InstAST.Identifier where
    instantiate _ (CheckAST.SimpleIdent name) = do
        return name
    instantiate context ast = do
        let prefix = CheckAST.prefix ast
            varName = CheckAST.varName ast
            suffix = CheckAST.suffix ast
            varValue = show $ getVarValue context varName
        suffixName <- case suffix of
            Nothing -> return ""
            Just s  -> instantiate context s
        return $ prefix ++ varValue ++ suffixName

instance Instantiatable CheckAST.NodeSpec InstAST.NodeSpec where
    instantiate context ast = do
        let nodeType = CheckAST.nodeType ast
            accept = CheckAST.accept ast
            translate = CheckAST.translate ast
            reserved = CheckAST.reserved ast
            overlay = CheckAST.overlay ast
        instAccept <- instantiate context accept
        instTranslate <- instantiate context translate
        instReserved <- instantiate context reserved
        instOverlay <- maybe (return Nothing) (\o -> instantiate context o >>= return . Just) overlay
        return InstAST.NodeSpec
            { InstAST.nodeType  = nodeType
            , InstAST.accept    = instAccept
            , InstAST.translate = instTranslate
            , InstAST.reserved  = instReserved
            , InstAST.overlay   = instOverlay
            }

instance Instantiatable CheckAST.NodeType InstAST.NodeType where
    instantiate _ CheckAST.Memory = return InstAST.Memory
    instantiate _ CheckAST.Device = return InstAST.Device
    instantiate _ CheckAST.Other  = return InstAST.Other

instance Instantiatable CheckAST.BlockSpec InstAST.BlockSpec where
    instantiate context (CheckAST.SingletonBlock base) = do
        instBase <- instantiate context base
        return InstAST.BlockSpec
            { InstAST.base  = instBase
            , InstAST.limit = instBase
            }
    instantiate context (CheckAST.RangeBlock base limit) = do
        instBase <- instantiate context base
        instLimit <- instantiate context limit
        return InstAST.BlockSpec
            { InstAST.base  = instBase
            , InstAST.limit = instLimit
            }
    instantiate context (CheckAST.LengthBlock base bits) = do
        instBase <- instantiate context base
        let instLimit = instBase + 2^bits - 1
        return InstAST.BlockSpec
            { InstAST.base  = instBase
            , InstAST.limit = instLimit
            }

instance Instantiatable CheckAST.MapSpec InstAST.MapSpec where
    instantiate context ast = do
        let block = CheckAST.block ast
            destNode = CheckAST.destNode ast
            destBase = fromMaybe (CheckAST.LiteralAddress 0) (CheckAST.destBase ast)
        instBlock <- instantiate context block
        instDestNode <- instantiate context destNode
        instDestBase <- instantiate context destBase
        return InstAST.MapSpec
            { InstAST.srcBlock    = instBlock
            , InstAST.destNode = instDestNode
            , InstAST.destBase = instDestBase
            }

instance Instantiatable CheckAST.OverlaySpec InstAST.OverlaySpec where
    instantiate context ast = do
        let over = CheckAST.over ast
            width = CheckAST.width ast
        instOver <- instantiate context over
        return InstAST.OverlaySpec
            { InstAST.over  = instOver
            , InstAST.width = width
            }

instance Instantiatable CheckAST.Address InstAST.Address where
    instantiate context (CheckAST.ParamAddress name) = do
        let value = getParamValue context name
        return value
    instantiate _ (CheckAST.LiteralAddress value) = return value

instance Instantiatable a b => Instantiatable (CheckAST.For a) [b] where
    instantiate context ast = do
        let body = CheckAST.body ast
            varRanges = CheckAST.varRanges ast
        concreteRanges <- instantiate context varRanges
        let valueList = Map.foldWithKey iterations [] concreteRanges
            iterContexts = map iterationContext valueList
        mapM (\c -> instantiate c body) iterContexts
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

instance Instantiatable CheckAST.ForRange [Integer] where
    instantiate context ast = do
        let start = CheckAST.start ast
            end = CheckAST.end ast
        simpleStart <- instantiate context start
        simpleEnd <- instantiate context end
        return [simpleStart..simpleEnd]

instance Instantiatable CheckAST.ForLimit Integer where
    instantiate _ (CheckAST.LiteralLimit value) = return value
    instantiate context (CheckAST.ParamLimit name) = return $ getParamValue context name

instance (Traversable t, Instantiatable a b) => Instantiatable (t a) (t b) where
    instantiate context ast = mapM (instantiate context) ast

getModule :: Context -> String -> CheckAST.Module
getModule context name = (modules context) Map.! name

getParamValue :: Context -> String -> Integer
getParamValue context name =
    let params = paramValues context
    in params Map.! name

getVarValue :: Context -> String -> Integer
getVarValue context name =
    let vars = varValues context
    in vars Map.! name
