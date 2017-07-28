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
( sockeyeInstantiate ) where

import Control.Monad.State

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Numeric (showHex)

import SockeyeChecks

import qualified SockeyeAST as AST
import qualified SockeyeASTInstantiator as InstAST

import Text.Groom (groom)
import Debug.Trace

data InstFails
    = ModuleInstLoop     [String]
    | DuplicateNamespace !String
    | DuplicateIdentifer !String
    | DuplicateInPort    !String
    | DuplicateOutPort   !String
    | DuplicateInMap     !String !String
    | DuplicateOutMap    !String !String
    | UndefinedOutPort   !String !String
    | UndefinedInPort    !String !String
    | UndefinedReference !String !String

instance Show InstFails where
    show (ModuleInstLoop     loop)       = concat ["Module instantiation loop: '", intercalate "' -> '" loop, "'"]
    show (DuplicateInPort    port)       = concat ["Multiple declarations of input port '", port, "'"]
    show (DuplicateOutPort   port)       = concat ["Multiple declarations of output port '", port, "'"]
    show (DuplicateNamespace ident)      = concat ["Multiple usage of namespace '", ident, "'"]
    show (DuplicateIdentifer ident)      = concat ["Multiple declarations of node '", ident, "'"]
    show (DuplicateInMap   inst port)    = concat ["Multiple mappings for input port '",  port, "' in module instantiation '", inst, "'"]
    show (DuplicateOutMap  inst port)    = concat ["Multiple mappings for output port '", port, "' in module instantiation '", inst, "'"]
    show (UndefinedInPort  inst port)    = concat ["Mapping to undefined input port '",   port, "' in module instantiation '", inst, "'"]
    show (UndefinedOutPort inst port)    = concat ["Mapping to undefined output port '",  port, "' in module instantiation '", inst, "'"]
    show (UndefinedReference decl ident) = concat ["Reference to undefined node '", ident, "' in declaration of node '", decl, "'"]

data Context = Context
    { spec        :: AST.SockeyeSpec
    , modulePath  :: [String]
    , paramValues :: Map String Integer
    , varValues   :: Map String Integer
    }

sockeyeInstantiate :: AST.SockeyeSpec -> Either (FailedChecks InstFails) InstAST.SockeyeSpec
sockeyeInstantiate ast = do
    let
        emptySpec = AST.SockeyeSpec Map.empty
        context = Context
            { spec        = emptySpec
            , modulePath  = []
            , paramValues = Map.empty
            , varValues   = Map.empty
            }
    runChecks $ evalStateT (instantiate context ast) Map.empty

--
-- Instantiate Module Templates
--
class Instantiatable a b where
    instantiate :: Context -> a -> StateT (Map String InstAST.Module) (Checks InstFails) b

instance Instantiatable AST.SockeyeSpec InstAST.SockeyeSpec where
    instantiate context ast = do
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
        [("", instRoot)] <- instantiate specContext rootInst
        modules <- get
        return InstAST.SockeyeSpec
            { InstAST.root = instRoot
            , InstAST.modules = modules
            }

instance Instantiatable AST.Module InstAST.Module where
    instantiate context ast = do
        let
            inPorts = AST.inputPorts ast
            outPorts = AST.outputPorts ast
            nodeDecls = AST.nodeDecls ast
            moduleInsts = AST.moduleInsts ast
            modName = head $ modulePath context
        modules <- get
        if modName `Map.member` modules
            then do
                return $ modules Map.! modName
            else do
                let sentinel = InstAST.Module
                        { InstAST.inputPorts   = Map.empty
                        , InstAST.outputPorts  = Map.empty
                        , InstAST.nodeDecls    = Map.empty
                        , InstAST.moduleInsts  = Map.empty
                        }
                modify $ Map.insert modName sentinel
                instInPorts <- do
                    instPorts <- instantiate context inPorts
                    return $ concat (instPorts :: [[(String, Integer)]])
                instOutPorts <- do
                    instPorts <- instantiate context outPorts
                    return $ concat (instPorts :: [[(String, Integer)]])
                instDecls <- do
                    decls <- instantiate context nodeDecls
                    return $ concat (decls :: [[(String, InstAST.NodeSpec)]])
                instInsts <- do
                    insts <- instantiate context moduleInsts
                    return $ concat (insts :: [[(String, InstAST.ModuleInst)]])
                lift $ checkDuplicates modName DuplicateInPort    $ (map fst instInPorts)
                lift $ checkDuplicates modName DuplicateOutPort   $ (map fst instOutPorts)
                lift $ checkDuplicates modName DuplicateIdentifer $ (map fst instDecls)
                lift $ checkDuplicates modName DuplicateNamespace $ (map fst instInsts)
                return InstAST.Module
                    { InstAST.inputPorts   = Map.fromList instInPorts
                    , InstAST.outputPorts  = Map.fromList instOutPorts
                    , InstAST.nodeDecls    = Map.fromList instDecls
                    , InstAST.moduleInsts  = Map.fromList instInsts
                    }

instance Instantiatable AST.Port [(String, Integer)] where
    instantiate context (AST.MultiPort for) = do
        instFor <- instantiate context for
        return $ concat (instFor :: [[(String, Integer)]])
    instantiate context ast = do
        let
            ident = AST.portId ast
            width = AST.portWidth ast
        instIdent <- instantiate context ident
        return [(instIdent, width)]

instance Instantiatable AST.ModuleInst [(String, InstAST.ModuleInst)] where
    instantiate context (AST.MultiModuleInst for) = do 
        simpleFor <- instantiate context for
        return $ concat (simpleFor :: [[(String, InstAST.ModuleInst)]])
    instantiate context ast = do
        let
            namespace = AST.namespace ast
            name = AST.moduleName ast
            args = AST.arguments ast
            inPortMap = AST.inPortMap ast
            outPortMap = AST.outPortMap ast
            modPath = modulePath context
            mod = getModule context name
        instNs <- instantiate context namespace
        instInMap <- do
            inMaps <- instantiate context inPortMap
            return $ concat (inMaps :: [[(String, String)]])
        instOutMap <- do
            outMaps <- instantiate context outPortMap
            return $ concat (outMaps :: [[(String, String)]])
        instArgs <- instantiate context args
        let
            instName = concat [name, "(", intercalate ", " $ argStrings instArgs mod, ")"]
            moduleContext = context
                    { modulePath  = instName:modPath
                    , paramValues = instArgs
                    , varValues   = Map.empty
                    }
        lift $ checkSelfInst modPath instName
        lift $ checkDuplicates name (DuplicateInMap  instName) $ map fst instInMap
        lift $ checkDuplicates name (DuplicateOutMap instName) $ map fst instOutMap
        let
            simplified = InstAST.ModuleInst
                { InstAST.moduleName = instName
                , InstAST.inPortMap  = Map.fromList instInMap
                , InstAST.outPortMap = Map.fromList instOutMap
                }
        instModule <- instantiate moduleContext mod
        modify $ Map.insert instName instModule
        return [(instNs, simplified)]
        where
            argStrings args mod =
                let
                    paramNames = AST.paramNames mod
                    paramTypes = AST.paramTypeMap mod
                    params = map (\p -> (p, paramTypes Map.! p)) paramNames
                in map showValue params
                    where
                        showValue (name, AST.AddressParam)  = "0x" ++ showHex (args Map.! name) ""
                        showValue (name, AST.NaturalParam) = show (args Map.! name)
            checkSelfInst path name = do
                case loop path of
                    [] -> return ()
                    l  -> failCheck "" $ ModuleInstLoop (reverse $ name:l)
                    where
                        loop [] = []
                        loop path@(p:ps)
                            | name `elem` path = p:(loop ps)
                            | otherwise = []

instance Instantiatable AST.ModuleArg Integer where
    instantiate _ (AST.AddressArg v) = return v
    instantiate _ (AST.NaturalArg v) = return v
    instantiate context (AST.ParamArg name) = return $ getParamValue context name

instance Instantiatable AST.PortMap [(String, String)] where
    instantiate context (AST.MultiPortMap for) = do
        instFor <- instantiate context for
        return $ concat (instFor :: [[(String, String)]])
    instantiate context ast = do
        let
            mappedId = AST.mappedId ast
            mappedPort = AST.mappedPort ast
        instId <- instantiate context mappedId
        instPort <- instantiate context mappedPort
        return [(instPort, instId)]

instance Instantiatable AST.NodeDecl [(String, InstAST.NodeSpec)] where
    instantiate context (AST.MultiNodeDecl for) = do
        instFor <- instantiate context for
        return $ concat (instFor :: [[(String, InstAST.NodeSpec)]])
    instantiate context ast = do
        let
            nodeId = AST.nodeId ast
            nodeSpec = AST.nodeSpec ast
        instNodeId <- instantiate context nodeId
        instNodeSpec <- instantiate context nodeSpec
        return [(instNodeId, instNodeSpec)]

instance Instantiatable AST.Identifier String where
    instantiate context (AST.SimpleIdent name) = do
        return name
    instantiate context ast = do
        let
            prefix = AST.prefix ast
            varName = AST.varName ast
            suffix = AST.suffix ast
            varValue = show $ getVarValue context varName
        suffixName <- case suffix of
            Nothing -> return ""
            Just s  -> instantiate context s
        return $ prefix ++ varValue ++ suffixName

instance Instantiatable AST.NodeSpec InstAST.NodeSpec where
    instantiate context ast = do
        let
            nodeType = AST.nodeType ast
            accept = AST.accept ast
            translate = AST.translate ast
            reserved = AST.reserved ast
            overlay = AST.overlay ast
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

instance Instantiatable AST.NodeType InstAST.NodeType where
    instantiate _ AST.Memory = return InstAST.Memory
    instantiate _ AST.Device = return InstAST.Device
    instantiate _ AST.Other  = return InstAST.Other

instance Instantiatable AST.BlockSpec InstAST.BlockSpec where
    instantiate context (AST.SingletonBlock base) = do
        instBase <- instantiate context base
        return InstAST.BlockSpec
            { InstAST.base  = instBase
            , InstAST.limit = instBase
            }
    instantiate context (AST.RangeBlock base limit) = do
        instBase <- instantiate context base
        instLimit <- instantiate context limit
        return InstAST.BlockSpec
            { InstAST.base  = instBase
            , InstAST.limit = instLimit
            }
    instantiate context (AST.LengthBlock base bits) = do
        instBase <- instantiate context base
        let
            instLimit = instBase + 2^bits - 1
        return InstAST.BlockSpec
            { InstAST.base  = instBase
            , InstAST.limit = instLimit
            }

instance Instantiatable AST.MapSpec InstAST.MapSpec where
    instantiate context ast = do
        let
            block = AST.block ast
            destNode = AST.destNode ast
            destBase = fromMaybe (AST.base block) (AST.destBase ast)
        instBlock <- instantiate context block
        instDestNode <- instantiate context destNode
        instDestBase <- instantiate context destBase
        return InstAST.MapSpec
            { InstAST.block    = instBlock
            , InstAST.destNode = instDestNode
            , InstAST.destBase = instDestBase
            }

instance Instantiatable AST.OverlaySpec InstAST.OverlaySpec where
    instantiate context ast = do
        let
            over = AST.over ast
            width = AST.width ast
        instOver <- instantiate context over
        return InstAST.OverlaySpec
            { InstAST.over  = instOver
            , InstAST.width = width
            }

instance Instantiatable AST.Address Integer where
    instantiate context (AST.ParamAddress name) = do
        let value = getParamValue context name
        return value
    instantiate _ (AST.LiteralAddress value) = return value

instance Instantiatable a b => Instantiatable (AST.For a) [b] where
    instantiate context ast = do
        let
            body = AST.body ast
            varRanges = AST.varRanges ast
        concreteRanges <- instantiate context varRanges
        let
            valueList = Map.foldWithKey iterations [] concreteRanges
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

instance Instantiatable AST.ForRange [Integer] where
    instantiate context ast = do
        let
            start = AST.start ast
            end = AST.end ast
        simpleStart <- instantiate context start
        simpleEnd <- instantiate context end
        return [simpleStart..simpleEnd]

instance Instantiatable AST.ForLimit Integer where
    instantiate _ (AST.LiteralLimit value) = return value
    instantiate context (AST.ParamLimit name) = return $ getParamValue context name

instance (Traversable t, Instantiatable a b) => Instantiatable (t a) (t b) where
    instantiate context ast = mapM (instantiate context) ast


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
