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

module SockeyeSimplifier
( sockeyeSimplify ) where

import Control.Monad.State

import Data.List (nub, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Numeric (showHex)

import SockeyeChecks

import qualified SockeyeAST as AST
import qualified SockeyeASTDecodingNet as NetAST

import Text.Groom (groom)
import Debug.Trace

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
    show (DuplicateOutMap   ns     port) = concat ["Multiple mappings for output port '", port, "' in '", ns, "'"]
    show (UndefinedOutPort modName port) = concat ["'", port, "' is not an output port in '", modName, "'"]
    show (DuplicateIdentifer ident) = concat ["Multiple declarations of node '", show ident, "'"]
    show (UndefinedReference ident) = concat ["Reference to undefined node '", show ident, "'"]

data Context = Context
    { spec         :: AST.SockeyeSpec
    , curNamespace :: NetAST.Namespace
    , curModule    :: !String
    , paramValues  :: Map String Integer
    , varValues    :: Map String Integer
    }

sockeyeSimplify :: AST.SockeyeSpec -> Either (CheckFailure FailedCheck) AST.SockeyeSpec
sockeyeSimplify ast = do
    let
        emptySpec = AST.SockeyeSpec Map.empty
        context = Context
            { spec         = emptySpec
            , curNamespace = NetAST.Namespace []
            , curModule    = ""
            , paramValues  = Map.empty
            , varValues    = Map.empty
            }
    runChecks $ evalStateT (simplify context ast) Map.empty
    -- let
    --     nodeIds = map fst net
    -- checkDuplicates nodeIds DuplicateIdentifer
    -- let
    --     nodeMap = Map.fromList net
    --     symbols = Map.keysSet nodeMap
    --     netSpec = NetAST.NetSpec $ nodeMap
    -- check symbols netSpec
    -- return netSpec

--
-- Simplify AST (instantiate module templates, expand for constructs)
--
class ASTSimplifiable a b where
    simplify :: Context -> a -> StateT (Map String AST.Module) (Checks FailedCheck) b

instance ASTSimplifiable AST.SockeyeSpec AST.SockeyeSpec where
    simplify context ast = do
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
        inst <- simplify specContext rootInst
        return (inst :: [AST.ModuleInst])
        modules <- get
        return AST.SockeyeSpec
            { AST.modules = modules }

instance ASTSimplifiable AST.Module AST.Module where
    simplify context ast = do
        let
            inPorts = AST.inputPorts ast
            outPorts = AST.outputPorts ast
            nodeDecls = AST.nodeDecls ast
            moduleInsts = AST.moduleInsts ast
            modName = curModule context
        simpleInPorts <- do
            simplePorts <- simplify context inPorts
            return $ concat (simplePorts :: [[AST.Port]])
        simpleOutPorts <- do
            simplePorts <- simplify context outPorts
            return $ concat (simplePorts :: [[AST.Port]])
        checkDuplicates (map (AST.prefix . AST.portId) simpleInPorts) $ DuplicateInPort modName
        checkDuplicates (map (AST.prefix . AST.portId) simpleOutPorts) $ DuplicateOutPort modName
        simpleDecls <- simplify context nodeDecls
        simpleInsts <- simplify context moduleInsts
        return AST.Module
            { AST.paramNames   = []
            , AST.paramTypeMap = Map.empty
            , AST.inputPorts   = simpleInPorts
            , AST.outputPorts  = simpleOutPorts
            , AST.nodeDecls    = concat (simpleDecls :: [[AST.NodeDecl]])
            , AST.moduleInsts  = concat (simpleInsts :: [[AST.ModuleInst]])
            }

instance ASTSimplifiable AST.Port [AST.Port] where
    simplify context (AST.MultiPort for) = do
        simpleFor <- simplify context for
        return $ concat (simpleFor :: [[AST.Port]])
    simplify context ast@(AST.InputPort {}) = do
        let
            ident = AST.portId ast
            width = AST.portWidth ast
        simpleIdent <- simplify context ident
        return [AST.InputPort
            { AST.portId    = simpleIdent
            , AST.portWidth = width
            }]
    simplify context ast@(AST.OutputPort {}) = do
        let
            ident = AST.portId ast
            width = AST.portWidth ast
        simpleIdent <- simplify context ident
        return [AST.OutputPort
            { AST.portId    = simpleIdent
            , AST.portWidth = width
            }]

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
        let
            simpleName = concat [name, "(", intercalate ", " $ argValues (simpleArgs :: Map String Integer) mod, ")"]
        simpleModule <- simplify (moduleContext simpleName simpleArgs) mod
        let
            simplified = AST.ModuleInst
                { AST.namespace  = simpleNS
                , AST.moduleName = simpleName
                , AST.arguments  = Map.empty
                , AST.inPortMap  = concat (simpleInMap :: [[AST.PortMap]])
                , AST.outPortMap = concat (simpleOutMap :: [[AST.PortMap]])
                }
        modify $ Map.insert simpleName simpleModule
        return [simplified]
        where
            moduleContext name paramValues = context
                    { curModule   = name
                    , paramValues = paramValues
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

instance ASTSimplifiable AST.ModuleArg Integer where
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
            reserved = AST.reserved ast
            overlay = AST.overlay ast
        simpleAccept <- simplify context accept
        simpleTranslate <- simplify context translate
        simpleReserved <- simplify context reserved
        -- simpleOverlay <- maybe (return Nothing) simplifyOverlay overlay
        return AST.NodeSpec
            { AST.nodeType  = nodeType
            , AST.accept    = simpleAccept
            , AST.translate = simpleTranslate
            , AST.reserved  = simpleReserved
            , AST.overlay   = Nothing --simpleOverlay
            }
        -- where
        --     simplifyOverlay ident = do
        --         simpleIdent <- simplify context ident
        --         return $ Just simpleIdent

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

instance ASTSimplifiable AST.ForRange [Integer] where
    simplify context ast = do
        let
            start = AST.start ast
            end = AST.end ast
        simpleStart <- simplify context start
        simpleEnd <- simplify context end
        return [simpleStart..simpleEnd]

instance ASTSimplifiable AST.ForLimit Integer where
    simplify _ (AST.LiteralLimit value) = return value
    simplify context (AST.ParamLimit name) = return $ getParamValue context name

instance (Traversable t, ASTSimplifiable a b) => ASTSimplifiable (t a) (t b) where
    simplify context ast = mapM (simplify context) ast


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

checkDuplicates :: (Eq a, Show a) => [a] -> (String -> FailedCheck) -> StateT (Map String AST.Module) (Checks FailedCheck) ()
checkDuplicates nodeIds fail = do
    let
        duplicates = duplicateNames nodeIds
    case duplicates of
        [] -> return ()
        _  -> lift $ mapM_ (failure . fail . show) duplicates
    where
        duplicateNames [] = []
        duplicateNames (x:xs)
            | x `elem` xs = nub $ [x] ++ duplicateNames xs
            | otherwise = duplicateNames xs
