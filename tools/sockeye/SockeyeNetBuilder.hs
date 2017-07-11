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

import Data.Either

import Data.Map (Map)
import qualified Data.Map as Map

import qualified SockeyeAST as AST
import qualified SockeyeASTDecodingNet as NetAST

import Debug.Trace

type NetList = [(NetAST.NodeId, NetAST.NodeSpec)]

newtype CheckFailure = CheckFailure
    { message :: String }

instance Show CheckFailure where
    show f = unlines $ ["", message f]

data Context = Context
    { spec        :: AST.SockeyeSpec
    , paramValues :: Map String Word
    , varValues   :: Map String Word
    }

sockeyeBuildNet :: AST.SockeyeSpec -> Either CheckFailure NetAST.NetSpec
sockeyeBuildNet ast = do
    let
        emptySpec = AST.SockeyeSpec Map.empty
        context = Context
            { spec        = emptySpec
            , paramValues = Map.empty
            , varValues   = Map.empty
            }
    net <- buildNet context ast
    -- TODO: check duplicates
    let
        nodeMap = Map.fromList net
    -- TODO: check references
    return $ NetAST.NetSpec nodeMap

class NetSource a b where
    buildNet :: Context -> a -> Either CheckFailure b

instance NetSource AST.SockeyeSpec NetList where
    buildNet context ast = do
        let
            rootInst = AST.ModuleInst
                { AST.nameSpace  = AST.SimpleIdent ""
                , AST.moduleName = "@root"
                , AST.arguments  = Map.empty
                , AST.inPortMap  = []
                , AST.outPortMap = []
                }
            specContext = context
                { spec = ast }
        buildNet specContext rootInst

instance NetSource AST.ModuleInst NetList where
    buildNet context (AST.MultiModuleInst for) = buildNet context for
    buildNet context ast = do
        let
            nameSpace = AST.nameSpace ast
            name = AST.moduleName ast
            args = AST.arguments ast
            mod = getModule context name
            nodeDecls = AST.nodeDecls mod
            modInsts = AST.moduleInsts mod
            concreteArgs = Map.map argumentValue args
            modContext = moduleContext concreteArgs
            nameSpaceId = identToName context nameSpace
        declNet <- buildNet modContext nodeDecls
        instNet <- buildNet modContext modInsts
        let
            prefixDeclNet = map (prefix nameSpaceId) declNet
            prefixInstNet = map (prefix nameSpaceId) instNet
        return $ prefixDeclNet ++ prefixInstNet
        where
            argumentValue (AST.AddressArg value) = value
            argumentValue (AST.NumberArg value) = value
            argumentValue (AST.ParamArg name) = getParamValue context name
            moduleContext paramValues =
                context
                    { paramValues = paramValues
                    , varValues = Map.empty
                    }


instance NetSource AST.Identifier NetAST.NodeId where
    buildNet context ast = do
        let
            name = identToName context ast
        return NetAST.NodeId
            { NetAST.namespace = []
            , NetAST.name      = name
            }

instance NetSource AST.NodeDecl NetList where
    buildNet context (AST.MultiNodeDecl for) = buildNet context for
    buildNet context ast = do
        let
            ident = AST.nodeId ast
            nodeSpec = AST.nodeSpec ast
        nodeId <- buildNet context ident
        netNodeSpec <- buildNet context nodeSpec
        return [(nodeId, netNodeSpec)]

instance NetSource a NetList => NetSource [a] NetList where
    buildNet context ast = do
        let
            decls = map (buildNet context) ast
            fs = lefts decls
            ds = rights decls
        case fs of
            [] -> return $ concat ds
            _  -> Left $ CheckFailure (unlines $ map message fs)

instance NetSource AST.NodeSpec NetAST.NodeSpec where
    buildNet context ast = do
        return NetAST.NodeSpec
            { NetAST.nodeType  = NetAST.Other
            , NetAST.accept    = []
            , NetAST.translate = []
            , NetAST.overlay   = Nothing
            }

instance NetSource a NetList => NetSource (AST.For a) NetList where
    buildNet context ast = do
        let
            body = AST.body ast
            varRanges = AST.varRanges ast
            concreteRanges = Map.map concreteRange varRanges
            valueList = Map.foldWithKey iterations [] concreteRanges
            iterContexts = map iterationContext valueList
            decls = map (\c -> buildNet c body) iterContexts
            fs = lefts decls
            ds = rights decls
        case fs of
            [] -> return $ concat ds
            _  -> Left $ CheckFailure (unlines $ map message fs)
        where
            concreteRange range =
                let
                    start = limitVal $ AST.start range
                    end = limitVal $ AST.end range
                in [start..end]
            limitVal (AST.NumberLimit value) = value
            limitVal (AST.ParamLimit name) = getParamValue context name
            iterations k vs [] = [Map.fromList [(k,v)] | v <- vs]
            iterations k vs ms = concat $ map (f ms k) vs
                where
                    f ms k v = map (Map.insert k v) ms
            iterationContext varMap =
                let values = varValues context
                in context
                    { varValues = values `Map.union` varMap }

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

identToName :: Context -> AST.Identifier -> String
identToName _ (AST.SimpleIdent name) = name
identToName context ident =
    let
        prefix = AST.prefix ident
        varName = AST.varName ident
        suffix = AST.suffix ident
        varValue = show $ getVarValue context varName
        suffixName = case suffix of
            Nothing -> ""
            Just s  -> identToName context s
    in prefix ++ varValue ++ suffixName

prefix :: String -> (NetAST.NodeId, NetAST.NodeSpec) -> (NetAST.NodeId, NetAST.NodeSpec)
prefix nameSpace (nodeId, nodeSpec) =
    let
        prevNS = NetAST.namespace nodeId
        prefixed = if nameSpace == ""
            then nodeId
            else nodeId
                { NetAST.namespace = nameSpace:prevNS }
    in (prefixed, nodeSpec)
