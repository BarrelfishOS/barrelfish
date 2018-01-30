{-
    SockeyeSymbolTableBuilder.hs: Symbol Table Builder for Sockeye

    Part of Sockeye

    Copyright (c) 2018, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module SockeyeSymbolTableBuilder
    (buildSymbolTable) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import SockeyeChecks

import SockeyeASTMeta
import SockeyeParserAST as AST
import SockeyeSymbolTable as ST

data SymbolTableFail
    = DuplicateModule String ASTMeta
    | DuplicateType   String ASTMeta
    | DuplicateParameter String
    | DuplicateConstant String ASTMeta
    | ParameterShadowing String ASTMeta
    | DuplicateInstance String ASTMeta
    | DuplicateNode String ASTMeta

instance CheckFailure SymbolTableFail where
    errorLines (DuplicateModule name firstDefined)    = ["Duplicate module '" ++ name ++ "'", "First defined at " ++ show firstDefined]
    errorLines (DuplicateType   name firstDefined)    = ["Duplicate type '" ++ name ++ "'", "First defined at " ++ show firstDefined]
    errorLines (DuplicateParameter name)              = ["Duplicate parameter '" ++ name ++ "'"]
    errorLines (DuplicateConstant name firstDefined)  = ["Duplicate named constant '" ++ name ++ "'", "First defined at " ++ show firstDefined]
    errorLines (ParameterShadowing name firstDefined) = ["Named constant '" ++ name ++ "' shadows module parameter", "Parameter is declared at " ++ show firstDefined]
    errorLines (DuplicateInstance name firstDefined)  = ["Duplicate instance '" ++ name ++ "'", "First defined at " ++ show firstDefined]
    errorLines (DuplicateNode name firstDefined)      = ["Duplicate node '" ++ name ++ "'", "First defined at " ++ show firstDefined]

buildSymbolTable :: AST.Sockeye -> Either (FailedChecks SymbolTableFail) ST.Sockeye
buildSymbolTable ast = runChecks $ symbol ast

class SymbolSource a b where
    symbol :: a -> Checks SymbolTableFail b

instance SymbolSource AST.Sockeye ST.Sockeye where
    symbol ast = do
        modules <- symbolMap AST.moduleName moduleDupFail $ AST.modules ast
        types <- symbolMap AST.typeName typeDupFail $ AST.types ast
        return ST.Sockeye
            { ST.sockeyeMeta = AST.sockeyeMeta ast
            , ST.modules = modules
            , ST.types = types
            }
        where
            moduleDupFail m s = DuplicateModule (AST.moduleName m) (meta s)
            typeDupFail t s = DuplicateType (AST.typeName t) (meta s)

instance SymbolSource AST.Module ST.Module where
    symbol ast = do
        let paramList = AST.parameters ast
            constantList = AST.constants ast
            instDecls = AST.instDecls ast
            nodeDecls = AST.nodeDecls ast
        parameters <- symbolMap AST.paramName paramDupFail paramList
        constants <- symbolMap AST.constName constDupFail constantList
        foldChecks (checkNoShadowing parameters) () constantList
        instances <- symbolMap AST.instName instDupFail instDecls
        allNodes <- symbolMap AST.nodeName nodeDupFail nodeDecls
        let parameterOrder = map AST.paramName paramList
            inputPortNames = Set.fromList (map AST.nodeName $ filter isInputPort nodeDecls)
            outputPortNames = Set.fromList (map AST.nodeName $ filter isOutputPort nodeDecls)
            (outputPorts, nodes) = Map.partitionWithKey (isInSet outputPortNames) allNodes
        return ST.Module
            { ST.moduleMeta     = meta ast
            , ST.parameters     = parameters
            , ST.parameterOrder = parameterOrder
            , ST.constants      = constants
            , ST.inputPorts     = inputPortNames
            , ST.outputPorts    = outputPorts
            , ST.instances      = instances
            , ST.nodes          = nodes
            }
        where
            paramDupFail p _ = DuplicateParameter (AST.paramName p)
            constDupFail c d = DuplicateConstant (AST.constName c) (meta d)
            instDupFail i d = DuplicateInstance (AST.instName i) (meta d)
            nodeDupFail n d = DuplicateNode (AST.nodeName n) (meta d)
            isInputPort n = AST.InputPort == (AST.nodeKind n)
            isOutputPort n = AST.OutputPort == (AST.nodeKind n)
            isInSet set k _ = k `Set.member` set
            checkNoShadowing paramMap c _ = do
                let name = AST.constName c
                case Map.lookup name paramMap of
                    Nothing -> return ()
                    Just shadowed -> failCheck (meta c) $ ParameterShadowing name (meta shadowed)

instance SymbolSource AST.ModuleParameter ST.ModuleParameter where
    symbol ast = do
        return ST.ModuleParameter
            { ST.paramMeta  = meta ast
            , ST.paramRange = ST.NaturalSet (meta $ AST.paramRange ast) []
            }

instance SymbolSource AST.InstanceDeclaration ST.Instance where
    symbol ast = do
        return ST.Instance
            { ST.instMeta    = meta ast
            , ST.instModule  = AST.instModName ast
            , ST.instArrSize = AST.instArrSize ast
            }

instance SymbolSource AST.NodeDeclaration ST.Node where
    symbol ast = do
        return ST.Node
            { ST.nodeMeta    = meta ast
            , ST.nodeType    = AST.nodeType ast
            , ST.nodeArrSize = AST.nodeArrSize ast
            }

instance SymbolSource AST.NamedType ST.NamedType where
    symbol ast = return ST.NamedType
        { ST.namedTypeMeta = meta ast
        , ST.namedType     = AST.namedType ast
        }

instance SymbolSource AST.NamedConstant ST.NamedConstant where
    symbol ast = return ST.NamedConstant
        { ST.namedConstMeta = meta ast
        , ST.namedConst     = AST.namedConst ast
        }

symbolMap :: (MetaAST a, MetaAST b, SymbolSource a b) => (a -> String) -> (a -> b -> SymbolTableFail) -> [a] -> Checks SymbolTableFail (Map String b)
symbolMap keyFn fail = foldChecks f Map.empty
    where
        f a symMap = do
            sym <- symbol a
            let name = keyFn a
            case Map.lookup name symMap of
                Nothing -> return $ Map.insert name sym symMap
                Just dup -> failCheck (meta a) (fail a dup) >> return symMap
