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

instance CheckFailure SymbolTableFail where
    errorLines (DuplicateModule name firstDefined) = ["Duplicate module '" ++ name ++ "'", "First defined at " ++ show firstDefined]
    errorLines (DuplicateType   name firstDefined) = ["Duplicate type '" ++ name ++ "'", "First defined at " ++ show firstDefined]

buildSymbolTable :: AST.Sockeye -> Either (FailedChecks SymbolTableFail) ST.Sockeye
buildSymbolTable ast = runChecks $ symbol ast

class SymbolSource a b where
    symbol :: a -> Checks SymbolTableFail b

instance SymbolSource AST.Sockeye ST.Sockeye where
    symbol ast = do
        modules <- symbolMap AST.moduleName DuplicateModule $ AST.modules ast
        types <- symbolMap AST.typeName DuplicateType $ AST.types ast
        return ST.Sockeye
            { ST.sockeyeMeta = AST.sockeyeMeta ast
            , ST.modules = modules
            , ST.types = types
            }

instance SymbolSource AST.Module ST.Module where
    symbol ast = return ST.Module
        { ST.moduleMeta = meta ast
        , ST.parameters = Map.empty
        , ST.parameterOrder = []
        , ST.constants = Map.empty
        , ST.inputPorts = Set.empty
        , ST.outputPorts = Map.empty
        , ST.instances = Map.empty
        , ST.nodes = Map.empty
        }

instance SymbolSource AST.NamedType ST.NamedType where
    symbol ast = return ST.NamedType
        { ST.namedTypeMeta = meta ast
        , ST.namedType = ST.AddressType (meta ast) []
        }

symbolMap :: (MetaAST a, MetaAST b, SymbolSource a b) => (a -> String) -> (String -> ASTMeta -> SymbolTableFail) -> [a] -> Checks SymbolTableFail (Map String b)
symbolMap keyFn fail = foldChecks f Map.empty
    where
        f a symMap = do
            sym <- symbol a
            let name = keyFn a
            case Map.lookup name symMap of
                Nothing -> return $ Map.insert name sym symMap
                Just dup -> failCheck (meta a) (fail name $ meta dup) >> return symMap
