{-
    SockeyeSymbolTableBuilder.hs: Symbol Table Builder for Sockeye

    Part of Sockeye

    Copyright (c) 2018, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module SockeyeSymbolTableBuilder
    (buildSymbolTable) where

import Data.List (intercalate)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import SockeyeChecks

import SockeyeASTMeta
import qualified SockeyeParserAST as AST
import qualified SockeyeSymbolTable as ST

data SymbolTableFail
    = CircularImports [FilePath]
    | DuplicateImport String ASTMeta
    | NoSuchExport String String
    | ImportShadowing String String String ASTMeta
    | DuplicateModule String ASTMeta
    | DuplicateType   String ASTMeta
    | ModuleTypeClash String ASTMeta ASTMeta
    | DuplicateParameter String
    | DuplicateConstant String ASTMeta
    | ParameterShadowing String ASTMeta
    | DuplicateInstance String ASTMeta
    | DuplicateNode String ASTMeta

instance CheckFailure SymbolTableFail where
    errorLines (CircularImports loop)                                    = ["Circular imports detected:", intercalate " -> " $ reverse loop]
    errorLines (DuplicateImport name firstDefined)                       = ["Duplicate import '" ++ name ++ "'", "First imported at " ++ show firstDefined]
    errorLines (NoSuchExport name file)                                  = ["File '" ++ file ++ "' does not export symbol '" ++ name ++ "'"]
    errorLines (ImportShadowing name defSymType impSymType firstDefined) = [defSymType ++ " '" ++ name ++ "' shadows imported " ++ impSymType, "Imported at " ++ show firstDefined]
    errorLines (DuplicateModule name firstDefined)                       = ["Duplicate module '" ++ name ++ "'", "First defined at " ++ show firstDefined]
    errorLines (DuplicateType   name firstDefined)                       = ["Duplicate type '" ++ name ++ "'", "First defined at " ++ show firstDefined]
    errorLines (ModuleTypeClash name mDef tDef)                          = ["Module and type with same name '" ++ name ++ "'", "Module declared at " ++ show mDef, "Type declared at " ++ show tDef]
    errorLines (DuplicateParameter name)                                 = ["Duplicate parameter '" ++ name ++ "'"]
    errorLines (DuplicateConstant name firstDefined)                     = ["Duplicate named constant '" ++ name ++ "'", "First defined at " ++ show firstDefined]
    errorLines (ParameterShadowing name firstDefined)                    = ["Named constant '" ++ name ++ "' shadows module parameter", "Parameter is declared at " ++ show firstDefined]
    errorLines (DuplicateInstance name firstDefined)                     = ["Duplicate instance '" ++ name ++ "'", "First defined at " ++ show firstDefined]
    errorLines (DuplicateNode name firstDefined)                         = ["Duplicate node '" ++ name ++ "'", "First defined at " ++ show firstDefined]

data Import
    = TypeImport ST.NamedType
    | ModuleImport ST.Module
    deriving (Show)

instance MetaAST Import where
    meta (TypeImport t) = meta t
    meta (ModuleImport m) = meta m

buildSymbolTable :: AST.Sockeye -> Either (FailedChecks SymbolTableFail) ST.Sockeye
buildSymbolTable ast = runChecks $ symbol ast

class SymbolSource a b where
    symbol :: a -> Checks SymbolTableFail b

instance SymbolSource AST.Sockeye ST.Sockeye where
    symbol ast = do
        let entryPoint = AST.entryPoint ast
        files <- fileSymbols [] entryPoint Map.empty
        return ST.Sockeye
            { ST.entryPoint = entryPoint
            , ST.files      = files
            }
        where
            fileSymbols predecessors filePath fileSyms
               | filePath `Map.member` fileSyms = return fileSyms
               | otherwise = do
                    let file = (AST.files ast) Map.! filePath
                        imports = AST.imports file
                    fileSyms' <- foldChecks importFileSymbols fileSyms imports
                    fileSym <- symbol file
                    importSymbols <- foldChecks (collectImports fileSyms') Map.empty imports
                    fileSym' <- foldChecks addImportSymbol fileSym $ Map.assocs importSymbols
                    return $ Map.insert filePath fileSym' fileSyms'
                where
                    importFileSymbols importAst fileSyms = do
                        let importFile = AST.importFile importAst
                        case loop importFile predecessors of
                            [] -> fileSymbols (importFile:predecessors) importFile fileSyms
                            loop -> failCheck (meta importAst) (CircularImports $ importFile:loop) >> return fileSyms
                    loop name [] = []
                    loop name path@(p:ps)
                        | name `elem` path = p:(loop name ps)
                        | otherwise = []

collectImports :: Map FilePath ST.SockeyeFile -> AST.Import -> Map String Import -> Checks SymbolTableFail (Map String Import)
collectImports fileSymbols importAst importSymbols = do
    let importedFileName = AST.importFile importAst
        importedFile = fileSymbols Map.! importedFileName
    case AST.explImports importAst of
        Just as -> foldChecks importWithAlias importSymbols as
        Nothing -> do
            withTypes <- foldChecks implicitTypeImport importSymbols (Map.assocs $ ST.types importedFile)
            foldChecks implicitModuleImport withTypes (Map.assocs $ ST.modules importedFile)
    where
        importWithAlias a importSymbols = do
            let importedFileName = AST.importFile importAst
                importedFile = fileSymbols Map.! importedFileName
                origName = AST.originalName a
                types = ST.types importedFile
                modules = ST.modules importedFile
            {-
             - Import either type or module
             - Only one can exist (see ModuleTypeClash check)
             -}
            case Map.lookup origName types of
                Just t -> explicitTypeImport a t importSymbols
                Nothing -> case Map.lookup origName modules of
                    Just m -> explicitModuleImport a m importSymbols
                    Nothing -> do
                        failCheck (meta a) $ NoSuchExport origName importedFileName
                        return importSymbols
        {- Imported types are not exported -}
        explicitTypeImport a (ST.ImportedType {}) importSymbols = do
            failCheck (meta importAst) $ NoSuchExport (AST.originalName a) (AST.importFile importAst)
            return importSymbols
        explicitTypeImport a (ST.NamedType {}) importSymbols = do
            let alias = AST.importAlias a
            case Map.lookup alias importSymbols of
                Nothing -> return $ Map.insert alias (importedType (AST.originalName a) $ meta a) importSymbols
                Just dup -> do
                    failCheck (meta a) $ DuplicateImport alias (meta dup)
                    return importSymbols
        {- Imported modules are not exported -}
        explicitModuleImport a (ST.ImportedModule {}) importSymbols = do
            failCheck (meta a) $ NoSuchExport (AST.originalName a) (AST.importFile importAst)
            return importSymbols
        explicitModuleImport a (ST.Module {}) importSymbols = do
            let alias = AST.importAlias a
            case Map.lookup alias importSymbols of
                Nothing -> return $ Map.insert alias (importedModule (AST.originalName a) $ meta a) importSymbols
                Just dup -> do
                    failCheck (meta a) $ DuplicateImport alias (meta dup)
                    return importSymbols
        {- Only import declared types, not imported ones -}
        implicitTypeImport (_, ST.ImportedType {}) importSymbols = return importSymbols
        implicitTypeImport (name, ST.NamedType {}) importSymbols = case Map.lookup name importSymbols of
            Nothing -> return $ Map.insert name (importedType name $ meta importAst) importSymbols
            Just dup -> do
                failCheck (meta importAst) $ DuplicateImport name (meta dup)
                return importSymbols
        {- Only import declared modules, not imported ones -}
        implicitModuleImport (_, ST.ImportedModule {}) importSymbols = return importSymbols
        implicitModuleImport (name, ST.Module {}     ) importSymbols = case Map.lookup name importSymbols of
            Nothing -> return $ Map.insert name (importedModule name $ meta importAst) importSymbols
            Just dup -> do
                failCheck (meta importAst) $ DuplicateImport name (meta dup)
                return importSymbols
        importedType typeName m = TypeImport $ ST.ImportedType
            { ST.namedTypeMeta = m
            , ST.typeFile      = AST.importFile importAst
            , ST.origTypeName  = typeName
            }
        importedModule modName m = ModuleImport $ ST.ImportedModule
            { ST.moduleMeta  = m
            , ST.moduleFile  = AST.importFile importAst
            , ST.origModName = modName
            }

addImportSymbol :: (String, Import) -> ST.SockeyeFile -> Checks SymbolTableFail ST.SockeyeFile
addImportSymbol (name, TypeImport i) fileSymbol = do
    case Map.lookup name (ST.modules fileSymbol) of
        Nothing -> return ()
        Just m -> failCheck (meta m) $ ImportShadowing name "Module" "type" (meta i)
    let ts = ST.types fileSymbol
    case Map.lookup name ts of
        Nothing -> return $ fileSymbol { ST.types = Map.insert name i ts }
        Just t -> failCheck (meta t) (ImportShadowing name "Type" "type" (meta i)) >> return fileSymbol
addImportSymbol (name, ModuleImport i) fileSymbol = do
    case Map.lookup name (ST.types fileSymbol) of
        Nothing -> return ()
        Just t -> failCheck (meta t) $ ImportShadowing name "Type" "module" (meta i)
    let ms = ST.modules fileSymbol
    case Map.lookup name ms of
        Nothing -> return $ fileSymbol { ST.modules = Map.insert name i ms }
        Just m -> failCheck (meta m) (ImportShadowing name "Module" "module" (meta i)) >> return fileSymbol
                    
instance SymbolSource AST.SockeyeFile ST.SockeyeFile where
    symbol ast = do
        modules <- symbolMap AST.moduleName moduleDupFail $ AST.modules ast
        types <- symbolMap AST.typeName typeDupFail $ AST.types ast
        types' <- foldChecks checkModTypeClash types $ Map.assocs modules
        return ST.SockeyeFile
            { ST.sockeyeFileMeta = AST.sockeyeFileMeta ast
            , ST.modules = modules
            , ST.types = types'
            }
        where
            moduleDupFail m s = DuplicateModule (AST.moduleName m) (meta s)
            typeDupFail t s = DuplicateType (AST.typeName t) (meta s)
            checkModTypeClash (name, m) ts = case Map.lookup name ts of
                Nothing -> return ts
                Just t -> do
                    let mMeta = meta m
                        tMeta = meta t
                    failCheck (max mMeta tMeta) $ ModuleTypeClash name mMeta tMeta
                    return $ Map.delete name ts

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
