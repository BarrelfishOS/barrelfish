{-
    SockeyeChecker.hs: AST checker for Sockeye

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

module SockeyeTypeChecker
( typeCheckSockeye ) where

import Control.Monad

import Data.List (nub)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either

import SockeyeChecks

import qualified SockeyeASTParser as ParseAST
import qualified SockeyeASTTypeChecker as CheckAST

import Debug.Trace

data TypeCheckFail
    = DuplicateModule String
    | DuplicateParameter String
    | DuplicateVariable String
    | NoSuchModule String
    | NoSuchParameter String
    | NoSuchVariable String
    | ParamTypeMismatch String CheckAST.ModuleParamType CheckAST.ModuleParamType
    | WrongNumberOfArgs String Int Int
    | ArgTypeMismatch String String CheckAST.ModuleParamType CheckAST.ModuleParamType

instance Show TypeCheckFail where
    show (DuplicateModule name)    = concat ["Multiple definitions for module '", name, "'"]
    show (DuplicateParameter name) = concat ["Multiple parameters named '", name, "'"]
    show (DuplicateVariable name)  = concat ["Multiple definitions for variable '", name, "'"]
    show (NoSuchModule name)       = concat ["No definition for module '", name, "'"]
    show (NoSuchParameter name)    = concat ["Parameter '", name, "' not in scope"]
    show (NoSuchVariable name)     = concat ["Variable '", name, "' not in scope"]
    show (WrongNumberOfArgs name takes given) = concat ["Module '", name, "' takes ", show takes, " argument(s), given ", show given]
    show (ParamTypeMismatch name expected actual) =
        concat ["Expected type '", show expected, "' but '", name, "' has type '", show actual, "'"]
    show (ArgTypeMismatch modName name expected actual) =
        concat ["Type mismatch for argument '", name, "' for module '", modName, "': Expected '", show expected, "', given '", show actual, "'"]

data ModuleSymbol = ModuleSymbol
    { paramNames :: [String]
    , paramTypes :: Map String CheckAST.ModuleParamType
    }
type SymbolTable = Map String ModuleSymbol

data Context = Context
    { symTable   :: SymbolTable
    , curModule  :: !String
    , instModule :: !String
    , vars       :: Set String
    }

typeCheckSockeye :: ParseAST.SockeyeSpec -> Either (FailedChecks TypeCheckFail) CheckAST.SockeyeSpec
typeCheckSockeye ast = do
    symbolTable <- runChecks $ buildSymbolTable ast
    let context = Context
            { symTable   = symbolTable
            , curModule  = ""
            , instModule = ""
            , vars       = Set.empty
            }
    runChecks $ check context ast

--
-- Build Symbol table
--
class SymbolSource a where
    buildSymbolTable :: a -> Checks TypeCheckFail SymbolTable

instance SymbolSource ParseAST.SockeyeSpec where
    buildSymbolTable ast = do
        let mods = ParseAST.modules ast
        symbolTables <- mapM buildSymbolTable mods
        let names = concat $ map Map.keys symbolTables
        checkDuplicates "@all" DuplicateModule names
        return $ Map.unions symbolTables
        
instance SymbolSource ParseAST.Module where
    buildSymbolTable ast = do
        let modName = ParseAST.name ast
            params = ParseAST.parameters ast
            names = map ParseAST.paramName params
            types = map ParseAST.paramType params
        checkDuplicates modName DuplicateParameter names
        let typeMap = Map.fromList $ zip names types
            modSymbol = ModuleSymbol
                { paramNames = names
                , paramTypes = typeMap
                }
        return $ Map.singleton modName modSymbol

--
-- Check module bodies
--
class Checkable a b where
    check :: Context -> a -> Checks TypeCheckFail b

instance Checkable ParseAST.SockeyeSpec CheckAST.SockeyeSpec where
    check context ast = do
        let mods = ParseAST.modules ast
            rootNetSpecs = ParseAST.net ast
            names = map ParseAST.name mods
            rootName = "@root"
            rootSymbol = ModuleSymbol
                { paramNames = []
                , paramTypes = Map.empty
                }
            rootModContext = context
                { symTable = Map.insert rootName rootSymbol $ symTable context
                , curModule = rootName
                }
        checkedRootNetSpecs <- check rootModContext rootNetSpecs
        checkedModules <- check context mods
        let root = CheckAST.ModuleInst
                { CheckAST.namespace  = CheckAST.SimpleIdent ""
                , CheckAST.moduleName = rootName
                , CheckAST.arguments  = Map.empty
                , CheckAST.inPortMap  = []
                , CheckAST.outPortMap = []
                }
            rootModule = CheckAST.Module
                { CheckAST.paramNames   = []
                , CheckAST.paramTypeMap = Map.empty
                , CheckAST.ports        = []
                , CheckAST.nodeDecls    = lefts  checkedRootNetSpecs
                , CheckAST.moduleInsts  = rights checkedRootNetSpecs
                }
            moduleMap = Map.fromList $ zip (rootName:names) (rootModule:checkedModules)
        return CheckAST.SockeyeSpec
            { CheckAST.root    = root
            , CheckAST.modules = moduleMap
            }

instance Checkable ParseAST.Module CheckAST.Module where
    check context ast = do
        let
            name = ParseAST.name ast
            body = ParseAST.moduleBody ast
            ports = ParseAST.ports body
            netSpecs = ParseAST.moduleNet body
            symbol = (symTable context) Map.! name
        let bodyContext = context
                { curModule = name }
        checkedPorts <- check bodyContext ports
        checkedNetSpecs <- check bodyContext netSpecs
        let
            checkedNodeDecls = lefts checkedNetSpecs
            checkedModuleInsts = rights checkedNetSpecs
        return CheckAST.Module
            { CheckAST.paramNames   = paramNames symbol
            , CheckAST.paramTypeMap = paramTypes symbol
            , CheckAST.ports        = checkedPorts
            , CheckAST.nodeDecls    = checkedNodeDecls
            , CheckAST.moduleInsts  = checkedModuleInsts
            }

instance Checkable ParseAST.Port CheckAST.Port where
    check context (ParseAST.InputPort portId portWidth) = do
        checkedId <- check context portId
        return $ CheckAST.InputPort checkedId portWidth
    check context (ParseAST.OutputPort portId portWidth) = do
        checkedId <- check context portId
        return $ CheckAST.OutputPort checkedId portWidth
    check context (ParseAST.MultiPort for) = do
        checkedFor <- check context for
        return $ CheckAST.MultiPort checkedFor

instance Checkable ParseAST.NetSpec (Either CheckAST.NodeDecl CheckAST.ModuleInst) where
    check context (ParseAST.NodeDeclSpec decl) = do
        checkedDecl <- check context decl
        return $ Left checkedDecl
    check context (ParseAST.ModuleInstSpec inst) = do
        checkedInst <- check context inst
        return $ Right checkedInst

instance Checkable ParseAST.ModuleInst CheckAST.ModuleInst where
    check context (ParseAST.MultiModuleInst for) = do
        checkedFor <- check context for
        return $ CheckAST.MultiModuleInst checkedFor
    check context ast = do
        let
            namespace = ParseAST.namespace ast
            name = ParseAST.moduleName ast
            arguments = ParseAST.arguments ast
            portMaps = ParseAST.portMappings ast
        checkedArgs <- if Map.member name (symTable context)
            then check (context { instModule = name }) arguments
            else do
                failCheck (curModule context) $ NoSuchModule name
                return Map.empty
        checkedNamespace <- check context namespace
        inPortMap  <- check context $ filter isInMap  portMaps
        outPortMap <- check context $ filter isOutMap portMaps
        return CheckAST.ModuleInst
            { CheckAST.namespace  = checkedNamespace
            , CheckAST.moduleName = name
            , CheckAST.arguments  = checkedArgs
            , CheckAST.inPortMap  = inPortMap
            , CheckAST.outPortMap = outPortMap
            }
        where
            isInMap  (ParseAST.InputPortMap  {}) = True
            isInMap  (ParseAST.OutputPortMap {}) = False
            isInMap  (ParseAST.MultiPortMap for) = isInMap $ ParseAST.body for
            isOutMap (ParseAST.InputPortMap  {}) = False
            isOutMap (ParseAST.OutputPortMap {}) = True
            isOutMap (ParseAST.MultiPortMap for) = isOutMap $ ParseAST.body for

instance Checkable [ParseAST.ModuleArg] (Map String CheckAST.ModuleArg) where
    check context ast = do
        let symbol = (symTable context) Map.! instName
            names = paramNames symbol
            expTypes = map (paramTypes symbol Map.!) names
        checkArgCount names ast
        checkedArgs <- zipWithM checkArgType (zip names expTypes) ast
        return $ Map.fromList $ zip names checkedArgs
        where
            checkArgCount params args = do
                let
                    paramc = length params
                    argc = length args
                if argc == paramc
                    then return ()
                    else failCheck curName $ WrongNumberOfArgs instName paramc argc
            checkArgType (name, expType) arg = do
                case arg of
                    ParseAST.AddressArg value -> do
                        if expType == CheckAST.AddressParam
                            then return $ CheckAST.AddressArg value
                            else do
                                mismatch CheckAST.AddressParam
                                return $ CheckAST.AddressArg value
                    ParseAST.NaturalArg value -> do
                        if expType == CheckAST.NaturalParam
                            then return $ CheckAST.NaturalArg value
                            else do
                                mismatch CheckAST.NaturalParam
                                return $ CheckAST.AddressArg value
                    ParseAST.ParamArg paramName -> do
                        checkParamType context paramName expType
                        return $ CheckAST.ParamArg paramName
                where
                    mismatch = failCheck curName . ArgTypeMismatch instName name expType
            curName = curModule context
            instName = instModule context

instance Checkable ParseAST.PortMap CheckAST.PortMap where
    check context (ParseAST.MultiPortMap for) = do
        checkedFor <- check context for
        return $ CheckAST.MultiPortMap checkedFor
    check context portMap = do
        let
            mappedId = ParseAST.mappedId portMap
            mappedPort = ParseAST.mappedPort portMap
        checkedId <- check context mappedId
        checkedPort <- check context mappedPort
        return $ CheckAST.PortMap
            { CheckAST.mappedId   = checkedId
            , CheckAST.mappedPort = checkedPort
            }

instance Checkable ParseAST.NodeDecl CheckAST.NodeDecl where
    check context (ParseAST.MultiNodeDecl for) = do
        checkedFor <- check context for
        return $ CheckAST.MultiNodeDecl checkedFor
    check context ast = do
        let
            nodeId = ParseAST.nodeId ast
            nodeSpec = ParseAST.nodeSpec ast
        checkedId <- check context nodeId
        checkedSpec <- check context nodeSpec
        return CheckAST.NodeDecl
            { CheckAST.nodeId   = checkedId
            , CheckAST.nodeSpec = checkedSpec
            }

instance Checkable ParseAST.Identifier CheckAST.Identifier where
    check _ (ParseAST.SimpleIdent name) = return $ CheckAST.SimpleIdent name
    check context ast = do
        let
            prefix = ParseAST.prefix ast
            varName = ParseAST.varName ast
            suffix = ParseAST.suffix ast
        checkVarInScope context varName
        checkedSuffix <- case suffix of
            Nothing    -> return Nothing
            Just ident -> do
                checkedIdent <- check context ident
                return $ Just checkedIdent
        return CheckAST.TemplateIdent
            { CheckAST.prefix  = prefix
            , CheckAST.varName = varName
            , CheckAST.suffix  = checkedSuffix
            }

instance Checkable ParseAST.NodeSpec CheckAST.NodeSpec where
    check context ast = do
        let 
            nodeType = ParseAST.nodeType ast
            accept = ParseAST.accept ast
            translate = ParseAST.translate ast
            overlay = ParseAST.overlay ast
            reserved = ParseAST.reserved ast
        checkedAccept <- check context accept
        checkedTranslate <- check context translate
        checkedReserved <- check context reserved
        checkedOverlay <- case overlay of
            Nothing    -> return Nothing
            Just ident -> do
                checkedIdent <- check context ident
                return $ Just checkedIdent
        return CheckAST.NodeSpec
            { CheckAST.nodeType  = nodeType
            , CheckAST.accept    = checkedAccept
            , CheckAST.translate = checkedTranslate
            , CheckAST.reserved  = checkedReserved
            , CheckAST.overlay   = checkedOverlay
            }

instance Checkable ParseAST.BlockSpec CheckAST.BlockSpec where
    check context (ParseAST.SingletonBlock address) = do
        checkedAddress <- check context address
        return CheckAST.SingletonBlock
            { CheckAST.base = checkedAddress }
    check context (ParseAST.RangeBlock base limit) = do
        checkedBase <- check context base
        checkedLimit <- check context limit
        return CheckAST.RangeBlock
            { CheckAST.base  = checkedBase
            , CheckAST.limit = checkedLimit
            }
    check context (ParseAST.LengthBlock base bits) = do
        checkedBase <- check context base
        return CheckAST.LengthBlock
            { CheckAST.base = checkedBase
            , CheckAST.bits = bits
            }

instance Checkable ParseAST.MapSpec CheckAST.MapSpec where
    check context ast = do
        let
            block = ParseAST.block ast
            destNode = ParseAST.destNode ast
            destBase = ParseAST.destBase ast
        checkedBlock <- check context block
        checkedDestNode <- check context destNode
        checkedDestBase <- case destBase of
            Nothing      -> return Nothing
            Just address -> do
                checkedAddress <- check context address
                return $ Just checkedAddress
        return CheckAST.MapSpec
            { CheckAST.block    = checkedBlock
            , CheckAST.destNode = checkedDestNode
            , CheckAST.destBase = checkedDestBase
            }

instance Checkable ParseAST.OverlaySpec CheckAST.OverlaySpec where
    check context (ParseAST.OverlaySpec over width) = do
        checkedOver <- check context over
        return $ CheckAST.OverlaySpec checkedOver width

instance Checkable ParseAST.Address CheckAST.Address where
    check _ (ParseAST.LiteralAddress value) = do
        return $ CheckAST.LiteralAddress value
    check context (ParseAST.ParamAddress name) = do
        checkParamType context name CheckAST.AddressParam
        return $ CheckAST.ParamAddress name

instance Checkable a b => Checkable (ParseAST.For a) (CheckAST.For b) where
    check context ast = do
        let
            varRanges = ParseAST.varRanges ast
            varNames = map ParseAST.var varRanges
            body = ParseAST.body ast
            currentVars = vars context
        checkDuplicates (curModule context) DuplicateVariable (varNames ++ Set.elems currentVars)
        ranges <- check context varRanges
        let
            bodyVars = currentVars `Set.union` (Set.fromList varNames)
            bodyContext = context
                { vars = bodyVars }
        checkedBody <- check bodyContext body
        let
            checkedVarRanges = Map.fromList $ zip varNames ranges
        return CheckAST.For
                { CheckAST.varRanges = checkedVarRanges
                , CheckAST.body      = checkedBody
                }

instance Checkable ParseAST.ForVarRange CheckAST.ForRange where
    check context ast = do
        let 
            start = ParseAST.start ast
            end = ParseAST.end ast
        checkedStart <- check context start
        checkedEnd<- check context end
        return CheckAST.ForRange
            { CheckAST.start = checkedStart
            , CheckAST.end   = checkedEnd
            }

instance Checkable ParseAST.ForLimit CheckAST.ForLimit where
    check _ (ParseAST.LiteralLimit value) = do
        return $ CheckAST.LiteralLimit value
    check context (ParseAST.ParamLimit name) = do
        checkParamType context name CheckAST.NaturalParam
        return $ CheckAST.ParamLimit name

instance (Traversable t, Checkable a b) => Checkable (t a) (t b) where
    check context as = mapM (check context) as

--
-- Helpers
--    
checkVarInScope :: Context -> String -> Checks TypeCheckFail ()
checkVarInScope context name = do
    if name `Set.member` (vars context)
        then return ()
        else failCheck (curModule context) $ NoSuchVariable name


checkParamType :: Context -> String -> CheckAST.ModuleParamType -> Checks TypeCheckFail ()
checkParamType context name expected = do
    let symbol = (symTable context) Map.! (curModule context)
    case Map.lookup name $ paramTypes symbol of
        Nothing -> failCheck (curModule context) $ NoSuchParameter name
        Just actual -> do
            if actual == expected
                then return ()
                else failCheck (curModule context) $ ParamTypeMismatch name expected actual
