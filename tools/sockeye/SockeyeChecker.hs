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

module SockeyeChecker
( checkSockeye ) where

import Control.Monad (join)

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either

import qualified SockeyeASTFrontend as ASTF
import qualified SockeyeASTIntermediate as ASTI

import Debug.Trace

data FailedCheck
    = DuplicateModule String
    | DuplicateParameter String
    | DuplicateVariable String
    | NoSuchModule String
    | NoSuchParameter String
    | NoSuchVariable String
    | ParamTypeMismatch String ASTI.ModuleParamType ASTI.ModuleParamType
    | WrongNumberOfArgs String Int Int
    | ArgTypeMismatch String String ASTI.ModuleParamType ASTI.ModuleParamType

instance Show FailedCheck where
    show (DuplicateModule name)    = concat ["Multiple definitions for module '", name, "'."]
    show (DuplicateParameter name) = concat ["Multiple definitions for parameter '", name, "'."]
    show (DuplicateVariable name)  = concat ["Multiple definitions for variable '", name, "'."]
    show (NoSuchModule name)       = concat ["No definition for module '", name, "'."]
    show (NoSuchParameter name)    = concat ["Parameter '", name, "' not in scope."]
    show (NoSuchVariable name)     = concat ["Variable '", name, "' not in scope."]
    show (ParamTypeMismatch name expected actual) =
        concat ["Parameter '", name, "' of type '", show actual, "' used as '", show expected, "'."]
    show (WrongNumberOfArgs name has given) =
        concat ["Module '", name, "' takes ", show has, " arguments, given ", show given, "."]
    show (ArgTypeMismatch modName paramName expected actual) =
        concat ["Argument '", paramName, "' to module '", modName, "' of type '", show expected, "' instantiated with type '", show actual, "."]

newtype CheckFailure = CheckFailure
    { failedChecks :: [FailedCheck] }

instance Show CheckFailure where
    show (CheckFailure fs) = unlines $ map (("    " ++) . show) fs

data Context = Context
    { spec       :: ASTI.SockeyeSpec
    , moduleName :: !String
    , vars       :: Set String
    }

checkSockeye :: ASTF.SockeyeSpec -> Either CheckFailure ASTI.SockeyeSpec
checkSockeye ast = do
    symbolTable <- buildSymbolTable ast
    let
        context = Context
            { spec       = symbolTable
            , moduleName = ""
            , vars       = Set.empty
            }
    check context ast
-- build symbol table
-- check modules:
--  - parameter types must match usage site types
--  - all variables must exist
--  - 
--  - all instantiated modules must exist
--  - modules can not instantiate themselves
--  - instantiation argument types must match parameter types

--
-- Build Symbol table
--
class SymbolSource a b where
    buildSymbolTable :: a -> Either CheckFailure b

instance SymbolSource ASTF.SockeyeSpec ASTI.SockeyeSpec where
    buildSymbolTable ast = do
        let
            modules = (rootModule ast):(ASTF.modules ast)
            names = map ASTF.name modules
        checkDuplicates names DuplicateModule
        symbolTables <- buildSymbolTable modules
        let
            moduleMap = Map.fromList $ zip names symbolTables
        return ASTI.SockeyeSpec
                { ASTI.modules = moduleMap }

instance SymbolSource ASTF.Module ASTI.Module where
    buildSymbolTable ast = do
        let
            paramNames = map ASTF.paramName (ASTF.parameters ast)
            paramTypes = map ASTF.paramType (ASTF.parameters ast)
        checkDuplicates paramNames DuplicateParameter
        let
            paramTypeMap = Map.fromList $ zip paramNames paramTypes
        return ASTI.Module
            { ASTI.paramNames   = paramNames
            , ASTI.paramTypeMap = paramTypeMap
            , ASTI.inputPorts   = []
            , ASTI.outputPorts  = []
            , ASTI.nodeDecls    = []
            , ASTI.moduleInsts  = []
            }

instance SymbolSource a b => SymbolSource [a] [b] where
    buildSymbolTable as = do
        let
            bs = map buildSymbolTable as
            es = concat $ map failedChecks (lefts bs)
        case es of
            [] -> return $ rights bs
            _  -> Left $ CheckFailure es
--
-- Check module bodies
--
class Checkable a b where
    check :: Context -> a -> Either CheckFailure b

instance Checkable ASTF.SockeyeSpec ASTI.SockeyeSpec where
    check context ast = do
        let
            modules = (rootModule ast):(ASTF.modules ast)
            names = map ASTF.name modules
        checked <- check context modules
        let
            sockeyeSpec = spec context
            checkedMap = Map.fromList $ zip names checked
        return sockeyeSpec
            { ASTI.modules = checkedMap }

instance Checkable ASTF.Module ASTI.Module where
    check context ast = do
        let
            name = ASTF.name ast
            bodyContext = context
                { moduleName = name}
            body = ASTF.moduleBody ast
            portDefs = ASTF.ports body
            netSpecs = ASTF.moduleNet body
        inputPorts  <- check bodyContext $ filter isInPort  portDefs
        outputPorts <- check bodyContext $ filter isOutPort portDefs
        checkedNetSpecs <- check bodyContext netSpecs
        let
            checkedNodeDecls = lefts checkedNetSpecs
            checkedModuleInsts = rights checkedNetSpecs
            mod = getCurrentModule bodyContext
        return mod
            { ASTI.inputPorts  = inputPorts
            , ASTI.outputPorts = outputPorts
            , ASTI.nodeDecls   = checkedNodeDecls
            , ASTI.moduleInsts = checkedModuleInsts
            }
        where
            isInPort (ASTF.InputPortDef _) = True
            isInPort (ASTF.MultiPortDef for) = isInPort $ ASTF.body for
            isInPort _ = False
            isOutPort = not . isInPort

instance Checkable ASTF.PortDef ASTI.Port where
    check context (ASTF.MultiPortDef for) = do
        checkedFor <- check context for
        return $ ASTI.MultiPort checkedFor
    check context portDef = do
        checkedId <- check context (ASTF.portId portDef)
        return $ ASTI.Port checkedId

instance Checkable ASTF.NetSpec (Either ASTI.NodeDecl ASTI.ModuleInst) where
    check context (ASTF.NodeDeclSpec decl) = do
        checkedDecl <- check context decl
        return $ Left checkedDecl
    check context (ASTF.ModuleInstSpec inst) = do
        checkedInst <- check context inst
        return $ Right checkedInst

instance Checkable ASTF.ModuleInst ASTI.ModuleInst where
    check context (ASTF.MultiModuleInst for) = do
        checkedFor <- check context for
        return $ ASTI.MultiModuleInst checkedFor
    check context ast = do
        let
            nameSpace = ASTF.nameSpace ast
            name = ASTF.moduleName ast
            arguments = ASTF.arguments ast
            portMaps = ASTF.portMappings ast
        mod <- getModule context name
        checkedNameSpace <- check context nameSpace
        checkArgCount name mod arguments
        checkedArgs <- checkArgTypes name mod arguments 
        inPortMap  <- check context $ filter isInMap  portMaps
        outPortMap <- check context $ filter isOutMap portMaps
        return ASTI.ModuleInst
            { ASTI.nameSpace  = checkedNameSpace
            , ASTI.moduleName = name
            , ASTI.arguments  = checkedArgs
            , ASTI.inPortMap  = inPortMap
            , ASTI.outPortMap = outPortMap
            }
        where
            isInMap (ASTF.InputPortMap {}) = True
            isInMap (ASTF.MultiPortMap for) = isInMap $ ASTF.body for
            isInMap _ = False
            isOutMap = not . isInMap
            checkArgCount modName mod args = do
                let
                    paramc = length $ ASTI.paramNames mod
                    argc = length args
                if argc == paramc
                    then return ()
                    else Left $ CheckFailure [WrongNumberOfArgs modName paramc argc]
            checkArgTypes modName mod args = do
                let
                    paramNames = ASTI.paramNames mod
                checkedArgs <- forAll id $ zipWith (checkArgType modName mod) args paramNames
                return $ Map.fromList $ zip paramNames checkedArgs
            checkArgType modName mod arg paramName = do
                let
                    expected = getParameterType mod paramName
                case arg of
                    ASTF.AddressArg value -> do
                        if expected == ASTI.AddressParam
                            then return $ ASTI.AddressArg value
                            else Left $ mismatch expected ASTI.AddressParam
                    ASTF.NumberArg value -> do
                        if expected == ASTI.NumberParam
                            then return $ ASTI.NumberArg value
                            else Left $ mismatch expected ASTI.NumberParam
                    ASTF.ParamArg name -> do
                        checkParamType context name expected
                        return $ ASTI.ParamArg name
                where
                    mismatch expected actual = CheckFailure [ArgTypeMismatch modName paramName expected actual]

instance Checkable ASTF.PortMap ASTI.PortMap where
    check context (ASTF.MultiPortMap for) = do
        checkedFor <- check context for
        return $ ASTI.MultiPortMap checkedFor
    check context portMap = do
        let
            mappedId = ASTF.mappedId portMap
            mappedPort = ASTF.mappedPort portMap
            idents = [mappedId, mappedPort]
        checkedIds <- check context idents
        return $ ASTI.PortMap
            { ASTI.mappedId   = head checkedIds
            , ASTI.mappedPort = last checkedIds
            }

instance Checkable ASTF.NodeDecl ASTI.NodeDecl where
    check context (ASTF.MultiNodeDecl for) = do
        checkedFor <- check context for
        return $ ASTI.MultiNodeDecl checkedFor
    check context ast = do
        let
            nodeId = ASTF.nodeId ast
            nodeSpec = ASTF.nodeSpec ast
        checkedId <- check context nodeId
        checkedSpec <- check context nodeSpec
        return ASTI.NodeDecl
            { ASTI.nodeId   = checkedId
            , ASTI.nodeSpec = checkedSpec
            }

instance Checkable ASTF.Identifier ASTI.Identifier where
    check _ (ASTF.SimpleIdent name) = return $ ASTI.SimpleIdent name
    check context ast = do
        let
            prefix = ASTF.prefix ast
            varName = ASTF.varName ast
            suffix = ASTF.suffix ast
        checkVarInScope context varName
        checkedSuffix <- case suffix of
            Nothing    -> return Nothing
            Just ident -> do
                checkedIdent <- check context ident
                return $ Just checkedIdent
        return ASTI.TemplateIdent
            { ASTI.prefix  = prefix
            , ASTI.varName = varName
            , ASTI.suffix  = checkedSuffix
            }

instance Checkable ASTF.NodeSpec ASTI.NodeSpec where
    check context ast = do
        let 
            nodeType = ASTF.nodeType ast
            accept = ASTF.accept ast
            translate = ASTF.translate ast
            overlay = ASTF.overlay ast
        checkedAccept <- check context accept
        checkedTranslate <- check context translate
        checkedOverlay <- case overlay of
            Nothing    -> return Nothing
            Just ident -> do
                checkedIdent <- check context ident
                return $ Just checkedIdent
        return ASTI.NodeSpec
            { ASTI.nodeType  = nodeType
            , ASTI.accept    = checkedAccept
            , ASTI.translate = checkedTranslate
            , ASTI.overlay   = checkedOverlay
            }

instance Checkable ASTF.BlockSpec ASTI.BlockSpec where
    check context (ASTF.SingletonBlock address) = do
        checkedAddress <- check context address
        return ASTI.SingletonBlock
            { ASTI.address = checkedAddress }
    check context (ASTF.RangeBlock base limit) = do
        let
            addresses = [base, limit]
        checkedAddresses <- check context addresses
        return ASTI.RangeBlock
            { ASTI.base  = head checkedAddresses
            , ASTI.limit = last checkedAddresses
            }
    check context (ASTF.LengthBlock base bits) = do
        checkedBase <- check context base
        return ASTI.LengthBlock
            { ASTI.base = checkedBase
            , ASTI.bits = bits
            }

instance Checkable ASTF.MapSpec ASTI.MapSpec where
    check context ast = do
        let
            block = ASTF.block ast
            destNode = ASTF.destNode ast
            destBase = ASTF.destBase ast
        checkedBlock <- check context block
        checkedDestNode <- check context destNode
        checkedDestBase <- case destBase of
            Nothing      -> return Nothing
            Just address -> do
                checkedAddress <- check context address
                return $ Just checkedAddress
        return ASTI.MapSpec
            { ASTI.block    = checkedBlock
            , ASTI.destNode = checkedDestNode
            , ASTI.destBase = checkedDestBase
            }

instance Checkable ASTF.Address ASTI.Address where
    check _ (ASTF.NumberAddress value) = do
        return $ ASTI.NumberAddress value
    check context (ASTF.ParamAddress name) = do
        checkParamType context name ASTI.AddressParam
        return $ ASTI.ParamAddress name

instance Checkable a b => Checkable (ASTF.For a) (ASTI.For b) where
    check context ast = do
        let
            varRanges = ASTF.varRanges ast
            varNames = map ASTF.var varRanges
            body = ASTF.body ast
        checkDuplicates varNames DuplicateVariable
        ranges <- check context varRanges
        let
            currentVars = vars context
            bodyVars = currentVars `Set.union` (Set.fromList varNames)
            bodyContext = context
                { vars = bodyVars }
        checkedBody <- check bodyContext body
        let
            checkedVarRanges = Map.fromList $ zip varNames ranges
        return ASTI.For
                { ASTI.varRanges = checkedVarRanges
                , ASTI.body      = checkedBody
                }

instance Checkable ASTF.ForVarRange ASTI.ForRange where
    check context ast = do
        let
            limits = [ASTF.start ast, ASTF.end ast]
        checkedLimits <- check context limits
        return ASTI.ForRange
            { ASTI.start = head checkedLimits
            , ASTI.end   = last checkedLimits
            }

instance Checkable ASTF.ForLimit ASTI.ForLimit where
    check _ (ASTF.NumberLimit value) = do
        return $ ASTI.NumberLimit value
    check context (ASTF.ParamLimit name) = do
        checkParamType context name ASTI.NumberParam
        return $ ASTI.ParamLimit name

instance Checkable a b => Checkable [a] [b] where
    check context as = do
        let
            bs = map (check context) as
            es = concat $ map failedChecks (lefts bs)
        case es of
            [] -> return $ rights bs
            _  -> Left $ CheckFailure es
--
-- Helpers
--
rootModule :: ASTF.SockeyeSpec -> ASTF.Module
rootModule spec =
    let
        body = ASTF.ModuleBody
            { ASTF.ports = []
            , ASTF.moduleNet = ASTF.net spec
            }
    in ASTF.Module
        { ASTF.name       = "@root"
        , ASTF.parameters = []
        , ASTF.moduleBody = body
        }

getModule :: Context -> String -> Either CheckFailure ASTI.Module
getModule context name = do
    let
        modMap = ASTI.modules $ spec context
    case Map.lookup name modMap of
        Nothing -> Left $ CheckFailure [NoSuchModule name]
        Just m  -> return m

getCurrentModule :: Context -> ASTI.Module
getCurrentModule context =
    let
        modMap = ASTI.modules $ spec context
    in modMap Map.! (moduleName context)

getParameterType :: ASTI.Module -> String -> ASTI.ModuleParamType
getParameterType mod name =
    let
        paramMap = ASTI.paramTypeMap mod
    in paramMap Map.! name

getCurrentParameterType :: Context -> String -> Either CheckFailure ASTI.ModuleParamType
getCurrentParameterType context name = do
    let
        mod = getCurrentModule context
        paramMap = ASTI.paramTypeMap mod
    case Map.lookup name paramMap of
        Nothing -> Left $ CheckFailure [NoSuchParameter name]
        Just t  -> return t

forAll :: (a -> Either CheckFailure b) -> [a] -> Either CheckFailure [b]
forAll f as = do
    let
        bs = map f as
        es = concat $ map failedChecks (lefts bs)
    case es of
        [] -> return $ rights bs
        _  -> Left $ CheckFailure es

checkDuplicates :: [String] -> (String -> FailedCheck) -> Either CheckFailure ()
checkDuplicates names failure = do
    let
        duplicates = duplicateNames names
    case duplicates of
        [] -> return ()
        _  -> Left $ CheckFailure (map failure duplicates)
    where
        duplicateNames [] = []
        duplicateNames (x:xs)
            | x `elem` xs = nub $ [x] ++ duplicateNames xs
            | otherwise = duplicateNames xs

checkVarInScope :: Context -> String -> Either CheckFailure ()
checkVarInScope context name = do
    if name `Set.member` (vars context)
        then return ()
        else Left $ CheckFailure [NoSuchVariable name]


checkParamType :: Context -> String -> ASTI.ModuleParamType -> Either CheckFailure ()
checkParamType context name expected = do
    actual <- getCurrentParameterType context name
    if actual == expected
        then return ()
        else Left $ mismatch actual
    where
        mismatch t = CheckFailure [ParamTypeMismatch name expected t]