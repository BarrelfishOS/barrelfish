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

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either

import qualified SockeyeASTFrontend as ASTF
import qualified SockeyeASTIntermediate as ASTI

data Context = Context
    { spec       :: ASTI.SockeyeSpec
    , moduleName :: !String
    , vars       :: Set String
    , instModule :: String
    }

data FailedCheckType
    = DuplicateModule String
    | DuplicateParameter String
    | DuplicateVariable String
    | NoSuchModule String
    | NoSuchParameter String
    | NoSuchVariable String
    | ParamTypeMismatch String ASTI.ModuleParamType ASTI.ModuleParamType
    | WrongNumberOfArgs Int Int
    | ArgTypeMismatch String ASTI.ModuleParamType ASTI.ModuleParamType

instance Show FailedCheckType where
    show (DuplicateModule name)          = concat ["Multiple definitions for module '", name, "'"]
    show (DuplicateParameter name)       = concat ["Multiple parameters named '", name, "'"]
    show (DuplicateVariable name)        = concat ["Multiple definitions for variable '", name, "'"]
    show (NoSuchModule name)             = concat ["No definition for module '", name, "'"]
    show (NoSuchParameter name)          = concat ["Parameter '", name, "' not in scope"]
    show (NoSuchVariable name)           = concat ["Variable '", name, "' not in scope"]
    show (WrongNumberOfArgs takes given) =
        let arg = if takes == 1
            then "argument"
            else "arguments"
        in concat ["Module takes ", show takes, " ", arg, ", given ", show given]
    show (ParamTypeMismatch name expected actual) =
        concat ["Expected type '", show expected, "' but '", name, "' has type '", show actual, "'"]
    show (ArgTypeMismatch name expected actual) =
        concat ["Type mismatch for argument '", name, "': Expected '", show expected, "', given '", show actual, "'"]

data FailedCheck = FailedCheck
    { failure  :: FailedCheckType
    , inModule :: !String
    }

newtype CheckFailure = CheckFailure
    { failedChecks :: [FailedCheck] }

instance Show CheckFailure where
    show (CheckFailure fs) = 
        let
            modules = nub $ map inModule fs
        in unlines $ concat (map showFailsForModule modules)
        where
            showFailsForModule name =
                let
                    title = "\nIn module '" ++ name ++ "':"
                    fails = filter (\f -> name == inModule f) fs
                in if name == ""
                    then "":showFails 0 fails
                    else title:showFails 1 fails
            showFails indentLevel fs =
                let
                    indent = replicate (indentLevel * 4) ' '
                in map ((indent ++) . showFail) fs
            showFail f = (show $ failure f)

checkFailure :: Context -> [FailedCheckType] -> CheckFailure
checkFailure context fs = CheckFailure $ map failCheck fs
    where
        failCheck f = FailedCheck
            { failure  = f
            , inModule = moduleName context
            }

checkSockeye :: ASTF.SockeyeSpec -> Either CheckFailure ASTI.SockeyeSpec
checkSockeye ast = do
    let
        emptySpec = ASTI.SockeyeSpec Map.empty
        initContext = Context
            { spec       = emptySpec
            , moduleName = ""
            , vars       = Set.empty
            , instModule = ""
            }
    symbolTable <- buildSymbolTable initContext ast
    let
        context = initContext
            { spec = symbolTable }
    check context ast

--
-- Build Symbol table
--
class SymbolSource a b where
    buildSymbolTable :: Context -> a -> Either CheckFailure b

instance SymbolSource ASTF.SockeyeSpec ASTI.SockeyeSpec where
    buildSymbolTable context ast = do
        let
            modules = (rootModule ast):(ASTF.modules ast)
            names = map ASTF.name modules
        checkDuplicates context names DuplicateModule
        symbolTables <- buildSymbolTable context modules
        let
            moduleMap = Map.fromList $ zip names symbolTables
        return ASTI.SockeyeSpec
                { ASTI.modules = moduleMap }

instance SymbolSource ASTF.Module ASTI.Module where
    buildSymbolTable context ast = do
        let
            name = ASTF.name ast
            paramNames = map ASTF.paramName (ASTF.parameters ast)
            paramTypes = map ASTF.paramType (ASTF.parameters ast)
            moduleContext = context
                { moduleName = name}
        checkDuplicates moduleContext paramNames DuplicateParameter
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
    buildSymbolTable context as = forAll (buildSymbolTable context) as

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
        let
            paramNames = ASTI.paramNames mod
            instContext = context
                { instModule = name }
        checkedArgs <- checkArgs instContext arguments
        checkedNameSpace <- check instContext nameSpace
        inPortMap  <- check instContext $ filter isInMap  portMaps
        outPortMap <- check instContext $ filter isOutMap portMaps
        let
            argMap = Map.fromList $ zip paramNames checkedArgs
        return ASTI.ModuleInst
            { ASTI.nameSpace  = checkedNameSpace
            , ASTI.moduleName = name
            , ASTI.arguments  = argMap
            , ASTI.inPortMap  = inPortMap
            , ASTI.outPortMap = outPortMap
            }
        where
            isInMap (ASTF.InputPortMap {}) = True
            isInMap (ASTF.MultiPortMap for) = isInMap $ ASTF.body for
            isInMap _ = False
            isOutMap = not . isInMap
            checkArgs context args = do
                mod <- getInstantiatedModule context
                let
                    typeMap = ASTI.paramTypeMap mod
                    paramNames = ASTI.paramNames mod
                    paramTypes = map (typeMap Map.!) paramNames
                    params = zip paramNames paramTypes
                checkArgCount paramNames args
                forAll id $ zipWith checkArgType params args
                where
                    checkArgCount params args = do
                        let
                            paramc = length params
                            argc = length args
                        if argc == paramc
                            then return ()
                            else Left $ checkFailure context [WrongNumberOfArgs paramc argc]
                    checkArgType (name, expected) arg = do
                        case arg of
                            ASTF.AddressArg value -> do
                                if expected == ASTI.AddressParam
                                    then return $ ASTI.AddressArg value
                                    else Left $ mismatch ASTI.AddressParam
                            ASTF.NumberArg value -> do
                                if expected == ASTI.NumberParam
                                    then return $ ASTI.NumberArg value
                                    else Left $ mismatch ASTI.NumberParam
                            ASTF.ParamArg pName -> do
                                checkParamType context pName expected
                                return $ ASTI.ParamArg pName
                        where
                            mismatch t = checkFailure context [ArgTypeMismatch name expected t]

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
        checkDuplicates context varNames DuplicateVariable
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
    check context as = forAll (check context) as
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
        Nothing -> Left $ checkFailure context [NoSuchModule name]
        Just m  -> return m

getCurrentModule :: Context -> ASTI.Module
getCurrentModule context =
    let
        modMap = ASTI.modules $ spec context
    in modMap Map.! (moduleName context)

getInstantiatedModule :: Context -> Either CheckFailure ASTI.Module
getInstantiatedModule context =
    let
        modName = instModule context
    in getModule context modName

getParameterType :: Context -> String -> Either CheckFailure ASTI.ModuleParamType
getParameterType context name = do
    let
        mod = getCurrentModule context
        paramMap = ASTI.paramTypeMap mod
    case Map.lookup name paramMap of
        Nothing -> Left $ checkFailure context [NoSuchParameter name]
        Just t  -> return t

forAll :: (a -> Either CheckFailure b) -> [a] -> Either CheckFailure [b]
forAll f as = do
    let
        bs = map f as
        es = concat $ map failedChecks (lefts bs)
    case es of
        [] -> return $ rights bs
        _  -> Left $ CheckFailure es

checkDuplicates :: Context -> [String] -> (String -> FailedCheckType) -> Either CheckFailure ()
checkDuplicates context names failure = do
    let
        duplicates = duplicateNames names
    case duplicates of
        [] -> return ()
        _  -> Left $ checkFailure context (map failure duplicates)
    where
        duplicateNames [] = []
        duplicateNames (x:xs)
            | x `elem` xs = nub $ [x] ++ duplicateNames xs
            | otherwise = duplicateNames xs

checkVarInScope :: Context -> String -> Either CheckFailure ()
checkVarInScope context name = do
    if name `Set.member` (vars context)
        then return ()
        else Left $ checkFailure context [NoSuchVariable name]


checkParamType :: Context -> String -> ASTI.ModuleParamType -> Either CheckFailure ()
checkParamType context name expected = do
    actual <- getParameterType context name
    if actual == expected
        then return ()
        else Left $ mismatch actual
    where
        mismatch t = checkFailure context [ParamTypeMismatch name expected t]
