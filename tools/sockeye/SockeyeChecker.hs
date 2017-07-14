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
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either

import qualified SockeyeASTParser as ParseAST
import qualified SockeyeAST as AST

data Context = Context
    { spec       :: AST.SockeyeSpec
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
    | ParamTypeMismatch String AST.ModuleParamType AST.ModuleParamType
    | WrongNumberOfArgs String Int Int
    | ArgTypeMismatch String String AST.ModuleParamType AST.ModuleParamType

instance Show FailedCheckType where
    show (DuplicateModule name)          = concat ["Multiple definitions for module '", name, "'"]
    show (DuplicateParameter name)       = concat ["Multiple parameters named '", name, "'"]
    show (DuplicateVariable name)        = concat ["Multiple definitions for variable '", name, "'"]
    show (NoSuchModule name)             = concat ["No definition for module '", name, "'"]
    show (NoSuchParameter name)          = concat ["Parameter '", name, "' not in scope"]
    show (NoSuchVariable name)           = concat ["Variable '", name, "' not in scope"]
    show (WrongNumberOfArgs name takes given) =
        let arg = if takes == 1
            then "argument"
            else "arguments"
        in concat ["Module '", name, "' takes ", show takes, " ", arg, ", given ", show given]
    show (ParamTypeMismatch name expected actual) =
        concat ["Expected type '", show expected, "' but '", name, "' has type '", show actual, "'"]
    show (ArgTypeMismatch modName name expected actual) =
        concat ["Type mismatch for argument '", name, "' for module '", modName, "': Expected '", show expected, "', given '", show actual, "'"]

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
                in if name == "@root"
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

checkSockeye :: ParseAST.SockeyeSpec -> Either CheckFailure AST.SockeyeSpec
checkSockeye ast = do
    let
        emptySpec = AST.SockeyeSpec Map.empty
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

instance SymbolSource ParseAST.SockeyeSpec AST.SockeyeSpec where
    buildSymbolTable context ast = do
        let
            modules = (rootModule ast):(ParseAST.modules ast)
            names = map ParseAST.name modules
        checkDuplicates context names DuplicateModule
        symbolTables <- buildSymbolTable context modules
        let
            moduleMap = Map.fromList $ zip names symbolTables
        return AST.SockeyeSpec
                { AST.modules = moduleMap }

instance SymbolSource ParseAST.Module AST.Module where
    buildSymbolTable context ast = do
        let
            name = ParseAST.name ast
            paramNames = map ParseAST.paramName (ParseAST.parameters ast)
            paramTypes = map ParseAST.paramType (ParseAST.parameters ast)
            moduleContext = context
                { moduleName = name}
        checkDuplicates moduleContext paramNames DuplicateParameter
        let
            paramTypeMap = Map.fromList $ zip paramNames paramTypes
        return AST.Module
            { AST.paramNames   = paramNames
            , AST.paramTypeMap = paramTypeMap
            , AST.inputPorts   = []
            , AST.outputPorts  = []
            , AST.nodeDecls    = []
            , AST.moduleInsts  = []
            }

instance SymbolSource a b => SymbolSource [a] [b] where
    buildSymbolTable context as = forAll (buildSymbolTable context) as

--
-- Check module bodies
--
class Checkable a b where
    check :: Context -> a -> Either CheckFailure b

instance Checkable ParseAST.SockeyeSpec AST.SockeyeSpec where
    check context ast = do
        let
            modules = (rootModule ast):(ParseAST.modules ast)
            names = map ParseAST.name modules
        checked <- check context modules
        let
            sockeyeSpec = spec context
            checkedMap = Map.fromList $ zip names checked
        return sockeyeSpec
            { AST.modules = checkedMap }

instance Checkable ParseAST.Module AST.Module where
    check context ast = do
        let
            name = ParseAST.name ast
            bodyContext = context
                { moduleName = name}
            body = ParseAST.moduleBody ast
            portDefs = ParseAST.ports body
            netSpecs = ParseAST.moduleNet body
        inputPorts  <- check bodyContext $ filter isInPort  portDefs
        outputPorts <- check bodyContext $ filter isOutPort portDefs
        checkedNetSpecs <- check bodyContext netSpecs
        let
            checkedNodeDecls = lefts checkedNetSpecs
            checkedModuleInsts = rights checkedNetSpecs
            mod = getCurrentModule bodyContext
        return mod
            { AST.inputPorts  = inputPorts
            , AST.outputPorts = outputPorts
            , AST.nodeDecls   = checkedNodeDecls
            , AST.moduleInsts = checkedModuleInsts
            }
        where
            isInPort (ParseAST.InputPortDef _) = True
            isInPort (ParseAST.MultiPortDef for) = isInPort $ ParseAST.body for
            isInPort _ = False
            isOutPort = not . isInPort

instance Checkable ParseAST.PortDef AST.Port where
    check context (ParseAST.InputPortDef ident) = do
        checkedId <- check context ident
        return $ AST.InputPort checkedId
    check context (ParseAST.OutputPortDef ident) = do
        checkedId <- check context ident
        return $ AST.OutputPort checkedId
    check context (ParseAST.MultiPortDef for) = do
        checkedFor <- check context for
        return $ AST.MultiPort checkedFor

instance Checkable ParseAST.NetSpec (Either AST.NodeDecl AST.ModuleInst) where
    check context (ParseAST.NodeDeclSpec decl) = do
        checkedDecl <- check context decl
        return $ Left checkedDecl
    check context (ParseAST.ModuleInstSpec inst) = do
        checkedInst <- check context inst
        return $ Right checkedInst

instance Checkable ParseAST.ModuleInst AST.ModuleInst where
    check context (ParseAST.MultiModuleInst for) = do
        checkedFor <- check context for
        return $ AST.MultiModuleInst checkedFor
    check context ast = do
        let
            namespace = ParseAST.namespace ast
            name = ParseAST.moduleName ast
            arguments = ParseAST.arguments ast
            portMaps = ParseAST.portMappings ast
        mod <- getModule context name
        let
            paramNames = AST.paramNames mod
            instContext = context
                { instModule = name }
        checkedArgs <- checkArgs instContext arguments
        checkedNamespace <- check instContext namespace
        inPortMap  <- check instContext $ filter isInMap  portMaps
        outPortMap <- check instContext $ filter isOutMap portMaps
        let
            argMap = Map.fromList $ zip paramNames checkedArgs
        return AST.ModuleInst
            { AST.namespace  = checkedNamespace
            , AST.moduleName = name
            , AST.arguments  = argMap
            , AST.inPortMap  = inPortMap
            , AST.outPortMap = outPortMap
            }
        where
            isInMap (ParseAST.InputPortMap {}) = True
            isInMap (ParseAST.MultiPortMap for) = isInMap $ ParseAST.body for
            isInMap _ = False
            isOutMap = not . isInMap
            checkArgs context args = do
                mod <- getInstantiatedModule context
                let
                    typeMap = AST.paramTypeMap mod
                    paramNames = AST.paramNames mod
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
                            else Left $ checkFailure context [WrongNumberOfArgs (instModule context) paramc argc]
                    checkArgType (name, expected) arg = do
                        case arg of
                            ParseAST.AddressArg value -> do
                                if expected == AST.AddressParam
                                    then return $ AST.AddressArg value
                                    else Left $ mismatch AST.AddressParam
                            ParseAST.NaturalArg value -> do
                                if expected == AST.NaturalParam
                                    then return $ AST.NaturalArg value
                                    else Left $ mismatch AST.NaturalParam
                            ParseAST.ParamArg pName -> do
                                checkParamType context pName expected
                                return $ AST.ParamArg pName
                        where
                            mismatch t = checkFailure context [ArgTypeMismatch (instModule context) name expected t]

instance Checkable ParseAST.PortMap AST.PortMap where
    check context (ParseAST.MultiPortMap for) = do
        checkedFor <- check context for
        return $ AST.MultiPortMap checkedFor
    check context portMap = do
        let
            mappedId = ParseAST.mappedId portMap
            mappedPort = ParseAST.mappedPort portMap
        (checkedId, checkedPort) <- check context (mappedId, mappedPort)
        return $ AST.PortMap
            { AST.mappedId   = checkedId
            , AST.mappedPort = checkedPort
            }

instance Checkable ParseAST.NodeDecl AST.NodeDecl where
    check context (ParseAST.MultiNodeDecl for) = do
        checkedFor <- check context for
        return $ AST.MultiNodeDecl checkedFor
    check context ast = do
        let
            nodeId = ParseAST.nodeId ast
            nodeSpec = ParseAST.nodeSpec ast
        checkedId <- check context nodeId
        checkedSpec <- check context nodeSpec
        return AST.NodeDecl
            { AST.nodeId   = checkedId
            , AST.nodeSpec = checkedSpec
            }

instance Checkable ParseAST.Identifier AST.Identifier where
    check _ (ParseAST.SimpleIdent name) = return $ AST.SimpleIdent name
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
        return AST.TemplateIdent
            { AST.prefix  = prefix
            , AST.varName = varName
            , AST.suffix  = checkedSuffix
            }

instance Checkable ParseAST.NodeSpec AST.NodeSpec where
    check context ast = do
        let 
            nodeType = ParseAST.nodeType ast
            accept = ParseAST.accept ast
            translate = ParseAST.translate ast
            overlay = ParseAST.overlay ast
        checkedAccept <- check context accept
        checkedTranslate <- check context translate
        checkedOverlay <- case overlay of
            Nothing    -> return Nothing
            Just ident -> do
                checkedIdent <- check context ident
                return $ Just checkedIdent
        return AST.NodeSpec
            { AST.nodeType  = nodeType
            , AST.accept    = checkedAccept
            , AST.translate = checkedTranslate
            , AST.overlay   = checkedOverlay
            }

instance Checkable ParseAST.BlockSpec AST.BlockSpec where
    check context (ParseAST.SingletonBlock address) = do
        checkedAddress <- check context address
        return AST.SingletonBlock
            { AST.base = checkedAddress }
    check context (ParseAST.RangeBlock base limit) = do
        (checkedBase, checkedLimit) <- check context (base, limit)
        return AST.RangeBlock
            { AST.base  = checkedBase
            , AST.limit = checkedLimit
            }
    check context (ParseAST.LengthBlock base bits) = do
        checkedBase <- check context base
        return AST.LengthBlock
            { AST.base = checkedBase
            , AST.bits = bits
            }

instance Checkable ParseAST.MapSpec AST.MapSpec where
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
        return AST.MapSpec
            { AST.block    = checkedBlock
            , AST.destNode = checkedDestNode
            , AST.destBase = checkedDestBase
            }

instance Checkable ParseAST.Address AST.Address where
    check _ (ParseAST.LiteralAddress value) = do
        return $ AST.LiteralAddress value
    check context (ParseAST.ParamAddress name) = do
        checkParamType context name AST.AddressParam
        return $ AST.ParamAddress name

instance Checkable a b => Checkable (ParseAST.For a) (AST.For b) where
    check context ast = do
        let
            varRanges = ParseAST.varRanges ast
            varNames = map ParseAST.var varRanges
            body = ParseAST.body ast
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
        return AST.For
                { AST.varRanges = checkedVarRanges
                , AST.body      = checkedBody
                }

instance Checkable ParseAST.ForVarRange AST.ForRange where
    check context ast = do
        let 
            start = ParseAST.start ast
            end = ParseAST.end ast
        (checkedStart, checkedEnd) <- check context (start, end)
        return AST.ForRange
            { AST.start = checkedStart
            , AST.end   = checkedEnd
            }

instance Checkable ParseAST.ForLimit AST.ForLimit where
    check _ (ParseAST.LiteralLimit value) = do
        return $ AST.LiteralLimit value
    check context (ParseAST.ParamLimit name) = do
        checkParamType context name AST.NaturalParam
        return $ AST.ParamLimit name

instance Checkable a b => Checkable [a] [b] where
    check context as = forAll (check context) as

instance (Checkable a c, Checkable b d) => Checkable (a, b) (c, d) where
    check context (a, b) =
        let
            eitherC = check context a
            eitherD = check context b
        in case (eitherC, eitherD) of
            (Right c, Right d) -> return (c, d)
            (Left e1, Left e2) -> Left $ CheckFailure (concat $ map failedChecks [e1, e2])
            (Left e1, _)       -> Left $ e1
            (_      , Left e2) -> Left $ e2
--
-- Helpers
--
rootModule :: ParseAST.SockeyeSpec -> ParseAST.Module
rootModule spec =
    let
        body = ParseAST.ModuleBody
            { ParseAST.ports = []
            , ParseAST.moduleNet = ParseAST.net spec
            }
    in ParseAST.Module
        { ParseAST.name       = "@root"
        , ParseAST.parameters = []
        , ParseAST.moduleBody = body
        }

getModule :: Context -> String -> Either CheckFailure AST.Module
getModule context name = do
    let
        modMap = AST.modules $ spec context
    case Map.lookup name modMap of
        Nothing -> Left $ checkFailure context [NoSuchModule name]
        Just m  -> return m

getCurrentModule :: Context -> AST.Module
getCurrentModule context =
    let
        modMap = AST.modules $ spec context
    in modMap Map.! (moduleName context)

getInstantiatedModule :: Context -> Either CheckFailure AST.Module
getInstantiatedModule context =
    let
        modName = instModule context
    in getModule context modName

getParameterType :: Context -> String -> Either CheckFailure AST.ModuleParamType
getParameterType context name = do
    let
        mod = getCurrentModule context
        paramMap = AST.paramTypeMap mod
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


checkParamType :: Context -> String -> AST.ModuleParamType -> Either CheckFailure ()
checkParamType context name expected = do
    actual <- getParameterType context name
    if actual == expected
        then return ()
        else Left $ mismatch actual
    where
        mismatch t = checkFailure context [ParamTypeMismatch name expected t]
