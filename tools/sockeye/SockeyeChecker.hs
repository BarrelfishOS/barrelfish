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

data FailedCheck
    = DuplicateModule String
    | DuplicateParameter String
    | DuplicateVariable String

instance Show FailedCheck where
    show (DuplicateModule name)    = "Duplicate module '" ++ name ++ "'"
    show (DuplicateParameter name) = "Duplicate parameter '" ++ name ++ "'"
    show (DuplicateVariable name)  = "Duplicate variable '" ++ name ++ "'"

newtype CheckFailure = CheckFailure
    { failedChecks :: [FailedCheck] }

instance Show CheckFailure where
    show (CheckFailure fs) = unlines $ map (("    " ++) . show) fs

checkSockeye :: ASTF.SockeyeSpec -> Either CheckFailure ASTI.SockeyeSpec
checkSockeye ast = do
    duplicateFree <- transform ast
    return duplicateFree
-- build symbol table
-- check modules / top level namespace
--  - no duplicate identifiers
--  - no duplicate namespaces
--  - all instantiated modules must exist
--  - all nodes in maps / overlays must exist
--  - all input ports must be specified
--  - 

-- checkModules :: ASTF.SockeyeSpec -> ASTI.SockeyeSpec -> Either CheckFailure ASTI.SockeyeSpec

class ASTTransformable a b where
    transform :: a -> Either CheckFailure b
--
-- Frontend AST -> Intermediate AST
--
instance ASTTransformable ASTF.SockeyeSpec ASTI.SockeyeSpec where
    transform ast = do
        let
            modules = rootModule:(ASTF.modules ast)
            names = map ASTF.name modules
        checkDuplicates names DuplicateModule
        transformed <- checkAll transform modules
        let
            moduleMap = Map.fromList $ zip names transformed
        return ASTI.SockeyeSpec
                { ASTI.modules = moduleMap }
        where
            rootModule =
                let
                    body = ASTF.ModuleBody
                        { ASTF.ports = []
                        , ASTF.moduleNet = ASTF.net ast
                        }
                in ASTF.Module
                    { ASTF.name       = "@root"
                    , ASTF.parameters = []
                    , ASTF.moduleBody = body
                    }   

instance ASTTransformable ASTF.Module ASTI.Module where
    transform ast = do
        let
            paramNames = map ASTF.paramName (ASTF.parameters ast)
            paramTypes = map ASTF.paramType (ASTF.parameters ast)
        checkDuplicates paramNames DuplicateParameter
        let
            portDefs = ASTF.ports $ ASTF.moduleBody ast
        inputPorts <- checkAll transform $ filter isInPort portDefs
        outputPorts <- checkAll transform $ filter (not . isInPort) portDefs
        let
            paramTypeMap = Map.fromList $ zip paramNames paramTypes
        return ASTI.Module
            { ASTI.paramNames  = paramNames
            , ASTI.paramTypes  = paramTypeMap
            , ASTI.inputPorts  = inputPorts
            , ASTI.outputPorts = outputPorts
            , ASTI.nodeDecls   = []
            , ASTI.moduleInsts = []
            }
        where
            isInPort (ASTF.InputPortDef _) = True
            isInPort (ASTF.OutputPortDef _) = False
            isInPort (ASTF.MultiPortDef for) = isInPort $ ASTF.body for

instance ASTTransformable ASTF.PortDef ASTI.Port where
    transform (ASTF.InputPortDef ident) = return $ ASTI.Port ident
    transform (ASTF.OutputPortDef ident) = return $ ASTI.Port ident
    transform (ASTF.MultiPortDef for) = do
        transformed <- transform for
        return $ ASTI.MultiPort transformed

instance ASTTransformable a b => ASTTransformable (ASTF.For a) (ASTI.For b) where
    transform ast = do
        let
            vars = map ASTF.var (ASTF.varRanges ast)
        checkDuplicates vars DuplicateVariable
        ranges <- checkAll transform (ASTF.varRanges ast)
        body <- transform $ ASTF.body ast
        let
            varRanges = Map.fromList $ zip vars ranges
        return ASTI.For
                { ASTI.varRanges = varRanges
                , ASTI.body      = body
                }

instance ASTTransformable ASTF.ForVarRange ASTI.ForRange where
    transform ast = do
        let
            start = ASTF.start ast
            end = ASTF.end ast
        return ASTI.ForRange
            { ASTI.start = start
            , ASTI.end   = end
            }

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

checkAll :: (a -> Either CheckFailure b) -> [a] -> Either CheckFailure [b]
checkAll f as = do
    let
        bs = map f as
        es = concat $ map failedChecks (lefts bs)
    case es of
        [] -> return $ rights bs
        _  -> Left $ CheckFailure es
