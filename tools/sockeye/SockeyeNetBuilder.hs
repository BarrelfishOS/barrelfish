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

import qualified Data.Map as Map

import qualified SockeyeAST as AST
import qualified SockeyeASTDecodingNet as NetAST

newtype CheckFailure = CheckFailure
    { message :: String }

instance Show CheckFailure where
    show f = unlines $ ["", message f]

sockeyeBuildNet :: AST.SockeyeSpec -> Either CheckFailure NetAST.NetSpec
sockeyeBuildNet ast = buildNet ast

class NetSource a b where
    buildNet :: a -> Either CheckFailure b

instance NetSource AST.SockeyeSpec NetAST.NetSpec where
    buildNet ast = do
        let
            rootInst = AST.ModuleInst
                { AST.nameSpace  = AST.SimpleIdent ""
                , AST.moduleName = "@root"
                , AST.arguments  = Map.empty
                , AST.inPortMap  = []
                , AST.outPortMap = []
                }
        buildNet rootInst

instance NetSource AST.ModuleInst NetAST.NetSpec where
    buildNet ast = Left $ CheckFailure "ModuleInst conversion not yet implemented"