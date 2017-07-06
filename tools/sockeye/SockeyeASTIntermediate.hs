{-
  SockeyeASTIntermediate.hs: Intermediate AST for Sockeye

  Part of Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeASTIntermediate
    ( module SockeyeASTIntermediate
    , module SockeyeASTFrontend
    ) where

import Data.Map (Map)
import Data.Set (Set)

import SockeyeASTFrontend
    ( Identifier(SimpleIdent, TemplateIdent)
    , prefix, varName, suffix
    , ModuleParamType(NumberParam, AddressParam)
    , ModuleArg(AddressArg, NumberArg, ParamArg)
    , NodeSpec(NodeSpec)
    , nodeType, accept, translate, overlay
    , NodeType(Memory, Device)
    , BlockSpec(SingletonBlock, RangeBlock, LengthBlock)
    , address, base, limit, bits
    , MapSpec(MapSpec)
    , block, destNode, destBase
    , Address(NumberAddress, ParamAddress)
    , ForLimit(NumberLimit, ParamLimit)
    )

newtype SockeyeSpec = SockeyeSpec
    { modules :: Map String Module }
    deriving (Show)

data Module = Module
    { paramNames   :: [String]
    , paramTypeMap :: Map String ModuleParamType
    , inputPorts   :: [Port]
    , outputPorts  :: [Port]
    , nodeDecls    :: [NodeDecl]
    , moduleInsts  :: [ModuleInst]
    } deriving (Show)

data Port
    = Port Identifier
    | MultiPort (For Port)
    deriving (Show)

data ModuleInst
    = ModuleInst
        { nameSpace  :: Identifier
        , moduleName :: String
        , arguments  :: Map String ModuleArg
        , inPortMap  :: [PortMap]
        , outPortMap :: [PortMap]
        }
    | MultiModuleInst (For ModuleInst)
    deriving (Show)

data PortMap
    = PortMap
        { mappedId   :: Identifier
        , mappedPort :: Identifier
        }
    | MultiPortMap (For PortMap)
    deriving (Show)

data NodeDecl
    = NodeDecl
        { nodeId   :: Identifier
        , nodeSpec :: NodeSpec
        }
    | MultiNodeDecl (For NodeDecl)
    deriving (Show)

data For a 
    = For
        { varRanges :: Map String ForRange
        , body      :: a
        } deriving (Show)

data ForRange
    = ForRange
    { start :: ForLimit
    , end   :: ForLimit
    } deriving (Show)
