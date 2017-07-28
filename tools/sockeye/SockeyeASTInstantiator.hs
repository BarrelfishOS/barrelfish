{-
  SockeyeASTInstantiator.hs: AST with instantiated modules for Sockeye

  Part of Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeASTInstantiator
    ( module SockeyeASTInstantiator
    , module SockeyeASTDecodingNet
    ) where

import Data.Map (Map)

import SockeyeASTDecodingNet
    ( NodeType(Other, Memory, Device)
    , BlockSpec(BlockSpec)
    , base, limit
    , Address
    )

data SockeyeSpec = SockeyeSpec
    { root :: ModuleInst
    , modules :: Map String Module
    } deriving (Show)

data Module = Module
    { inputPorts   :: PortMap
    , outputPorts  :: PortMap
    , moduleInsts  :: ModuleInstMap
    , nodeDecls    :: NodeDeclMap
    } deriving (Show)

type PortMap = Map Identifier Integer
type ModuleInstMap = Map String ModuleInst
type NodeDeclMap = Map Identifier NodeSpec

data ModuleInst
    = ModuleInst
        { moduleName :: Identifier
        , inPortMap  :: PortMappingMap
        , outPortMap :: PortMappingMap
        } deriving (Show)

type PortMappingMap = Map Identifier Identifier

type Identifier = String

data NodeSpec = NodeSpec
    { nodeType  :: NodeType
    , accept    :: [BlockSpec]
    , translate :: [MapSpec]
    , reserved  :: [BlockSpec]
    , overlay   :: Maybe OverlaySpec
    } deriving (Show)

data MapSpec 
    = MapSpec
        { srcBlock :: BlockSpec
        , destNode :: !Identifier
        , destBase :: !Address
        } deriving (Show)

data OverlaySpec
    = OverlaySpec
        { over  :: !Identifier
        , width :: !Integer
        } deriving (Show)

