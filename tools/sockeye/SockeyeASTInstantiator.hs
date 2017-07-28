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
    , module SockeyeAST
    ) where

import Data.Map (Map)

import SockeyeAST
    ( NodeType(Other, Memory, Device) )

data SockeyeSpec = SockeyeSpec
    { root :: ModuleInst
    , modules :: Map String Module
    } deriving (Show)

data Module = Module
    { inputPorts   :: Map String Integer
    , outputPorts  :: Map String Integer
    , nodeDecls    :: Map String NodeSpec
    , moduleInsts  :: Map String ModuleInst
    } deriving (Show)

data ModuleInst
    = ModuleInst
        { moduleName :: String
        , inPortMap  :: Map String String
        , outPortMap :: Map String String
        } deriving (Show)

data NodeSpec = NodeSpec
    { nodeType  :: NodeType
    , accept    :: [BlockSpec]
    , translate :: [MapSpec]
    , reserved  :: [BlockSpec]
    , overlay   :: Maybe OverlaySpec
    } deriving (Show)

data BlockSpec 
    = BlockSpec
        { base  :: !Integer
        , limit :: !Integer
        } deriving (Show)

data MapSpec 
    = MapSpec
        { block    :: BlockSpec
        , destNode :: !String
        , destBase :: !Integer
        } deriving (Show)

data OverlaySpec
    = OverlaySpec
        { over  :: !String
        , width :: !Integer
        } deriving (Show)

