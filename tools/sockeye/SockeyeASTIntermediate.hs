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
    ( Identifier(SimpleIdent,TemplateIdent)
    , prefix, varName, suffix
    , ModuleParamType(NumberParam,AddressParam)
    , NodeType(Memory,Device)
    , ForLimit(NumberLimit,ParamLimit)
    )

newtype SockeyeSpec = SockeyeSpec
    { modules :: Map String Module }
    deriving (Show)

data Module = Module
    { paramNames  :: [String]
    , paramTypes  :: Map String ModuleParamType
    , inputPorts  :: [Port]
    , outputPorts :: [Port]
    , nodeDecls   :: [NodeDecl]
    , moduleInsts :: [ModuleInst]
    } deriving (Show)

data Port
    = Port Identifier
    | MultiPort (For Port)
    deriving (Show)

data ModuleInst
    = ModuleInst
        { nameSpace     :: Identifier
        , moduleName    :: String
        , arguments     :: Map String Word
        , inputPortMap  :: [PortMap]
        , outputPortMap :: [PortMap]
        }
    | MultiModuleInst (For ModuleInst)
    deriving (Show)

data PortMap
    = PortMap
        { mappedId :: Identifier
        , portId   :: Identifier
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

data NodeSpec = NodeSpec
    { nodeType  :: Maybe NodeType
    , accept    :: [BlockSpec]
    , translate :: [MapSpec]
    , overlay   :: Maybe String
    } deriving (Show)

data BlockSpec = BlockSpec
    { base  :: !Word
    , limit :: !Word
    } deriving (Show)

data MapSpec = MapSpec
    { block    :: BlockSpec
    , destNode :: !String
    , destBase :: !Word
    } deriving (Show)

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
