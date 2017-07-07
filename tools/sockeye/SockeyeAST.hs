{-
  SockeyeAST.hs: AST for Sockeye

  Part of Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeAST where

import Data.Map (Map)
import Data.Set (Set)

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

data ModuleParamType 
    = NumberParam
    | AddressParam
    deriving (Eq)

instance Show ModuleParamType where
    show NumberParam = "nat"
    show AddressParam = "addr"

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

data ModuleArg
    = AddressArg !Word
    | NumberArg !Word
    | ParamArg !String
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

data Identifier
    = SimpleIdent !String
    | TemplateIdent
        { prefix  :: !String
        , varName :: !String
        , suffix  :: Maybe Identifier
        }
    deriving (Show)

data NodeSpec = NodeSpec
    { nodeType  :: Maybe NodeType
    , accept    :: [BlockSpec]
    , translate :: [MapSpec]
    , overlay   :: Maybe Identifier
    } deriving (Show)

data NodeType
    = Memory
    | Device
    deriving (Show)

data BlockSpec 
    = SingletonBlock
        { address :: !Address }
    | RangeBlock
        { base  :: !Address
        , limit :: !Address
        }
    | LengthBlock
        { base :: !Address
        , bits :: !Word
        }
    deriving (Show)

data MapSpec 
    = MapSpec
        { block    :: BlockSpec
        , destNode :: Identifier
        , destBase :: Maybe Address
        } deriving (Show)

data Address
    = NumberAddress !Word
    | ParamAddress !String
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

data ForLimit 
    = NumberLimit !Word
    | ParamLimit !String
    deriving (Show)
