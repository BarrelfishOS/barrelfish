{-
  SockeyeASTTypeChecker.hs: Type checked AST for Sockeye

  Part of Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeASTTypeChecker
 ( module SockeyeASTTypeChecker
 , module SockeyeASTInstantiator
 ) where

import Data.Map (Map)

import SockeyeASTInstantiator
    ( NodeType(Core, Device, Memory, Other) )

data SockeyeSpec = SockeyeSpec
    { root    :: ModuleInst
    , modules :: Map String Module }
    deriving (Show)

data Module = Module
    { paramNames   :: [String]
    , paramTypeMap :: Map String ModuleParamType
    , ports        :: [Port]
    , moduleInsts  :: [ModuleInst]
    , nodeDecls    :: [NodeDecl]
    } deriving (Show)

data ModuleParamType 
    = NaturalParam
    | AddressParam
    deriving (Eq)

instance Show ModuleParamType where
    show NaturalParam = "nat"
    show AddressParam = "addr"

data Port
    = InputPort 
        { portId    :: Identifier
        , portWidth :: !Integer
        }
    | OutputPort
        { portId    :: Identifier
        , portWidth :: !Integer
        }
    | MultiPort (For Port)
    deriving (Show)

data ModuleInst
    = ModuleInst
        { namespace  :: Maybe Identifier
        , moduleName :: String
        , arguments  :: Map String ModuleArg
        , inPortMap  :: [PortMap]
        , outPortMap :: [PortMap]
        }
    | MultiModuleInst (For ModuleInst)
    deriving (Show)

data ModuleArg
    = NumericalArg !Integer
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
    = SimpleIdent 
        { prefix  :: !String }
    | TemplateIdent
        { prefix  :: !String
        , varName :: !String
        , suffix  :: Maybe Identifier
        }
    deriving (Show)

data NodeSpec = NodeSpec
    { nodeType  :: NodeType
    , accept    :: [BlockSpec]
    , translate :: [MapSpec]
    , reserved  :: [BlockSpec]
    , overlay   :: Maybe OverlaySpec
    } deriving (Show)

data BlockSpec 
    = SingletonBlock
        { base :: Address }
    | RangeBlock
        { base  :: Address
        , limit :: Address
        }
    | LengthBlock
        { base :: Address
        , bits :: !Integer
        }
    deriving (Show)

data MapSpec 
    = MapSpec
        { block    :: BlockSpec
        , destNode :: Identifier
        , destBase :: Maybe Address
        } deriving (Show)

data OverlaySpec
    = OverlaySpec
        { over  :: Identifier
        , width :: !Integer
        } deriving (Show)

data Address
    = LiteralAddress !Integer
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
    = LiteralLimit !Integer
    | ParamLimit !String
    deriving (Show)
