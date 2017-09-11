{-
    SockeyeASTParser.hs: AST for the Sockeye parser

    Part of Sockeye

    Copyright (c) 2017, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

module SockeyeASTParser 
( module SockeyeASTParser
, module SockeyeASTTypeChecker
) where

import SockeyeASTTypeChecker
    ( Identifier(SimpleIdent, TemplateIdent)
    , prefix, varName, suffix
    , ModuleParamType(NaturalParam, AddressParam)
    , ModuleArg(AddressArg, NaturalArg, ParamArg)
    , NodeSpec(NodeSpec)
    , nodeType, accept, translate, reserved, overlay
    , NodeType(Core, Device, Memory, Other)
    , BlockSpec(SingletonBlock, RangeBlock, LengthBlock)
    , base, limit, bits
    , MapSpec(MapSpec)
    , OverlaySpec(OverlaySpec)
    , over, width
    , block, destNode, destBase
    , Address(LiteralAddress, ParamAddress)
    , ForLimit(LiteralLimit, ParamLimit)
    )

data SockeyeSpec = SockeyeSpec
    { imports :: [Import]
    , modules :: [Module]
    , net     :: [NetSpec]
    } deriving (Show)

data Import = Import 
    { filePath :: !FilePath }
    deriving (Show)

data Module = Module
    { name       :: String
    , parameters :: [ModuleParam]
    , moduleBody :: ModuleBody
    } deriving (Show)

data ModuleParam = ModuleParam
    { paramName :: !String
    , paramType :: ModuleParamType
    } deriving (Show)

data ModuleBody = ModuleBody
    { ports     :: [Port]
    , moduleNet :: [NetSpec]
    } deriving (Show)

data NetSpec
    = ModuleInstSpec ModuleInst
    | NodeDeclSpec NodeDecl
    deriving (Show)

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
        { moduleName   :: String
        , namespace    :: Identifier
        , arguments    :: [ModuleArg]
        , portMappings :: [PortMap]
        }
    | MultiModuleInst (For ModuleInst)
    deriving (Show)

data PortMap
    = InputPortMap
        { mappedId   :: Identifier
        , mappedPort :: Identifier
        }
    | OutputPortMap
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
        { varRanges :: [ForVarRange]
        , body      :: a
        } deriving (Show)

data ForVarRange
    = ForVarRange
    { var   :: !String
    , start :: ForLimit
    , end   :: ForLimit
    } deriving (Show)
