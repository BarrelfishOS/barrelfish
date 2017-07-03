{-
    SockeyeASTFrontend.hs: Frontend AST for Sockeye

    Part of Sockeye

    Copyright (c) 2017, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

module SockeyeASTFrontend where

data SockeyeSpec = SockeyeSpec
    { modules :: [Module]
    , net     :: [NetSpec]
    } deriving (Show)

data Module = Module
    { name  :: String
    , parameters  :: [ModuleParam]
    , moduleBody  :: ModuleBody
    } deriving (Show)

data ModuleParam = ModuleParam
    { paramName :: !String
    , paramType :: ModuleParamType
    } deriving (Show)

data ModuleParamType 
    = NumberParam
    | AddressParam
    deriving (Show)

data ModuleBody = ModuleBody
    { ports     :: [PortDef]
    , moduleNet :: [NetSpec]
    } deriving (Show)

data PortDef
    = InputPortDef Identifier
    | OutputPortDef Identifier
    | MultiPortDef (For PortDef)
    deriving (Show)

data NetSpec
    = NodeDeclSpec NodeDecl
    | ModuleInstSpec ModuleInst
    | MultiNetSpec (For [NetSpec])
    deriving (Show)

data ModuleInst
    = ModuleInst
        { moduleName   :: String
        , nameSpace    :: Identifier
        , arguments    :: [ModuleArg]
        , portMappings :: [PortMap]
        } deriving (Show)

data ModuleArg
    = AddressArg !Word
    | NumberArg !Word
    | ParamArg !String
    deriving (Show)

data PortMap
    = InputPortMap
        { port   :: Identifier
        , nodeId :: Identifier
        }
    | OutputPortMap
        { port   :: Identifier
        , nodeId :: Identifier
        }
    | MultiPortMap (For [PortMap])
    deriving (Show)

data NodeDecl
    = NodeDecl
        { nodeIds  :: [Identifier]
        , nodeSpec :: NodeSpec
        } deriving (Show)

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
    = Singleton
        { address :: !Address }
    | Range
        { base  :: !Address
        , limit :: !Address
        }
    | Length
        { base :: !Address
        , bits :: !Word
        }
    deriving (Show)

data Address
    = NumberAddress !Word
    | ParamAddress !String
    deriving (Show)

data MapSpec 
    = MapSpec
        { block :: BlockSpec
        , dests :: [MapDest]
        } deriving (Show)

data MapDest
    = Direct
        { destNode :: Identifier }
    | BaseAddress
        { destNode :: Identifier
        , destBase :: Address
        }
    deriving (Show)

data For a 
    = For
        { varRanges :: [ForVarRange]
        , body     :: a
        } deriving (Show)

data ForVarRange
    = ForVarRange
    { var   :: !String
    , start :: ForLimit
    , end   :: ForLimit
    } deriving (Show)

data ForLimit 
    = NumberLimit !Word
    | ParamLimit !String
    deriving (Show)
