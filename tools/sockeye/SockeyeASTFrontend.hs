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

module SockeyeASTFrontend where

data SockeyeSpec = SockeyeSpec
    { modules :: [Module]
    , net     :: [NetSpec]
    }

data ParamType = IndexParam | AddressParam

data ModuleParam = ModuleParam
    { paramName :: !String
    , paramType :: Maybe ParamType
    }

data Module = Module
    { inputPorts  :: [Identifier]
    , outputPorts :: [Identifier]
    , parameters  :: [ModuleParam]
    , body        :: [NetSpec]
    }

data ModuleParamMap
    = ModuleParamMap
        { port   :: Identifier
        , nodeId :: Identifier
        }

data ModuleInstantiation
    = ModuleInstantiation
        { nameSpace      :: Identifier
        , arguments      :: [Word]
        , inputMappings  :: [ModuleParamMap]
        , outputMappings :: [ModuleParamMap]
        }

data NetSpec
    = NodeDeclSpec NodeDecl
    | ModuleInstSpec ModuleInstantiation



data NodeDecl = NodeDecl
    { nodeIds  :: [Identifier]
    , nodeSpec :: NodeSpec
    }

data IdentifierIndex 
    = NumberIndex !Word
    | ParamIndex !String

data Identifier
    = Single
        { prefix :: !String }
    | Indexed
        { prefix :: !String }            
    | Multi
        { prefix :: !String
        , start  :: IdentifierIndex
        , end    :: IdentifierIndex
        }

data NodeType = Memory | Device

data NodeSpec = NodeSpec
    { nodeType  :: Maybe NodeType
    , accept    :: [BlockSpec]
    , translate :: [MapSpec]
    , overlay   :: Identifier
    }

data Address = Address !Word
             | Param !String

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

data MapDest
    = Direct
        { destNode :: Identifier }
    | BaseAddress
        { destNode :: Identifier
        , destBase :: Address
        }

data MapSpec = MapSpec
    { block :: BlockSpec
    , dest  :: [MapDest]
    }


