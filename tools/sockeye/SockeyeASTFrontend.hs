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
    { modules   :: [Module]
    , nodeDecls :: [NodeDecl]
    }

data ParamType = Index | Address

data ModuleParam = ModuleParam
    { paramName :: !String
    , paramType :: !Maybe ParamType
    }

data Module = Module
    { inputPorts  :: [NodeId]
    , outputPorts :: [NodeId]
    , parameters  :: [ModuleParam]
    , body        :: [NodeDecl]
    }

data NodeDecl = NodeDecl
    { nodeId   :: NodeId
    , nodeSpec :: NodeSpec
    }

data NodeIdIndex = Index
    { index :: !Word }
                 | Param
    { name :: !String }

data NodeId = Single
    { prefix :: String }
            | Indexed
    { prefix :: !String }            
            | Multi
    { prefix :: !String
    , start  :: NodeIdIndex
    , end    :: NodeIdIndex
    }

data NodeType = Memory | Device

data NodeSpec = NodeSpec
    { nodeType  :: !Maybe NodeType
    , accept    :: [BlockSpec]
    , translate :: [MapSpec]
    , overlay   :: !String
    }

data Address = Address
    { address :: Word }
             | Param
    { name :: String }

data BlockSpec = Singleton
    { address :: !Address }
               | Range
    { base  :: !Address
    , limit :: !Address
    }
               | Length
    { base :: !Address
    , bits :: !Word
    }

data MapDest = Direct
    { destNode :: !NodeId }
             | BaseAddress
    { destNode :: !NodeId
    , destBase :: !Address
    }

data MapSpec = MapSpec
    { block :: BlockSpec
    , dest  :: [MapDest]
    }


