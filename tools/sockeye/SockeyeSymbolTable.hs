{-
    SockeyeSymbolTable.hs: Symbol Table for Sockeye

    Part of Sockeye

    Copyright (c) 2017, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

module SockeyeSymbolTable where

import Data.Set (Set)
import Data.Map (Map)

import SockeyeASTMeta

data Sockeye = Sockeye
    { sockeyeMeta :: ASTMeta 
    , modules     :: Map String Module
    , types       :: Map String NamedType
    }
    deriving (Show)

instance MetaAST Sockeye where
    meta = sockeyeMeta

data Module = Module
    { moduleMeta     :: ASTMeta
    , parameters     :: Map String ModuleParameter
    , parameterOrder :: [String]
    , constants      :: Map String NamedConstant
    , inputPorts     :: Set String
    , outputPorts    :: Map String OutputPort
    , instances      :: Map String Instance
    , nodes          :: Map String Node
    }
    deriving (Show)

instance MetaAST Module where
    meta = moduleMeta

data ModuleParameter = ModuleParameter
    { paramMeta  :: ASTMeta
    , paramRange :: NaturalSet
    }
    deriving (Show)

instance MetaAST ModuleParameter where
    meta = paramMeta

data OutputPort = OutputPort
    { portMeta    :: ASTMeta
    , portType    :: NodeType
    , portArrSize :: Maybe ArraySize
    }
    deriving (Show)

instance MetaAST OutputPort where
    meta = portMeta

data Instance
    = SingleInstance
        { instMeta       :: ASTMeta
        , instanceModule :: !String
        , instArrSize    :: Maybe ArraySize
        }
    deriving (Show)

instance MetaAST Instance where
    meta = instMeta

data Node
    = SingleNode
        { nodeMeta    :: ASTMeta
        , nodeType    :: NodeType
        , nodeArrSize :: Maybe ArraySize
        }
    deriving (Show)

instance MetaAST Node where
    meta = nodeMeta

data NodeType = NodeType
    { nodeTypeMeta :: ASTMeta
    , originDomain :: !Domain
    , originType   :: EdgeType
    , targetDomain :: !Domain
    , targetType   :: Maybe EdgeType
    }
    deriving (Show)

instance MetaAST NodeType where
    meta = nodeTypeMeta

data Domain
    = Memory
    | Interrupt
    | Power
    | Clock
    deriving (Show)

data EdgeType = EdgeType
    { edgeTypeMeta :: ASTMeta
    , typeLiteral  :: AddressType
    }
    deriving (Show)

instance MetaAST EdgeType where
    meta = edgeTypeMeta

data NamedType = NamedType
    { namedTypeMeta :: ASTMeta
    , namedType     :: AddressType
    }
    deriving (Show)

instance MetaAST NamedType where
    meta = namedTypeMeta

data NamedConstant = NamedConstant
    { namedConstMeta :: ASTMeta
    , namedConst     :: !Integer
    }
    deriving (Show)

instance MetaAST NamedConstant where
    meta = namedConstMeta

data AddressType = AddressType ASTMeta [NaturalSet]
    deriving (Show)

instance MetaAST AddressType where
    meta (AddressType m _) = m

data ArraySize = ArraySize ASTMeta [NaturalSet]
    deriving (Show)

instance MetaAST ArraySize where
    meta (ArraySize m _) = m

data NaturalSet = NaturalSet ASTMeta [NaturalRange]
    deriving (Show)

instance MetaAST NaturalSet where
    meta (NaturalSet m _) = m

data NaturalRange
    = SingletonRange
        { natRangeMeta :: ASTMeta
        , base         :: NaturalExpr
        }
    | LimitRange
        { natRangeMeta :: ASTMeta
        , base         :: NaturalExpr
        , limit        :: NaturalExpr
        }
    | BitsRange
        { natRangeMeta :: ASTMeta
        , base         :: NaturalExpr
        , bits         :: NaturalExpr
        }
    deriving (Show)

instance MetaAST NaturalRange where
    meta = natRangeMeta

data NaturalExpr
    = Addition
        { natExprMeta :: ASTMeta
        , natExprOp1  :: NaturalExpr
        , natExprOp2  :: NaturalExpr
        }
    | Subtraction
        { natExprMeta :: ASTMeta
        , natExprOp1  :: NaturalExpr
        , natExprOp2  :: NaturalExpr
        }
    | Multiplication
        { natExprMeta :: ASTMeta
        , natExprOp1  :: NaturalExpr
        , natExprOp2  :: NaturalExpr
        }
    | Slice
        { natExprMeta :: ASTMeta
        , natExprOp1  :: NaturalExpr
        , bitRange    :: NaturalSet
        }
    | Concat
        { natExprMeta :: ASTMeta
        , natExprOp1  :: NaturalExpr
        , natExprOp2  :: NaturalExpr
        }
    | Variable
        { natExprMeta :: ASTMeta
        , varName     :: !String
        }
    | Literal
        { natExprMeta :: ASTMeta
        , natural     :: !Integer
        }
    deriving (Show)

instance MetaAST NaturalExpr where
    meta = natExprMeta
