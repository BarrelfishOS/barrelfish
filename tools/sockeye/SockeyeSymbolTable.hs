{-
    SockeyeSymbolTable.hs: Symbol Table for Sockeye

    Part of Sockeye

    Copyright (c) 2017, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

module SockeyeSymbolTable
    ( module SockeyeSymbolTable
    , module SockeyeAST
    ) where

import Data.Set (Set)
import Data.Map (Map)

import SockeyeASTMeta

import SockeyeAST
    ( NaturalSet(NaturalSet)
    , NaturalRange(SingletonRange, LimitRange, BitsRange)
    , natRangeMeta, base, limit, bits
    , NaturalExpr(Addition, Subtraction, Multiplication, Slice, Concat, Variable, Literal)
    , natExprMeta, natExprOp1, natExprOp2, bitRange, varName, natural
    )

data Sockeye = Sockeye
    { entryPoint :: FilePath
    , files      :: Map FilePath SockeyeFile
    }
    deriving (Show)

data SockeyeFile = SockeyeFile
    { sockeyeFileMeta :: ASTMeta 
    , modules         :: Map String Module
    , types           :: Map String NamedType
    }
    deriving (Show)

instance MetaAST SockeyeFile where
    meta = sockeyeFileMeta

data Module
    = Module
        { moduleMeta     :: ASTMeta
        , parameters     :: Map String ModuleParameter
        , parameterOrder :: [String]
        , constants      :: Map String NamedConstant
        , inputPorts     :: Set String
        , outputPorts    :: Map String Node
        , instances      :: Map String Instance
        , nodes          :: Map String Node
        }
    | ImportedModule
        { moduleMeta  :: ASTMeta
        , moduleFile  :: !FilePath
        , origModName :: !String
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

data Instance = Instance
    { instMeta    :: ASTMeta
    , instModule  :: !String
    , instArrSize :: Maybe ArraySize
    }
    deriving (Show)

instance MetaAST Instance where
    meta = instMeta

data Node = Node
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
    deriving (Eq, Show)

data EdgeType
    = TypeLiteral
        { edgeTypeMeta :: ASTMeta
        , typeLiteral  :: AddressType
        }
    | TypeName
        { edgeTypeMeta :: ASTMeta
        , typeRef      :: !String
        }
    deriving (Show)

instance MetaAST EdgeType where
    meta = edgeTypeMeta

data NamedType
    = NamedType
        { namedTypeMeta :: ASTMeta
        , namedType     :: AddressType
        }
    | ImportedType
        { namedTypeMeta :: ASTMeta
        , typeFile      :: !FilePath
        , origTypeName  :: !String
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

data ArraySize = ArraySize ASTMeta [NaturalSet]
    deriving (Show)

instance MetaAST ArraySize where
    meta (ArraySize m _) = m

data AddressType = AddressType ASTMeta [NaturalSet]
    deriving (Show)

instance MetaAST AddressType where
    meta (AddressType m _) = m
