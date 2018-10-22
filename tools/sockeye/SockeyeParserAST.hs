{-
    SockeyeParserAST.hs: AST for the Sockeye parser

    Part of Sockeye

    Copyright (c) 2018, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

module SockeyeParserAST
    ( module SockeyeParserAST
    , module SockeyeSymbolTable
    , module SockeyeAST
    ) where

import Data.Map (Map)

import SockeyeASTMeta

import SockeyeSymbolTable
    ( NodeType(NodeType)
    , nodeTypeMeta, originDomain, originType, targetDomain, targetType
    , Domain(Memory, Interrupt, Power, Clock)
    , EdgeType(TypeLiteral, TypeName)
    , edgeTypeMeta, typeLiteral, typeRef
    , AddressType(AddressType)
    , ArraySize(ArraySize)
    )

import SockeyeAST
    ( UnqualifiedRef(UnqualifiedRef)
    , refMeta, refName, refIndex
    , NodeReference(InternalNodeRef, InputPortRef)
    , nodeRefMeta, instRef, nodeRef
    , ArrayIndex(ArrayIndex)
    , Address(Address)
    , AddressBlock(AddressBlock)
    , WildcardSet(ExplicitSet, Wildcard)
    , NaturalSet(NaturalSet)
    , NaturalRange(SingletonRange, LimitRange, BitsRange)
    , natRangeMeta, base, limit, bits
    , NaturalExpr(Addition, Subtraction, Multiplication, Slice, Concat, Variable, Literal)
    , natExprMeta, natExprOp1, natExprOp2, bitRange, varName, natural
    , PropertyExpr(And, Or, Not, Property, True, False)
    , propExprMeta, propExprOp1, propExprOp2, property
    )

data Sockeye = Sockeye
    { entryPoint :: FilePath
    , files      :: Map FilePath SockeyeFile
    }
    deriving (Show)

data SockeyeFile = SockeyeFile
    { sockeyeFileMeta :: ASTMeta
    , imports         :: [Import]
    , modules         :: [Module]
    , types           :: [NamedType]
    }
    deriving (Show)

instance MetaAST SockeyeFile where
    meta = sockeyeFileMeta

data Import = Import
    { importMeta  :: ASTMeta
    , importFile  :: !FilePath
    , explImports :: Maybe [ImportAlias]
    }
    deriving (Show)

instance MetaAST Import where
    meta = importMeta

data ImportAlias = ImportAlias
    { importAliasMeta :: ASTMeta
    , originalName    :: !String
    , importAlias     :: !String
    }
    deriving (Show)

instance MetaAST ImportAlias where
    meta = importAliasMeta

data Module = Module
    { moduleMeta  :: ASTMeta
    , moduleName  :: !String
    , parameters  :: [ModuleParameter]
    , constants   :: [NamedConstant]
    , instDecls   :: [InstanceDeclaration]
    , nodeDecls   :: [NodeDeclaration]
    , definitions :: [Definition]
    }
    deriving (Show)

instance MetaAST Module where
    meta = moduleMeta

data ModuleParameter = ModuleParameter
    { paramMeta  :: ASTMeta
    , paramName  :: !String
    , paramRange :: NaturalSet
    }
    deriving (Show)

instance MetaAST ModuleParameter where
    meta = paramMeta

data InstanceDeclaration = InstanceDeclaration
    { instDeclMeta :: ASTMeta
    , instName     :: !String
    , instModName  :: !String
    , instArrSize  :: Maybe ArraySize
    }
    deriving (Show)

instance MetaAST InstanceDeclaration where
    meta = instDeclMeta

data NodeDeclaration = NodeDeclaration
    { nodeDeclMeta :: ASTMeta
    , nodeKind     :: !NodeKind
    , nodeType     :: NodeType
    , nodeName     :: !String
    , nodeArrSize  :: Maybe ArraySize
    }
    deriving (Show)

instance MetaAST NodeDeclaration where
    meta = nodeDeclMeta

data NodeKind
    = InputPort
    | OutputPort
    | InternalNode
    deriving (Eq, Show)

data Definition
    = Accepts
        { defMeta :: ASTMeta
        , node    :: UnqualifiedRef
        , accepts :: [AddressBlock]
        }
    | Maps
        { defMeta :: ASTMeta
        , node    :: UnqualifiedRef
        , maps    :: [MapSpec]
        }
    | Converts
        { defMeta  :: ASTMeta
        , node     :: UnqualifiedRef
        , converts :: [ConvertSpec]
        }
    | Overlays
        { defMeta  :: ASTMeta
        , node     :: UnqualifiedRef
        , overlays :: NodeReference
        }
    | BlockOverlays
        { defMeta  :: ASTMeta
        , node     :: UnqualifiedRef
        , overlays :: NodeReference
        , blocksizes :: [Integer]
        }
    | Instantiates
        { defMeta    :: ASTMeta
        , inst       :: UnqualifiedRef
        , instModule :: !String
        , arguments  :: [NaturalExpr]
        }
    | Binds
        { defMeta :: ASTMeta
        , inst    :: UnqualifiedRef
        , binds   :: [PortBinding]
        }
    | Forall
        { defMeta        :: ASTMeta
        , boundVarName   :: !String
        , varRange       :: NaturalSet
        , quantifierBody :: [Definition]
        }
    deriving (Show)

instance MetaAST Definition where
    meta = defMeta

data MapSpec = MapSpec
    { mapSpecMeta :: ASTMeta
    , mapAddr     :: AddressBlock
    , mapTargets  :: [MapTarget]
    }
    deriving (Show)

instance MetaAST MapSpec where
    meta = mapSpecMeta

data MapTarget = MapTarget
    { mapTargetMeta :: ASTMeta
    , targetNode    :: NodeReference
    , targetAddr    :: AddressBlock
    }
    deriving (Show)

instance MetaAST MapTarget where
    meta = mapTargetMeta

type ConvertSpec = MapSpec

data PortBinding = PortBinding
    { portBindMeta :: ASTMeta
    , boundPort    :: UnqualifiedRef
    , boundNode    :: NodeReference
    }
    deriving (Show)

instance MetaAST PortBinding where
    meta = portBindMeta

data NamedType = NamedType
    { namedTypeMeta :: ASTMeta
    , typeName      :: !String
    , namedType     :: AddressType
    }
    deriving (Show)

instance MetaAST NamedType where
    meta = namedTypeMeta

data NamedConstant = NamedConstant
    { namedConstMeta :: ASTMeta
    , constName      :: !String
    , namedConst     :: !Integer
    }
    deriving (Show)

instance MetaAST NamedConstant where
    meta = namedConstMeta
