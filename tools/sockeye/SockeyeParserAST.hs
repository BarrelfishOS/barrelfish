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

module SockeyeParserAST where

import SockeyeASTMeta

data Sockeye = Sockeye
    { sockeyeMeta :: ASTMeta 
    , modules     :: [Module]
    , types       :: [NamedType]
    }
    deriving (Show)

instance MetaAST Sockeye where
    meta = sockeyeMeta

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
    { instDeclMeta   :: ASTMeta
    , instanceName   :: !String
    , instanceModule :: !String
    , instArrSize    :: Maybe ArraySize
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
    deriving (Show) 

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
        , varRange       :: [NaturalSet]
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
    , boundPort    :: PortReference
    , boundNode    :: NodeReference
    }
    deriving (Show)

instance MetaAST PortBinding where
    meta = portBindMeta

data UnqualifiedRef
    = SingleRef
        { refMeta :: ASTMeta
        , refName :: String
        }
    | ArrayRef
        { refMeta  :: ASTMeta
        , refName  :: String
        , refIndex :: ArrayIndex
        }
    deriving (Show)

instance MetaAST UnqualifiedRef where
    meta = refMeta

type PortReference = UnqualifiedRef

data NodeReference
    = InternalNodeRef
        { nodeRefMeta :: ASTMeta
        , nodeRef     :: UnqualifiedRef
        }
    | InputPortRef
        { nodeRefMeta :: ASTMeta
        , instRef     :: UnqualifiedRef
        , nodeRef     :: UnqualifiedRef
        }
    deriving (Show)

instance MetaAST NodeReference where
    meta = nodeRefMeta

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

data AddressType = AddressType ASTMeta [NaturalSet]
    deriving (Show)

instance MetaAST AddressType where
    meta (AddressType m _) = m

data Address = Address ASTMeta [WildcardSet]
    deriving (Show)

instance MetaAST Address where
    meta (Address m _) = m

data AddressBlock = AddressBlock
    { addrBlockMeta :: ASTMeta
    , addresses     :: Address
    , properties    :: PropertyExpr
    }
    deriving (Show)

instance MetaAST AddressBlock where
    meta = addrBlockMeta

data ArraySize = ArraySize ASTMeta [NaturalSet]
    deriving (Show)

instance MetaAST ArraySize where
    meta (ArraySize m _) = m

data ArrayIndex = ArrayIndex ASTMeta [WildcardSet]
    deriving (Show)

instance MetaAST ArrayIndex where
    meta (ArrayIndex m _) = m

data NaturalSet = NaturalSet ASTMeta [NaturalRange]
    deriving (Show)

instance MetaAST NaturalSet where
    meta (NaturalSet m _) = m

data WildcardSet
    = ExplicitSet ASTMeta NaturalSet
    | Wildcard ASTMeta
    deriving (Show)

instance MetaAST WildcardSet where
    meta (ExplicitSet m _) = m
    meta (Wildcard m) = m

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

data PropertyExpr
    = And
        { propExprMeta :: ASTMeta
        , pExprOp1     :: PropertyExpr
        , propExprOp2  :: PropertyExpr
        }
    | Or
        { propExprMeta :: ASTMeta
        , propExprOp1  :: PropertyExpr
        , propExprOp2  :: PropertyExpr
        }
    | Not
        { propExprMeta :: ASTMeta
        , propExprOp1  :: PropertyExpr
        }
    | Property
        { propExprMeta :: ASTMeta
        , property     :: !String
        }
    | True
    | False
    deriving (Show)

instance MetaAST PropertyExpr where
    meta = propExprMeta
