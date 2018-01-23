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

module SockeyeASTParser where

import Text.Parsec.Pos (SourcePos)

type ParserMeta = SourcePos

data Sockeye m = Sockeye
    { sockeyeMeta :: m 
    , modules     :: [Module m]
    , types       :: [NamedType m]
    }
    deriving (Show)

data Module m = Module
    { moduleMeta  :: m
    , moduleName  :: !String
    , parameters  :: [ModuleParameter m]
    , constants   :: [NamedConstant m]
    , instDecls   :: [InstanceDeclaration m]
    , nodeDecls   :: [NodeDeclaration m]
    , definitions :: [Definition m]
    } deriving (Show)

data ModuleParameter m = ModuleParameter
    { paramMeta  :: m
    , paramName  :: !String
    , paramRange :: NaturalSet m
    } deriving (Show)

data InstanceDeclaration m
    = SingleInstance
        { instDeclMeta   :: m
        , instanceName   :: !String
        , instanceModule :: !String
        }
    | ArrayInstance
        { instDeclMeta   :: m
        , instanceName   :: !String
        , instArrSize    :: ArraySize m
        , instanceModule :: !String
        }
    deriving (Show)

data NodeDeclaration m
    = SingleNode
        { nodeDeclMeta :: m
        , nodeKind     :: !NodeKind
        , originDomain :: !Domain
        , originType   :: NodeType m
        , targetDomain :: !Domain
        , targetType   :: Maybe (NodeType m)
        , nodeName     :: !String
        }
    | ArrayNode
        { nodeDeclMeta :: m
        , nodeKind     :: !NodeKind
        , originDomain :: !Domain
        , originType   :: NodeType m
        , targetDomain :: !Domain
        , targetType   :: Maybe (NodeType m)
        , nodeName     :: !String
        , nodeArrSize  :: ArraySize m
        }
    deriving (Show)

data NodeKind
    = InputPort
    | OutputPort
    | InternalNode
    deriving (Show)

data Domain
    = Memory
    | Interrupt
    | Power
    | Clock
    deriving (Show)

data NodeType m
    = TypeLiteral
        { nodeTypeMeta :: m
        , typeLiteral  :: AddressType m
        }
    | TypeName
        { nodeTypeMeta :: m
        , typeRef :: !String
        }
    deriving (Show)

data Definition m
    = Accepts
        { defMeta :: m
        , node    :: UnqualifiedRef m
        , accepts :: [AddressBlock m]
        }
    | Maps
        { defMeta :: m
        , node    :: UnqualifiedRef m
        , maps    :: [MapSpec m]
        }
    | Converts
        { defMeta  :: m
        , node     :: UnqualifiedRef m
        , converts :: [ConvertSpec m]
        }
    | Overlays
        { defMeta  :: m
        , node     :: UnqualifiedRef m
        , overlays :: NodeReference m
        }
    | Instantiates
        { defMeta    :: m
        , inst       :: UnqualifiedRef m
        , instModule :: !String
        , arguments  :: [NaturalExpr m]
        }
    | Binds
        { defMeta :: m
        , inst    :: UnqualifiedRef m
        , binds   :: [PortBinding m]
        }
    | Forall
        { defMeta        :: m
        , boundVarName   :: !String
        , varRange       :: [NaturalSet m]
        , quantifierBody :: [Definition m]
        }
    deriving (Show)

data MapSpec m = MapSpec
    { mapSpecMeta :: m
    , mapAddr     :: AddressBlock m
    , mapTargets  :: [MapTarget m]
    }
    deriving (Show)

data MapTarget m = MapTarget
    { mapTargetMeta :: m
    , targetNode    :: NodeReference m
    , targetAddr    :: AddressBlock m
    }
    deriving (Show)

type ConvertSpec = MapSpec

data PortBinding m = PortBinding
    { portBindMeta :: m
    , boundPort    :: PortReference m
    , boundNode    :: NodeReference m
    }
    deriving (Show)

data UnqualifiedRef m
    = SingleRef
        { refMeta :: m
        , refName :: String
        }
    | ArrayRef
        { refMeta  :: m
        , refName  :: String
        , refIndex :: ArrayIndex m
        }
    deriving (Show)

type PortReference = UnqualifiedRef

data NodeReference m
    = InternalNodeRef
        { nodeRefMeta :: m
        , nodeRef     :: UnqualifiedRef m
        }
    | InputPortRef
        { nodeRefMeta :: m
        , instRef     :: UnqualifiedRef m
        , nodeRef     :: UnqualifiedRef m
        }
    deriving (Show)

data NamedType m = NamedType
    { namedTypeMeta :: m
    , typeName      :: !String
    , namedType     :: AddressType m
    }
    deriving (Show)

data NamedConstant m = NamedConstant
    { namedConstMeta :: m
    , constName      :: !String
    , namedConst     :: !Integer
    }
    deriving (Show)

data AddressType m = AddressType m [NaturalSet m]
    deriving (Show)

data Address m = Address m [WildcardSet m]
    deriving (Show)

data AddressBlock m = AddressBlock
    { addrBlockMeta :: m
    , addresses     :: Address m
    , properties    :: PropertyExpr m
    }
    deriving (Show)

data ArraySize m = ArraySize m [NaturalSet m]
    deriving (Show)

data ArrayIndex m = ArrayIndex m [WildcardSet m]
    deriving (Show)

data NaturalSet m = NaturalSet m [NaturalRange m]
    deriving (Show)

data WildcardSet m
    = ExplicitSet m (NaturalSet m)
    | Wildcard
    deriving (Show)

data NaturalRange m
    = SingletonRange
        { natRangeMeta :: m
        , base         :: NaturalExpr m
        }
    | LimitRange
        { natRangeMeta :: m
        , base         :: NaturalExpr m
        , limit        :: NaturalExpr m
        }
    | BitsRange
        { natRangeMeta :: m
        , base         :: NaturalExpr m
        , bits         :: NaturalExpr m
        }
    deriving (Show)

data NaturalExpr m
    = Addition
        { natExprMeta :: m
        , natExprOp1  :: NaturalExpr m
        , natExprOp2  :: NaturalExpr m
        }
    | Subtraction
        { natExprMeta :: m
        , natExprOp1  :: NaturalExpr m
        , natExprOp2  :: NaturalExpr m
        }
    | Multiplication
        { natExprMeta :: m
        , natExprOp1  :: NaturalExpr m
        , natExprOp2  :: NaturalExpr m
        }
    | Slice
        { natExprMeta :: m
        , natExprOp1  :: NaturalExpr m
        , bitRange    :: NaturalSet m
        }
    | Concat
        { natExprMeta :: m
        , natExprOp1  :: NaturalExpr m
        , natExprOp2  :: NaturalExpr m
        }
    | Variable
        { natExprMeta :: m
        , varName     :: !String
        }
    | Literal
        { natExprMeta :: m
        , natural     :: !Integer
        }
    deriving (Show)

data PropertyExpr m
    = And
        { propExprMeta :: m
        , pExprOp1     :: PropertyExpr m
        , propExprOp2  :: PropertyExpr m
        }
    | Or
        { propExprMeta :: m
        , propExprOp1  :: PropertyExpr m
        , propExprOp2  :: PropertyExpr m
        }
    | Not
        { propExprMeta :: m
        , propExprOp1  :: PropertyExpr m
        }
    | Property
        { propExprMeta :: m
        , property     :: !String
        }
    | True
    | False
    deriving (Show)
