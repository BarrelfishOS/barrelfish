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

data Sockeye = Sockeye
    { modules :: [Module]
    , types   :: [NamedType]
    }
    deriving (Show)

data Module = Module
    { moduleName  :: !String
    , parameters  :: [ModuleParameter]
    , constDecls  :: [NamedConstant]
    , instDecls   :: [InstanceDeclaration]
    , nodeDecls   :: [NodeDeclaration]
    , definitions :: [Definition]
    } deriving (Show)

data ModuleParameter = ModuleParameter
    { paramName  :: !String
    , paramRange :: NaturalSet
    } deriving (Show)

data InstanceDeclaration
    = SingleInstance
        { instanceName   :: !String
        , instanceModule :: !String
        }
    | ArrayInstance
        { instanceName   :: !String
        , instArrSize    :: ArraySize
        , instanceModule :: !String
        }
    deriving (Show)

data NodeDeclaration
    = SingleNode
        { nodeKind     :: !NodeKind
        , originDomain :: !Domain
        , originType   :: NodeType
        , targetDomain :: !Domain
        , targetType   :: Maybe NodeType
        , nodeName     :: !String
        }
    | ArrayNode
        { nodeKind     :: !NodeKind
        , originDomain :: !Domain
        , originType   :: NodeType
        , targetDomain :: !Domain
        , targetType   :: Maybe NodeType
        , nodeName     :: !String
        , nodeArrSize  :: ArraySize
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

data NodeType
    = TypeLiteral
        { typeLiteral :: [NaturalSet] }
    | TypeName
        { typeRef :: !String }
    deriving (Show)

data Definition
    = Accepts
        { node    :: UnqualifiedRef
        , accepts :: [AddressBlock]
        }
    | Maps
        { node :: UnqualifiedRef
        , maps :: [MapSpec]
        }
    | Converts
        { node     :: UnqualifiedRef
        , converts :: [ConvertSpec]
        }
    | Overlays
        { node     :: UnqualifiedRef
        , overlays :: NodeReference
        }
    | Instantiates
        { inst       :: UnqualifiedRef
        , instModule :: !String
        , arguments  :: [NaturalExpr]
        }
    | Binds
        { inst  :: UnqualifiedRef
        , binds :: [PortBinding]
        }
    | Forall
        { boundVarName   :: !String
        , varRange       :: [NaturalSet]
        , quantifierBody :: [Definition]
        }
    deriving (Show)

data MapSpec = MapSpec
    { mapOrigin   :: AddressBlock
    , mapTargets  :: [MapTarget]
    }
    deriving (Show)

data MapTarget = MapTarget
    { targetNode :: NodeReference
    , targetAddr :: AddressBlock
    }
    deriving (Show)

type ConvertSpec = MapSpec

data PortBinding = PortBinding
    { boundPort :: PortReference
    , boundNode :: NodeReference
    }
    deriving (Show)

data UnqualifiedRef
    = SingleRef
        { refName :: !String }
    | ArrayRef
        { refName  :: !String
        , refRange :: ArrayRange
        }
    deriving (Show)

type PortReference = UnqualifiedRef

data NodeReference
    = InternalNodeRef
        { nodeRef :: UnqualifiedRef }
    | InputPortRef
        { nodeRef :: UnqualifiedRef
        , instRef :: UnqualifiedRef
        }
    deriving (Show)

data NamedType = NamedType
    { typeName  :: !String
    , namedType :: [NaturalSet]
    }
    deriving (Show)

data NamedConstant = NamedConstant
    { constName  :: !String
    , namedConst :: !Integer
    }
    deriving (Show)

data AddressBlock = AddressBlock
    { addresses  :: [WildcardSet]
    , properties :: PropertyExpr
    }
    deriving (Show)

type ArraySize = [NaturalSet]
type ArrayRange = [WildcardSet]

type NaturalSet = [NaturalRange]

data NaturalRange
    = SingletonRange
        { base :: NaturalExpr}
    | LimitRange
        { base  :: NaturalExpr
        , limit :: NaturalExpr
        }
    | BitsRange
        { base :: NaturalExpr
        , bits :: NaturalExpr
        }
    deriving (Show)

data WildcardSet
    = ExplicitSet { set :: NaturalSet }
    | Wildcard
    deriving (Show)

data NaturalExpr
    = Addition
        { nExprOp1 :: NaturalExpr
        , nExprOp2 :: NaturalExpr
        }
    | Subtraction
        { nExprOp1 :: NaturalExpr
        , nExprOp2 :: NaturalExpr
        }
    | Multiplication
        { nExprOp1 :: NaturalExpr
        , nExprOp2 :: NaturalExpr
        }
    | Slice
        { nExprOp1 :: NaturalExpr
        , bitRange :: NaturalSet
        }
    | Concat
        { nExprOp1 :: NaturalExpr
        , nExprOp2 :: NaturalExpr
        }
    | Variable
        { varName :: !String }
    | Literal
        { natural :: !Integer }
    deriving (Show)

data PropertyExpr
    = And
        { pExprOp1 :: PropertyExpr
        , pExprOp2 :: PropertyExpr
        }
    | Or
        { pExprOp1 :: PropertyExpr
        , pExprOp2 :: PropertyExpr
        }
    | Not
        { pExprOp1 :: PropertyExpr }
    | Property
        { property :: !String }
    | True
    | False
    deriving (Show)
