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
    { moduleName :: !String
    , parameters :: [ModuleParameter]
    , moduleBody :: [Statement]
    } deriving (Show)

data ModuleParameter = ModuleParameter
    { paramName  :: !String
    , paramRange :: NaturalSet
    } deriving (Show)

data Statement
    = DeclStmt
        { declStmt :: Declaration }
    | DefStmt 
        { defStmt :: Definition }
    deriving (Show)

data Declaration
    = NodeDecl
        { nodeDecl :: NodeDeclaration }
    | InstanceDecl
        { instDecl :: InstanceDeclaration }
    deriving (Show)

data NodeDeclaration
    = SingleNode
        { nodeKind   :: !NodeKind
        , nodeOrigin :: Domain
        , nodeTarget :: Domain
        , nodeName   :: !String
        , nodeType   :: AddressType
        }
    | ArrayNode
        { nodeKind    :: !NodeKind
        , nodeOrigin  :: Domain
        , nodeTarget  :: Domain
        , nodeName    :: !String
        , nodeType    :: AddressType
        , nodeArrSize :: [NaturalSet]
        }
    deriving (Show)

data NodeKind
    = InputPort
    | OutputPort
    | InternalNode
    deriving (Show)

data InstanceDeclaration
    = SingleInstance
        { instanceName   :: !String
        , instanceModule :: !String
        }
    | ArrayInstance
        { instanceName   :: !String
        , instanceModule :: !String
        , instArrSize    :: [NaturalSet]
        }
    deriving (Show)

data Definition
    = Accepts
    { node   :: UnqualifiedNodeRef
    , accept :: [AddressBlock]
    }
    | Maps
    { node :: UnqualifiedNodeRef
    , maps :: [MapSpec]
    }
    | Converts
    { node     :: UnqualifiedNodeRef
    , converts :: [ConvertSpec]
    }
    | Overlays
    { node     :: UnqualifiedNodeRef
    , overlays :: NodeReference
    }
    | Binds
    { inst  :: InstReference
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
    , originProps :: PropertyExpr
    , targetNode  :: NodeReference
    , mapTarget   :: AddressBlock
    , targetProps :: PropertyExpr
    }
    deriving (Show)

type ConvertSpec = MapSpec

data PortBinding = PortBinding
    { boundPort :: PortReference
    , boundNode :: NodeReference
    }
    deriving (Show)

data UnqualifiedNodeRef
    = SingleNodeRef
        { refName   :: !String }
    | ArrayNodeRef
        { refName   :: !String
        , refRange :: [NaturalSet]
        }
    deriving (Show)

type PortReference = UnqualifiedNodeRef

data NodeReference
    = InternalNodeRef
        { nodeRef :: UnqualifiedNodeRef }
    | InputPortRef
        { nodeRef :: UnqualifiedNodeRef
        , instRef :: InstReference
        }
    deriving (Show)

data InstReference
    = SingleInstRef
        { instanceRef :: !String }
    | ArrayInstRef
        { instanceRef   :: !String
        , instanceRange :: [NaturalSet]
        }
    deriving (Show)

data Domain
    = Memory
    | Interrupt
    | Power
    | Clock
    deriving (Show)

data NamedType = NamedType
    { typeName  :: !String
    , namedType :: AddressType
    } deriving (Show)

data NamedConstant = NamedConstant
    { constName  :: !String
    , namedConst :: !Integer
    }

type AddressType = [NaturalSet]

data AddressDimension
     = SetDimension { addressSet :: NaturalSet }
     | Wildcard
     deriving (Show)

type AddressBlock = [AddressDimension]

data NaturalSet
    = SingletonSet
        { element  :: NaturalExpr }
    | SparseSet
        { elements :: [NaturalExpr] }
    | Range
        { base  :: NaturalExpr
        , limit :: NaturalExpr
        }
    | BitRange
        { base :: NaturalExpr
        , bits :: NaturalExpr
        }
    deriving (Show)

data Natural
    = Literal
        { value :: !Integer }
    | Variable
        { varName :: !String }
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
        { natural  :: Natural
        , bitRange :: NaturalSet
        }
    | ConcatSlice
        { nExprOp1 :: NaturalExpr
        , natural  :: Natural
        , bitRange :: NaturalSet
        }
    | NaturalLeaf
        { natural :: Natural }
    deriving (Show)

newtype Property = Property
    { propName :: String }
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
    | PropertyLeaf
        { property :: Property }
    deriving (Show)
