{-
    SockeyeAST.hs: AST for Sockeye

    Part of Sockeye

    Copyright (c) 2018, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

module SockeyeAST where

import SockeyeASTMeta

{- TODO: This is just a dummy -}
data Sockeye = Sockeye
    deriving (Show)

{-
 - From here the data structures are shared with the Parse AST
 - If they should be different, either change the parser to directly parse them differently
 - or move them to the ParserAST
 -}
data UnqualifiedRef = UnqualifiedRef
    { refMeta  :: ASTMeta
    , refName  :: String
    , refIndex :: Maybe ArrayIndex
    }
    deriving (Show)

instance MetaAST UnqualifiedRef where
    meta = refMeta

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

data ArrayIndex = ArrayIndex ASTMeta [WildcardSet]
    deriving (Show)

instance MetaAST ArrayIndex where
    meta (ArrayIndex m _) = m

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

data WildcardSet
    = ExplicitSet ASTMeta NaturalSet
    | Wildcard ASTMeta
    deriving (Show)

instance MetaAST WildcardSet where
    meta (ExplicitSet m _) = m
    meta (Wildcard m) = m

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
        , bitRange    :: NaturalRange
        }
    | Concat
        { natExprMeta :: ASTMeta
        , natExprOp1  :: NaturalExpr
        , natExprOp2  :: NaturalExpr
        }
    | Parameter
        { natExprMeta :: ASTMeta
        , varName     :: !String
        }
    | Constant
        { natExprMeta :: ASTMeta
        , varName     :: !String
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
        , propExprOp1     :: PropertyExpr
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