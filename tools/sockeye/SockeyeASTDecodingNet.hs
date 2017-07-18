{-
  SockeyeASTDecodingNet.hs: Decoding net AST for Sockeye

  Part of Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeASTDecodingNet where

import Data.List (intercalate)
import Data.Map (Map)

newtype NetSpec =
    NetSpec
        { net :: Map NodeId NodeSpec }
    deriving (Show)

data NodeId = NodeId
    { namespace :: Namespace
    , name      :: !String
    } deriving (Eq, Ord)

instance Show NodeId where
    show (NodeId namespace name) = 
        case ns namespace of
            [] -> name
            _  -> concat [show namespace, ".", name]

newtype Namespace = Namespace
    { ns :: [String] }
    deriving (Eq, Ord)

instance Show Namespace where
    show (Namespace ns) = intercalate "." $ reverse ns

data NodeSpec
    = NodeSpec
        { nodeType  :: NodeType
        , accept    :: [BlockSpec]
        , translate :: [MapSpec]
        }
    deriving (Show)

data NodeType
    = Memory
    | Device
    | Other
    deriving (Show)

data BlockSpec = BlockSpec
    { base  :: Address
    , limit :: Address
    } deriving (Show)

data MapSpec = MapSpec
    { srcBlock :: BlockSpec
    , destNode :: NodeId
    , destBase :: Address
    } deriving (Show)

newtype Address =
    Address
        { address :: Word }
    deriving (Show)
