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

type NetSpec = Map NodeId NodeSpec

data NodeId = NodeId
    { name      :: !String
    , namespace :: [String]
    } deriving (Eq, Ord)

instance Show NodeId where
    show (NodeId n ns) =
        let noEmpty = filter ((> 0) . length) ns
        in intercalate "." $ reverse (n:noEmpty)

data NodeSpec
    = NodeSpec
        { nodeType  :: NodeType
        , accept    :: [BlockSpec]
        , translate :: [MapSpec]
        }
    deriving (Show)

data NodeType
    = Core
    | Device
    | Memory
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

type Address = Integer
