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

{-
Nodes are identfied by strings
-}
newtype NodeId = NodeId String
  deriving (Eq, Ord, Show)

{-
Addresses are natural numbers
-}
newtype Addr = Addr Word
  deriving (Eq, Ord, Show)

{-
A block is a contigous set of addresses
-}
data BlockSpec = BlockSpec
    { base  :: Addr
    , limit :: Addr
    } deriving (Eq, Ord, Show)

{-
A mapping of a source address block to a destination node
at a base address
-}
data MapSpec = MapSpec
    { srcBlock :: BlockSpec
    , destNode :: NodeId
    , destBase :: Addr
    } deriving (Show)

{-
Node can either be memory, device or other
-}
data NodeType
    = Memory
    | Device
    | Other

instance Show NodeType where
    show Memory = "memory"
    show Device = "device"
    show Other  = "other"

{-
A node is specified as a list of blocks it accepts,
a list of mappings and possibly an overlay on another block
-}
data NodeSpec = NodeSpec
    { nodeType  :: NodeType
    , accept    :: [BlockSpec]
    , translate :: [MapSpec]
    , overlay   :: Maybe NodeId
    } deriving (Show)

{-
A decoding net is specified as a list 
of Node IDs mapped to Nodes
-}
newtype NetSpec = NetSpec [(NodeId, NodeSpec)]
    deriving (Show)
