{-
  SockeyeAST.hs: AST for Sockeye

  Part of Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeAST where

import Data.Set (Set)
import Data.Map (Map)

{-
Nodes are identfied by strings
-}
type NodeId = String
{-
Addresses are natural numbers
-}
type Addr = Word

{-
A contigous block of addresses
-}
data Block = Block { base  :: Addr
                   , limit :: Addr
                   }

{-
A name is an address block qualified by a node ID
-}
type Name = (NodeId, Block)

{-
A node can accept a set of addresses and translate a
(not necessarily disjoint) set of addresses
-}
data Node = Node { accept    :: Set Block
                 , translate :: Set (Block, Name)
                 }

{-
A (decoding) net is a Map from Node IDs to nodes
-}
type Net = Map NodeId Node