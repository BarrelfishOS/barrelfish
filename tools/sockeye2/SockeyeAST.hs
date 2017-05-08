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

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric (showHex)

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
                   } deriving (Ord, Eq)

{-
A name is an address block qualified by a node ID
-}
data Name = Name { nodeId     :: NodeId
                 , block  :: Block
                 }

{-
A node can accept a set of addresses and translate a
(not necessarily disjoint) set of addresses
-}
data Node = Node { accept    :: [Block]
                 , translate :: [(Block, Name)]
                 }

{-
A (decoding) net is a Map from Node IDs to nodes
-}
newtype Net = Net { getAssignment :: Map NodeId Node }

{- Pretty Printing -}
instance Show Block where
    show block = "0x" ++ showHex (base block) "" ++ "-" ++ "0x" ++ showHex (limit block) ""

instance Show Name where
    show name = nodeId name ++ " at " ++ show (block name)

instance Show Node where
    show node = acceptStr node ++ " " ++ translateStr node
        where acceptStr node = "accept [" ++ intercalate ", " (map show (accept node)) ++ "]"
              translateStr node = "translate [" ++ intercalate ", " (map translationStr (translate node)) ++ "]"
                where translationStr (fromBlock, name) = show fromBlock ++ " to " ++ show name

instance Show Net where
    show net = intercalate "\n" (map nodeStr (Map.toList $ getAssignment net))
        where nodeStr (id, node) = id ++ " is " ++ show node