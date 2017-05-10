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
A block is a contigous set of addresses
-}
data BlockSpec = BlockSpec { base  :: Addr
                           , limit :: Addr
                           } deriving (Ord, Eq)

{-
A mapping of a source address block to a destination node
at a base address
-}
data MapSpec = MapSpec { srcBlock :: BlockSpec
                       , destNode :: NodeId
                       , destBase :: Addr
                       }

{-
A node is specified as a list of blocks it accepts,
a list of mappings and possibly an overlay on another block
-}
data NodeSpec = NodeSpec { accept    :: [BlockSpec]
                         , translate :: [MapSpec]
                         , overlay   :: Maybe NodeId
                         }

{-
A decoding net is specified as a list 
of Node IDs mapped to Nodes
-}
newtype NetSpec = NetSpec { getNodes :: [(NodeId, NodeSpec)] }

{- Pretty Printing -}
instance Show BlockSpec where
    show blockSpec = "0x" ++ showHex (base blockSpec) "" ++ "-" ++ "0x" ++ showHex (limit blockSpec) ""

instance Show MapSpec where
    show mapSpec = let srcStr  = show $ srcBlock mapSpec
                       nodeStr = destNode mapSpec
                       baseStr = "0x" ++ showHex (destBase mapSpec) ""
                   in srcStr ++ " to " ++ nodeStr ++ " at " ++ baseStr

instance Show NodeSpec where
    show nodeSpec = let acceptStr    = "accept [" ++ intercalate ", " (map show (accept nodeSpec)) ++ "]"
                        translateStr = "map [" ++ intercalate ", " (map show (translate nodeSpec)) ++ "]"
                        overlayStr   = case overlay nodeSpec of Nothing     -> ""
                                                                Just nodeId -> "over " ++ nodeId
                    in acceptStr ++ " " ++ translateStr ++ " " ++ overlayStr

instance Show NetSpec where
    show netSpec = intercalate "\n" (map nodeStr (getNodes netSpec))
        where nodeStr (id, node) = id ++ " is " ++ show node