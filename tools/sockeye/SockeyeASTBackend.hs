{-
  SockeyeASTBackend.hs: Backend AST for Sockeye

  Part of Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeASTBackend where

import Data.List
import Numeric (showHex)

{-
Nodes are identfied by strings
-}
newtype NodeId = NodeId String deriving (Eq, Ord)

{-
Addresses are natural numbers
-}
newtype Addr = Addr Word deriving (Eq, Ord)

{-
A block is a contigous set of addresses
-}
data BlockSpec = BlockSpec
    { base  :: Addr
    , limit :: Addr
    } deriving (Eq, Ord)

{-
A mapping of a source address block to a destination node
at a base address
-}
data MapSpec = MapSpec { srcBlock :: BlockSpec
                       , destNode :: NodeId
                       , destBase :: Addr
                       }

{-
Node can either be memory, device or other
-}
data NodeType = Memory
              | Device
              | Other

{-
A node is specified as a list of blocks it accepts,
a list of mappings and possibly an overlay on another block
-}
data NodeSpec = NodeSpec { nodeType  :: NodeType
                         , accept    :: [BlockSpec]
                         , translate :: [MapSpec]
                         , overlay   :: Maybe NodeId
                         }

{-
A decoding net is specified as a list 
of Node IDs mapped to Nodes
-}
newtype NetSpec = NetSpec [(NodeId, NodeSpec)]

{- Pretty Printing -}
instance Show NodeId where
    show (NodeId id) = id

instance Show Addr where
    show (Addr addr) = "0x" ++ showHex addr ""

instance Show BlockSpec where
    show blockSpec = show (base blockSpec) ++ "-" ++ show (limit blockSpec)

instance Show MapSpec where
    show mapSpec = let srcStr  = show $ srcBlock mapSpec
                       nodeStr = show $ destNode mapSpec
                       baseStr = show $ destBase mapSpec
                   in srcStr ++ " to " ++ nodeStr ++ " at " ++ baseStr

instance Show NodeType where
  show Memory = "memory"
  show Device = "device"
  show Other  = "other"

instance Show NodeSpec where
    show nodeSpec = let typeStr      = show $ nodeType nodeSpec
                        acceptStr    = "accept [" ++ intercalate ", " (map show (accept nodeSpec)) ++ "]"
                        translateStr = "map [" ++ intercalate ", " (map show (translate nodeSpec)) ++ "]"
                        overlayStr   = case overlay nodeSpec of
                                        Nothing     -> ""
                                        Just nodeId -> "over " ++ show nodeId
                    in intercalate " " [typeStr, acceptStr, translateStr, overlayStr]

instance Show NetSpec where
    show (NetSpec netSpec) = unlines $ map nodeStr netSpec
                             where nodeStr (id, node) = show id ++ " is " ++ show node