{-
  SockeyeBackendProlog.hs: Backend for generating Prolog facts for Sockeye

  Part of Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeBackendProlog
( compile ) where

import Data.List
import Data.Char

import qualified SockeyeAST as AST

compile :: AST.NetSpec -> String
compile = generate

{- Code Generator -}
class PrologGenerator a where
    generate :: a -> String

instance PrologGenerator AST.NetSpec where
    generate (AST.NetSpec net) = unlines $ map toFact net
        where toFact (nodeId, nodeSpec) = let atom = generate nodeId
                                              node = generate nodeSpec
                                          in predicate "net" [atom, node] ++ "."

instance PrologGenerator AST.NodeId where
    generate (AST.NodeId id) = quotes id

instance PrologGenerator AST.NodeSpec where
    generate nodeSpec = predicate "node" [nodeType, accept, translate, overlay]
        where nodeType = generate (AST.nodeType nodeSpec)
              accept = list $ map generate (AST.accept nodeSpec)
              translate = list $ map generate (AST.translate nodeSpec)
              overlay = case AST.overlay nodeSpec of
                Nothing -> "'@none'"
                Just id -> generate id

instance PrologGenerator AST.BlockSpec where
    generate blockSpec = let base  = generate $ AST.base blockSpec
                             limit = generate $ AST.limit blockSpec
                         in predicate "block" [base, limit]

instance PrologGenerator AST.MapSpec where
    generate mapSpec = let src  = generate $ AST.srcBlock mapSpec
                           dest = generate $ AST.destNode mapSpec
                           base = generate $ AST.destBase mapSpec
                       in predicate "map" [src, dest, base]

instance PrologGenerator AST.NodeType where
    generate = show 

instance PrologGenerator AST.Addr where
    generate (AST.Addr addr) = show addr

{- Helper functions -}
predicate :: String -> [String] -> String
predicate name args = name ++ (parens $ intercalate "," args)

list :: [String] -> String
list elems = brackets $ intercalate "," elems

enclose :: String -> String -> String -> String
enclose start end string = start ++ string ++ end

parens :: String -> String
parens = enclose "(" ")"

brackets :: String -> String
brackets = enclose "[" "]"

quotes :: String -> String
quotes = enclose "'" "'"