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

import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Numeric (showHex)

import qualified SockeyeASTDecodingNet as AST

compile :: AST.NetSpec -> String
compile = fromJust . generate

{- Code Generator -}
class PrologGenerator a where
    generate :: a -> Maybe String

instance PrologGenerator AST.NetSpec where
    generate (AST.NetSpec net) = do
        let
            mapped = Map.mapWithKey toFact net
            facts = catMaybes $ Map.elems mapped
        return $ unlines facts
        where
            toFact nodeId nodeSpec = do
                atom <- generate nodeId
                node <- generate nodeSpec
                return $ predicate "net" [atom, node] ++ "."

instance PrologGenerator AST.NodeId where
    generate ast = do
        return $ (atom $ show ast)

instance PrologGenerator AST.NodeSpec where
    generate (AST.AliasSpec alias) = maybe Nothing generate alias
    generate ast = do
        nodeType <- generate $ AST.nodeType ast
        accept <- generate $ AST.accept ast
        translate <- generate $ AST.translate ast
        overlay <- case AST.overlay ast of
            Nothing -> return $ atom "@none"
            Just id -> generate id
        return $ predicate "node" [nodeType, accept, translate, overlay]

instance PrologGenerator AST.BlockSpec where
    generate blockSpec = do
        base  <- generate $ AST.base blockSpec
        limit <- generate $ AST.limit blockSpec
        return $ predicate "block" [base, limit]

instance PrologGenerator AST.MapSpec where
    generate mapSpec = do
        src  <- generate $ AST.srcBlock mapSpec
        dest <- generate $ AST.destNode mapSpec
        base <- generate $ AST.destBase mapSpec
        return $ predicate "map" [src, dest, base]

instance PrologGenerator AST.NodeType where
    generate AST.Memory = return $ atom "memory"
    generate AST.Device = return $ atom "device"
    generate AST.Other  = return $ atom "other"

instance PrologGenerator AST.Address where
    generate (AST.Address addr) = return $ "16'" ++ showHex addr ""

instance PrologGenerator a => PrologGenerator [a] where
    generate ast = do
        let
            mapped = map generate ast
        return $ (list . catMaybes) mapped

{- Helper functions -}
atom :: String -> String
atom name@(c:cs)
    | isLower c && alphaNum cs = name
    | otherwise = quotes name
    where
        alphaNum cs = foldl (\acc c -> isAlphaNum c && acc) True cs

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