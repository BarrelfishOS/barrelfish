{-
  SockeyeBackendProlog.hs: Backend for generating ECLiPSe-Prolog facts for Sockeye

  Part of Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module SockeyeBackendProlog
( compile ) where

import Control.Monad.State

import Data.Char
import Data.List
import qualified Data.Map as Map
import Numeric (showHex)

import qualified SockeyeASTDecodingNet as AST

compile :: AST.NetSpec -> String
compile = generate

{- Code Generator -}
class PrologGenerator a where
    generate :: a -> String

instance PrologGenerator AST.NetSpec where
    generate net = let
        mapped = Map.mapWithKey toFact net
        facts = Map.elems mapped
        in unlines facts
        where
            toFact nodeId nodeSpec = let
                idString = generate nodeId
                specString = generate nodeSpec
                in struct "node" [("id", idString), ("spec", specString)] ++ "."

instance PrologGenerator AST.NodeId where
    generate ast = let
        name = AST.name ast
        namespace = AST.namespace ast
        in struct "node_id" [("name", atom name), ("namespace", list $ map atom namespace)]

instance PrologGenerator AST.NodeSpec where
    generate ast = let
        nodeType = AST.nodeType ast
        accept = AST.accept ast
        translate = AST.translate ast
        reserved = AST.reserved ast
        overlay = AST.overlay ast
        mapBlocks = map AST.srcBlock translate
        overMaps = case overlay of
            Nothing -> []
            Just o -> overlayMaps (AST.over o) (AST.width o) (accept ++ mapBlocks ++ reserved)
        nodeTypeString = generate nodeType 
        acceptString = generate accept
        translateString = generate (translate ++ overMaps)
        in struct "node_spec" [("type", nodeTypeString), ("accept", acceptString), ("translate", translateString)]

overlayMaps :: AST.NodeId -> Integer -> [AST.BlockSpec] -> [AST.MapSpec]
overlayMaps destId width blocks =
    let
        blockPoints = concat $ map toScanPoints blocks
        maxAddress = 2^width
        overStop  = BlockStart $ maxAddress
        scanPoints = filter ((maxAddress >=) . address) $ sort (overStop:blockPoints)
        startState = ScanLineState
            { insideBlocks    = 0
            , startAddress    = 0
            }
    in evalState (scanLine scanPoints []) startState
    where
        toScanPoints (AST.BlockSpec base limit) =
                [ BlockStart base
                , BlockEnd   limit
                ]
        scanLine [] ms = return ms
        scanLine (p:ps) ms = do
            maps <- pointAction p ms
            scanLine ps maps
        pointAction (BlockStart a) ms = do
            s <- get       
            let
                i = insideBlocks s
                base = startAddress s
                limit = a - 1
            maps <- if (i == 0) && (base <= limit)
                then
                    let
                        baseAddress = startAddress s
                        limitAddress = a - 1
                        srcBlock = AST.BlockSpec baseAddress limitAddress
                        m = AST.MapSpec srcBlock destId baseAddress
                    in return $ m:ms
                else return ms
            modify (\s -> s { insideBlocks = i + 1})
            return maps
        pointAction (BlockEnd a) ms = do
            s <- get
            let
                i = insideBlocks s
            put $ ScanLineState (i - 1) (a + 1)
            return ms

data StoppingPoint
    = BlockStart { address :: !AST.Address }
    | BlockEnd   { address :: !AST.Address }
    deriving (Eq, Show)

instance Ord StoppingPoint where
    (<=) (BlockStart a1) (BlockEnd   a2)
        | a1 == a2 = True
        | otherwise = a1 <= a2
    (<=) (BlockEnd   a1) (BlockStart a2)
        | a1 == a2 = False
        | otherwise = a1 <= a2
    (<=) sp1 sp2 = (address sp1) <= (address sp2)

data ScanLineState
    = ScanLineState
        { insideBlocks :: !Integer
        , startAddress :: !AST.Address
        } deriving (Show)

instance PrologGenerator AST.NodeType where
    generate AST.Core   = atom "core"
    generate AST.Device = atom "device"
    generate AST.Memory = atom "memory"
    generate AST.Other  = atom "other"

instance PrologGenerator AST.BlockSpec where
    generate blockSpec = let
        base = generate $ AST.base blockSpec
        limit = generate $ AST.limit blockSpec
        in struct "block" [("base", base), ("limit", limit)]

instance PrologGenerator AST.MapSpec where
    generate mapSpec = let
        src  = generate $ AST.srcBlock mapSpec
        dest = generate $ AST.destNode mapSpec
        base = generate $ AST.destBase mapSpec
        in struct "map" [("src_block", src), ("dest_node", dest), ("dest_base", base)]

instance PrologGenerator AST.Address where
    generate addr = "0x" ++ showHex addr ""

instance PrologGenerator a => PrologGenerator [a] where
    generate ast = let
        mapped = map generate ast
        in list mapped

{- Helper functions -}
atom :: String -> String
atom "" = ""
atom name@(c:cs)
    | isLower c && allAlphaNum cs = name
    | otherwise = quotes name
    where
        allAlphaNum cs = foldl (\acc c -> isAlphaNum c && acc) True cs

predicate :: String -> [String] -> String
predicate name args = name ++ (parens $ intercalate "," args)

struct :: String -> [(String, String)] -> String
struct name fields = name ++ (braces $ intercalate "," (map toFieldString fields))
    where
        toFieldString (key, value) = key ++ ":" ++ value

list :: [String] -> String
list elems = brackets $ intercalate "," elems

enclose :: String -> String -> String -> String
enclose start end string = start ++ string ++ end

parens :: String -> String
parens = enclose "(" ")"

brackets :: String -> String
brackets = enclose "[" "]"

braces :: String -> String
braces = enclose "{" "}"

quotes :: String -> String
quotes = enclose "'" "'"