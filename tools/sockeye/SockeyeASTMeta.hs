{-
    SockeyeASTMeta.hs: AST metadata for Sockeye

    Part of Sockeye

    Copyright (c) 2018, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

module SockeyeASTMeta where

import Data.List (intercalate)
import Text.Parsec.Pos

newtype ASTMeta = ParserMeta SourcePos
    deriving (Eq, Ord)

instance Show ASTMeta where
    show (ParserMeta pos) = intercalate ":" [sourceName pos, show $ sourceLine pos, show $ sourceColumn pos]

class MetaAST a where
    meta :: a -> ASTMeta
