{-
  SockeyeBackendIsabelle.hs: Backend for generating Isabelle/HOL for Sockeye

  Part of Sockeye

  Copyright (c) 2018, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeBackendIsabelle
( compile ) where

import qualified SockeyeSymbolTable as ST
import qualified SockeyeAST as AST

compile :: ST.Sockeye -> AST.Sockeye -> String
compile symTable ast = "Isabelle backend not yet implemented"
