{-
  SockeyeBackendPrintAST.hs: Backend for printing the AST for Sockeye

  Part of Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeBackendPrintAST
( compile ) where

import qualified SockeyeASTBackend as AST

compile :: AST.NetSpec -> String
compile = show
