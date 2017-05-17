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

import qualified SockeyeAST as AST

compile :: AST.NetSpec -> String
compile _ = "Prolog backend is under construction..."
