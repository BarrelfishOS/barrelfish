{-
  SockeyeParser.hs: Parser for Sockeye

  Part of Sockeye

  Copyright (c) 2018, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeChecker where

import SockeyeChecks

import SockeyeASTMeta
import qualified SockeyeParserAST as ParseAST
import qualified SockeyeSymbolTable as ST
import qualified SockeyeAST as AST

data CheckFail
    = NotYetImplemented

instance CheckFailure CheckFail where
    errorLines NotYetImplemented = ["Sockeye Checker not yet implemented"]

checkSockeye :: ST.Sockeye -> ParseAST.Sockeye -> Either (FailedChecks CheckFail) AST.Sockeye
checkSockeye symTable ast = Right AST.Sockeye
