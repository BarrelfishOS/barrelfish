{- 
   GCBackend: Flounder stub generator for generic code
   
  Part of Flounder: a message passing IDL for Barrelfish
   
  Copyright (c) 2007-2010, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module SockeyeDocBackend where

import qualified CAbsSyntax as C
import SockeyeSyntax (Schema (Schema))

compile :: String -> String -> SockeyeSyntax.Schema -> String
compile infile outfile schema = 
    ""