{- 
  SkateBackendWiki: Backend to generate a Wiki documentation
   
  Part of Skate: a Schema specification languge
   
  Copyright (c) 2017, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}  

module SkateBackendWiki   where

import qualified SkateParser

compile :: String -> String -> SkateParser.Schema -> String
compile infile outfile schema = 
    ""