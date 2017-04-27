{- 
  SkateTypeTable: List of all defined types
   
  Part of Skate: a Schema specification languge
   
  Copyright (c) 2017, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-} 

module SkateTypeTable where

import SkateParser

data Rec = BuiltIn { name :: String }
           | Alias { name :: String }
           | Flags { name :: String }
           | Constants {name :: String }



make_table :: Schema -> [Rec]
make_table s@(Schema n d decls imps) =[]
    