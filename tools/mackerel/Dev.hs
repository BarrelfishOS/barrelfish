{- 
  Dev: representation of a device
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, 2008, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module Dev where

import MackerelParser


import qualified TypeTable as TT
import qualified RegisterTable as RT
import qualified ConstTable as CT
import qualified Space


{--------------------------------------------------------------------

--------------------------------------------------------------------}

data Rec = Rec { name :: String,
                 desc :: String,
                 args :: [ AST ],
                 types ::[ TT.Rec ],
                 dtypes ::[ TT.Rec ],
                 cnstnts ::[ CT.Rec ],
                 registers ::[ RT.Rec ],
                 spaces :: [ Space.Rec ]
               }

make_dev :: AST -> Rec
make_dev (Device n bitorder al d decls) =
    let sptbl = Space.builtins ++ [ rec | (SpaceDecl rec) <- decls ]
        ttbl = TT.make_rt_table decls bitorder
        rtbl = RT.make_table ttbl decls bitorder sptbl
    in
      Rec { name = n, 
            desc = d, 
            args = al, 
            types = ttbl,
            dtypes = TT.make_dt_table decls bitorder,
            cnstnts = CT.make_table decls, 
            registers = rtbl,
            spaces = sptbl
        }

shdws :: Rec -> [ RT.Shadow ]
shdws dev = RT.get_shadows (registers dev)
