{- 
  Const table: list of all constants types
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, 2008, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module ConstTable where

import MackerelParser
import Text.ParserCombinators.Parsec

{--------------------------------------------------------------------

--------------------------------------------------------------------}

data Val = Val { cname :: String,
                 cval :: Expr,
                 cdesc :: String,
                 ctype :: String,
                 cpos :: SourcePos }
         deriving Show

data Rec = Rec { name :: String,
                 desc :: String,
                 vals :: [ Val ],
                 width :: Maybe Integer,
                 pos  :: SourcePos }

make_table :: [AST] -> [Rec]
make_table decls = concat [ (make_rec d) | d <- decls ]


make_rec :: AST -> [Rec]
make_rec (Constants n d vs p) = 
    [ Rec { name = n, desc = d, vals = [ make_val n v | v <- vs ], pos = p }]
make_rec _ = []

make_val :: String -> AST -> Val
make_val tn (ConstVal i e d p) 
    = Val { cname = i, cval = e, cdesc = d, ctype = tn, cpos = p }

make_val_table :: [Rec] -> [Val] 
make_val_table recs = concat [ (vals r) | r <- recs ]



