{- 
  Type table: list of all register types
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, 2008, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module TypeTable where

import MackerelParser
import Attr
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import qualified Fields as F

{--------------------------------------------------------------------

--------------------------------------------------------------------}

data Rec = Format { tt_name :: String, 
                    tt_size :: Integer,
                    fields :: [F.Rec],
                    tt_desc :: String,
                    pos :: SourcePos,
                    is_reg :: Bool }
         | Primitive  { tt_name :: String,
                        tt_size :: Integer,
                        tt_attr :: Attr }
           deriving Show 

is_primitive Format {} = False
is_primitive Primitive {} = True

is_builtin Format {} = False
is_builtin Primitive { tt_name = n } = is_builtin_type n

builtin_size "uint8" = 8
builtin_size "uint16" = 16
builtin_size "uint32" = 32
builtin_size "uint64" = 64

is_builtin_type "uint8" = True
is_builtin_type "uint16" = True
is_builtin_type "uint32" = True
is_builtin_type "uint64" = True
is_builtin_type some_type = False



make_rt_table :: [AST] -> BitOrder -> [Rec]
make_rt_table decls order = concat [ (make_rtrec d order) | d <- decls ]

make_rtrec :: AST -> BitOrder -> [Rec]
make_rtrec (RegType nm dsc (TypeDefn decls) p) order = 
    [ Format { tt_name = nm,
               tt_size = (calc_tt_size decls),
               fields = F.make_list NOATTR order 0 decls,
               tt_desc = dsc,
               pos = p,
               is_reg = True } ]

make_rtrec (Register nm tt_attrib _ _ dsc (TypeDefn decls) p) order = 
    [ Format { tt_name = nm,
               tt_size = (calc_tt_size decls),
               fields = F.make_list tt_attrib order 0 decls,
               tt_desc = "Implicit type of " ++ dsc ++ " register",
               pos = p,
               is_reg = True } ]

make_rtrec (RegArray nm tt_attrib _ _ _ dsc (TypeDefn decls) p) order = 
    [ Format { tt_name = nm,
               tt_size = (calc_tt_size decls),
               fields = F.make_list NOATTR order 0 decls,
               tt_desc = "Implicit type of " ++ dsc ++ " register array",
               pos = p,
               is_reg = True } ]

make_rtrec _ _ = []


make_dt_table :: [AST] -> BitOrder -> [Rec]
make_dt_table decls order = concat [ (make_dtrec d order) | d <- decls ]

make_dtrec :: AST -> BitOrder -> [Rec]
make_dtrec (DataType nm dsc (TypeDefn decls) o w p) devorder = 
    let order = if o == NOORDER then devorder else o
    in
      [ Format { tt_name = nm,
                 tt_size = (calc_tt_size decls),
                 fields = F.make_list RW order w decls,
                 tt_desc = dsc,
                 pos = p,
                 is_reg = False } ]

make_dtrec _ _ = []

calc_tt_size :: [AST] -> Integer
calc_tt_size decls = sum [ sz | (RegField _ sz _ _ _ _) <- decls ]

get_rtrec :: [Rec] -> String -> Rec
get_rtrec rtinfo nm = 
    let l  = [ rt | rt <- rtinfo, (tt_name rt) == nm ]
    in
      if (length l) > 0
      then head l
      else Format { tt_name = "", -- never going to exist
            tt_size = 32,
            fields = [],
            tt_desc = "Fictional non-existent type",
            pos = initialPos "no file",
            is_reg = True } 
