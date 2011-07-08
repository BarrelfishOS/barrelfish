{- 
   Fields: Mackerel register fields
   
  Part of Mackerel: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2007, 2008, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}  

module Fields where

import Attr
import Data.Bits
import Text.ParserCombinators.Parsec
import MackerelParser

data Rec = Rec { name :: String,
                 size :: Integer,
                 offset :: Integer,
                 attr :: Attr,
                 initial :: Integer,
                 tpe :: Maybe String,
                 desc :: String,
                 pos :: SourcePos,
                 is_anon :: Bool }
           deriving (Show,Eq)

is_writeonly f = attr_is_writeonly (attr f)
is_readable  f = attr_is_readable (attr f)
is_writeable f = attr_is_writeable (attr f)
is_rsvd Rec { attr = RSVD } = True
is_rsvd _ = False

--
-- Create a list of fields, in the right order, with the right default
-- attribute, from a set of declarations.
--
make_list :: Attr -> BitOrder -> Integer -> [AST] -> [Rec]
make_list dflt order 0 decls 
    = make_list_from_word dflt order 0 decls
make_list dflt order word_size decls 
    = make_list_of_words dflt order word_size 0 decls []

make_list_of_words dflt order word_size off decls acc 
    = let acc_length = foldl (+) 0 [ s | (RegField _ s _ _ _ _) <- acc ]
      in
        if acc_length >= word_size then
            let al = make_list_from_word dflt order off acc
                new_off = (offset $ last al) + (size $ last al)
            in 
              al ++ (make_list_of_words dflt order word_size new_off decls [])
        else 
            if (length decls) == 0 then
                 make_list_from_word dflt order off acc
            else
                make_list_of_words dflt order word_size off (tail decls) (acc ++ [head decls])


make_list_from_word :: Attr -> BitOrder -> Integer -> [AST] -> [Rec]
make_list_from_word dflt LSBFIRST init_offset decls = 
    -- Cons up a list of the offsets of each field in the structure. 
    let add_sizes decls = 
            foldl (\t (RegField _ s _ _ _ _) -> t ++ [(last t) + s]) [init_offset] decls
    in map (make_field dflt False) $ zip decls (add_sizes decls)
make_list_from_word dflt MSBFIRST init_offset decls = 
    make_list dflt LSBFIRST init_offset (reverse decls)


--
-- Create a list of fields, in the right order, with the right 
-- attribute, from a set of other fields (e.g. from a type).
-- The inheritance rules for attributes are as follows:
--
inherit_list :: Attr -> [Rec] -> [Rec]
inherit_list regattr ftlist = 
    [ r { attr = (if (attr r) == NOATTR then regattr else (attr r)) } | r <- ftlist ]

--
-- Fix up default attributes.  Anything without an attributed defaults
-- to the attribute of the register (dflt here), unless it's "_", in
-- which case it defaults to RSVD.
--
make_field :: Attr -> Bool -> (AST, Integer) -> Rec
make_field dflt anon ((RegField id sz a t dsc p), off) 
    | id == "_" = 
        make_field RSVD True ((RegField ("_anon" ++ show off) sz a t "_" p), off)
    | otherwise = 
        Rec { name = id, 
              size = sz, 
              offset = off,
              initial = if a == MB1 then (shift 1 $ fromInteger sz) - 1 else 0,
              attr = if a == NOATTR then dflt else a,
              tpe = (make_ftype t),
              desc = dsc, 
              pos = p, 
            is_anon = anon }

make_ftype :: AST -> Maybe String
make_ftype NoBitFieldType = Nothing
make_ftype (TypeRef i) = Just i

--
-- Generate masks and shifts for isolating this field.  These functions
-- are polymorphic so that they don't need to know how large the total
-- load unit is (32 bits? 8 bits?) etc.
--
extract_mask :: (Bits a) => Rec -> Integer -> a
extract_mask f sz = 
    foldl setBit 0 (enumFromTo (fromInteger $ offset f) 
                               (fromInteger $ (offset f) + (size f) - 1))
insert_mask :: (Bits a) => Rec -> Integer -> a
insert_mask f sz = 
    foldl complementBit (extract_mask f sz) (enumFromTo 0 (fromInteger sz - 1))

extract_shift :: Rec -> Integer
extract_shift f = - (insert_shift f)

insert_shift :: Rec -> Integer
insert_shift f = offset f

initial_mask :: Rec -> Integer
initial_mask f = shift (initial f) (fromInteger $ insert_shift f)
