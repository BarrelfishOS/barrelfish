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

import Data.List

import System.IO
import System.IO.Error
import Text.Printf

import SkateParser
import SkateTypes

data RecType = TTBuiltIn | TTFlags | TTConstant | TTEnum | TTFact
    deriving(Eq)

instance Show RecType where
    show TTBuiltIn = "builtin"
    show TTFlags = "flag"
    show TTConstant = "const"
    show TTEnum = "enum"
    show TTFact = "fact"

data TTEntry = Rec RecType String

instance Show TTEntry where
    show (Rec _ s) = "TT.Rec: " ++ s


{-
==============================================================================
= Public Functions
==============================================================================
-}

{- creates the Skate type table -}
make_table :: Schema -> IO [TTEntry]
make_table s@(Schema n d decls imps) =
    let
        tt = addOneTypeToTable n [] decls;
    in
    do {
    printf "Creating TypeTable.\n";

    print (show tt);
    return (tt)
}

{- -}
exist :: [TTEntry] -> RecType -> String -> Bool
exist ttbl t a = not (null (filter (type_ref_exists a t) ttbl))

{- -}
lookup ::  [TTEntry] -> String -> RecType
lookup t a = tt
    where
        Rec tt _ = head (filter (typeExists a) t)

{-
==============================================================================
= Module Private Functions
==============================================================================
-}


{- recursively adds a list of declarations to the type table -}
addOneTypeToTable :: String -> [TTEntry] -> [Declaration] -> [TTEntry]
addOneTypeToTable p t (xs:x) = (addOneTypeToTable p (parseType p t xs) x)
addOneTypeToTable p t [] = t


{- handles each declaration and adds a type  -}
parseType :: String -> [TTEntry] -> Declaration -> [TTEntry]
parseType p t d@(Fact i _ _) = addOneType i t TTFact
parseType p t d@(Flags i _ w _) = addOneType i t TTFlags
parseType p t d@(Constants i _ _ _) = addOneType i t TTConstant
parseType p t d@(Enumeration i _ _) = addOneType i t TTEnum
parseType p t d@(Namespace i _ decls) = addOneTypeToTable i t decls
parseType p t d@(Section _ decls) = addOneTypeToTable p t decls
parseType p t d@(Text _) = t

{- boolean function that returns True iff the type record matches -}
typeExists :: String -> TTEntry -> Bool
typeExists a d@(Rec _ e) = (a == e)

{- boolean function that returns True iff the type record matches -}
type_ref_exists :: String -> RecType -> TTEntry -> Bool
type_ref_exists a t d@(Rec tt e) = ((a == e) &&  (t == tt))

{- adds one type to the type table -}
addOneType :: String -> [TTEntry] -> RecType -> [TTEntry]
addOneType n recs t =
    if null (filter (typeExists n) recs) then recs ++ [Rec t n]
    else error $ "error: type '" ++ n ++ "' has already been defined";
