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

data RecType = TTBuiltIn | TTFlags | TTConstant | TTEnum | TTFact

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
exist :: [TTEntry] -> String -> Bool
exist t a = not (null (filter (typeExists a) t))


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
addOneTypeToTable p t (xs:x) = (addOneTypeToTable p (t ++ parseType p t xs) x)
addOneTypeToTable p t [] = t


{- handles each declaration and adds a type  -}
parseType :: String -> [TTEntry] -> Declaration -> [TTEntry]
parseType p t d@(Fact i _ _) = addOneType (make_qualified_name p i) t TTFact
parseType p t d@(Flags i _ w _) = addOneType (make_qualified_name p i) t TTFlags
parseType p t d@(Constants i _ _ _) = addOneType (make_qualified_name p i) t TTConstant
parseType p t d@(Enumeration i _ _) = addOneType (make_qualified_name p i) t TTEnum
parseType p t d@(Namespace i _ decls) = addOneTypeToTable (make_qualified_name p i) t decls
parseType p t d@(Section _ decls) = addOneTypeToTable p t decls
parseType p t d@(Text _) = t

{- boolean function that returns True iff the type record matches -}
typeExists :: String -> TTEntry -> Bool
typeExists a d@(Rec _ e) = (a == e)

{- adds one type to the type table -}
addOneType :: String -> [TTEntry] -> RecType -> [TTEntry]
addOneType n recs t =
    if null (filter (typeExists n) recs) then  recs ++ [Rec t n]
    else error $ "error: type '" ++ n ++ "' has already been defined";


{- xxx: move this somewhere else ... -}
make_qualified_name :: String -> String -> String
make_qualified_name "" i = i
make_qualified_name q i = q ++ "." ++ i
