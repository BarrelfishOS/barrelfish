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

data Rec = Rec RecType String

instance Show Rec where
    show (Rec _ s) = "TT.Rec: " ++ s

make_table :: Schema -> IO [Rec]
make_table s@(Schema n d decls imps) =
    let
        tt = addDecls n [] decls;
    in
    do {
    printf "Creating TypeTable.\n";

    print (show tt);
    return (tt)
}

--

addDecls :: String -> [Rec] -> [Declaration] -> [Rec]
--addDecls p t decls = foldl (++) t [ parseType p [] decl | decl <- decls ]
addDecls p t decls = [t | decl <- decls, t <- parseType p t decl] --

make_qualified_name :: String -> String -> String
make_qualified_name "" i = i
make_qualified_name q i = q ++ "." ++ i

parseType :: String -> [Rec] -> Declaration -> [Rec]
parseType p t d@(Fact i _ _) = addTypes (make_qualified_name p i) t TTFact
parseType p t d@(Flags i _ w _) = addTypes (make_qualified_name p i) t TTFlags
parseType p t d@(Constants i _ _ _) = addTypes (make_qualified_name p i) t TTConstant
parseType p t d@(Enumeration i _ _) = addTypes (make_qualified_name p i) t TTEnum
parseType p t d@(Namespace i _ decls) = addDecls (make_qualified_name p i) t decls
parseType p t d@(Section _ decls) = addDecls p t decls
parseType p t d@(Text _) = t

typeExists :: String -> Rec -> Bool
typeExists a d@(Rec _ e) = (a == e)

addTypes :: String -> [Rec] -> RecType -> [Rec]
addTypes n recs t =
    let
        e = filter (typeExists n) recs
    in
    if null e then  recs ++ [Rec t n]
    else error $ "FOOBAR" ++ n ++ "already defined"

--let ttbl = TT.make_table s
--in

{-

parseTypeFact :: String -> [Rec] -> Fact -> [Rec]
parseTypeFact prefix types decl@(Fact i _ _) = types ++ [Rec TTFact (make_qualified_name prefix i)]

parseTypeFlags :: String -> [Rec] -> Flags -> [Rec]
parseTypeFlags prefix types decl@(Flags i _ w _) = []

parseTypeConstants :: String -> [Rec] -> Constants -> [Rec]
parseTypeConstants prefix types decl@(Constants i _ _ _) = []

parseTypeEnumeration :: String -> [Rec] -> Enumeration -> [Rec]
parseTypeEnumeration prefix types decl@(Enumeration i _ _) = []

parseTypeNamespace :: String -> [Rec] -> Namespace -> [Rec]
parseTypeNamespace prefix types decl@(Namespace i _ decls) =  types ++ (parseType (make_qualified_name prefix i) types decls)

parseTypeSection :: String -> [Rec] -> Section -> [Rec]
parseTypeSection prefix types decl@(Section _ decls) = types ++ (parseType prefix types decls)

parseTypeText :: String -> [Rec] -> Text -> [Rec]
parseTypeText prefix types decl@(Text ) = []
-}
