{-
  SkateDeclarationTable: List of all declarations

  Part of Skate: a Schema specification languge

  Copyright (c) 2017, ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}


module SkateDeclarationTable where


import Data.List

import System.IO
import System.IO.Error
import Text.Printf

import SkateParser
import SkateTypes
import qualified SkateTypeTable as TT

data Rec = Rec {
    name :: String
}

data DeclarationTable = DTRec [Declaration] [Declaration] [Declaration] [Declaration] [Declaration]

{-
==============================================================================
= Public Functions
==============================================================================
-}

make_table :: Schema -> [TT.TTEntry] -> IO DeclarationTable
make_table s@(Schema n d decls imps) ttbl = do {
    printf "Creating DeclarationTable.\n";
    print (show facts);
    print (show namespaces);
    print (show flags);
    print (show constants);
    print (show enumerations);
    return (DTRec namespaces facts flags constants enumerations)
}
    where
        fdecls = flatten_decl_tree n [] decls;
        facts = filter fact_filter fdecls;
        namespaces = filter namespace_filter fdecls;
        flags = filter flags_filter fdecls;
        constants = filter constants_filter fdecls;
        enumerations = filter enumeration_filter fdecls;


{-
==============================================================================
= Module Private Functions
==============================================================================
-}


{- filter functions -}
fact_filter :: Declaration -> Bool
fact_filter d@(Fact _ _ _) = True
fact_filter _ = False

namespace_filter :: Declaration -> Bool
namespace_filter d@(Namespace _ _ _) = True
namespace_filter _ = False

flags_filter :: Declaration -> Bool
flags_filter d@(Flags _ _ _ _) = True
flags_filter _ = False

constants_filter :: Declaration -> Bool
constants_filter d@(Constants _ _ _ _) = True
constants_filter _ = False

enumeration_filter :: Declaration -> Bool
enumeration_filter d@(Enumeration _ _ _) = True
enumeration_filter _ = False


{- recursively go over the declaration list -}
flatten_decl_tree :: String -> [Declaration] -> [Declaration] -> [Declaration]
flatten_decl_tree p t (xs:x) = (flatten_decl_tree p ((parseType p t xs)) x)
flatten_decl_tree p t [] = t


{- handles each declaration and adds a type  -}
parseType :: String -> [Declaration] -> Declaration -> [Declaration]
parseType p t x@(Fact i d a) = t ++ [x]
parseType p t x@(Flags i d w f) = t ++ [x]
parseType p t x@(Constants i d a w) = t ++ [x]
parseType p t x@(Enumeration i d e) = t ++ [x]
parseType p t x@(Namespace i d decls) = flatten_decl_tree i (t ++ [x]) decls
parseType p t x@(Section _ decls) = flatten_decl_tree p t decls
parseType p t x@(Text _) = t
