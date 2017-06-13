{-
  SkateSchema: Represents a parsed Skate Schema

  Part of Skate: a Schema specification languge

  Copyright (c) 2017, ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}

module SkateSchema where

import SkateParser

import System.IO
import System.IO.Error
import Text.Printf

import qualified SkateTypeTable as TT
import qualified SkateDeclarationTable as DT

data SchemaRecord = SchemaRecord {
    name :: String,
    desc :: String,
    schema:: Schema,

    facts :: [Declaration],
    flags :: [Declaration],
    constants :: [Declaration],
    enumerations :: [Declaration],
    namespaces :: [Declaration],

    types :: [TT.TTEntry],
    allTypes :: [TT.TTEntry],
    imports :: [ String ]
}


skateSchemaTransform :: SchemaRecord -> IO(SchemaRecord)
skateSchemaTransform sr = do {
    return (sr)
 }



make_schema_record :: Schema -> [Schema] -> IO SchemaRecord
make_schema_record s@(Schema n d decls imps sp) dfl =
    do {
        printf "Creating SchemRecord.\n";
        ttbl <- TT.make_table s;
        DT.DTRec ns fa fl co en <-  DT.make_table s ttbl;
        --dt <- DT.make_table s ttbl;
        --facts <- DT.make_facts_table s ttbl
        --namespaces <-DT.make_namespaces_table s ttbl
        --flags <-DT.make_flags_table s ttbl
        --constants <-DT.make_constants_table s ttbl
        --enums <-DT.make_enumerations_table s ttbl

        --ftbl <- DT.make_table ttbl decls n;
        --qtbl <- DT.make_table ttbl decls n;
        return SchemaRecord {
            name = n,
            desc = d,
            schema = s,
            types = ttbl,
            allTypes = ttbl, -- ++ ( concat $ map TT.make_rtypetable dfl ),
            facts = fa,
            flags = fl,
            constants = co,
            enumerations = en,
            namespaces = ns,
            imports = imps
            }
        }




skateSchemaGetAst :: SchemaRecord -> Schema
skateSchemaGetAst  sr@(SchemaRecord _ _ s _ _ _ _ _ _ _ _) = s

skateSchemaGetFacts :: SchemaRecord -> [Declaration]
skateSchemaGetFacts  sr = facts sr
