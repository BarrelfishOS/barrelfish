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
    facts :: [DT.Rec],
    types :: [TT.Rec],
    imports :: [ String ]
}


skateSchemaTransform :: SchemaRecord -> IO(SchemaRecord)
skateSchemaTransform sr = do {
    return (sr)
 }



make_schema_record :: Schema -> [Schema] -> IO SchemaRecord
make_schema_record s@(Schema n d decls imps) dfl =
    do
        printf "Creating SchemRecord.\n";
        ttbl <- TT.make_table s;
        --ftbl <- DT.make_table ttbl decls n;
        --qtbl <- DT.make_table ttbl decls n;
        return SchemaRecord {
            name = n,
            desc = d,
            schema = s,
            types = ttbl,
    -- all_types = ttbl ++ ( concat $ map TT.make_rtypetable dfl ),
            facts = [],
                imports = imps
            }




skateSchemaGetAst :: SchemaRecord -> Schema
skateSchemaGetAst  sr@(SchemaRecord n d s f t i) = s
