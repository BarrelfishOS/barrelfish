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

import qualified SkateTypeTable as TT
import qualified SkateDeclarationTable as DT

data SchemaRecord = SchemaRecord {
    name :: String,
    desc :: String,
    schema:: Schema,
    facts :: [DT.Rec],
    queries :: [DT.Rec],
    types :: [TT.Rec],
    imports :: [ String ]
}


skateSchemaTransform :: SchemaRecord -> SchemaRecord
skateSchemaTransform sr = sr

make_schema_record :: Schema -> [Schema] -> SchemaRecord
make_schema_record s@(Schema n d decls imps) dfl =
    let ttbl = TT.make_table s
        ftbl = DT.make_table ttbl decls n
        qtbl = DT.make_table ttbl decls n
    in
        (skateSchemaTransform SchemaRecord { 
            name = n, 
            desc = d,   
            schema = s, 
            types = ttbl,
            -- all_types = ttbl ++ ( concat $ map TT.make_rtypetable dfl ),
            facts = ftbl,
            queries = qtbl,
            imports = imps
        })

        



skateSchemaGetAst :: SchemaRecord -> Schema
skateSchemaGetAst sr = (schema sr)


