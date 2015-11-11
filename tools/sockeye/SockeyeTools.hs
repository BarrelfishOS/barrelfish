{-
  SockeyeTools.hs: Tools for the Sockeye parser

  Part of Sockeye: a strawman device definition DSL for Barrelfish

  Copyright (c) 2015, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeTools where
import SockeyeSyntax

import qualified Data.Map as Map

collectTypes :: Schema -> (Map.Map String TypeRef)
collectTypes (Schema name doc ast) = Map.unions (map collectTypes_Declaration ast)

collectTypes_Declaration :: Declaration -> Map.Map String TypeRef
collectTypes_Declaration (Typedef typeDef) = collectTypes_TypeDef typeDef
collectTypes_Declaration (Factdef factDef) = collectTypes_FactDef factDef
collectTypes_Declaration (Querydef queryDef) = collectTypes_QueryDef queryDef

collectTypes_TypeDef :: TypeDef -> Map.Map String TypeRef
collectTypes_TypeDef (TEnum _ _) = Map.empty
collectTypes_TypeDef (TAlias name ref) = Map.singleton name ref
collectTypes_TypeDef (TAliasT name builtin) = Map.singleton name (Builtin builtin)

collectTypes_FactDef :: FactDef -> Map.Map String TypeRef
collectTypes_FactDef (Fact name _ _) = Map.singleton name (FactType name)

collectTypes_QueryDef :: QueryDef -> Map.Map String TypeRef
collectTypes_QueryDef (Query _ _ _) = Map.empty

rewireTypes :: Schema -> Map.Map String TypeRef -> Schema
rewireTypes ast decls = rewireTypes_Schema ast decls

rewireTypes_Schema (Schema name doc ast) decls =
    Schema name doc [rewireTypes_Declaration i decls | i <- ast]

rewireTypes_Declaration (Typedef t) _ = Typedef t
rewireTypes_Declaration (Factdef f) decl = Factdef (rewireTypes_FactDef f decl)

rewireTypes_FactDef f@(Fact name doc fattribs) decls =
    Fact name doc [rewireTypes_FactAttribute i decls | i <- fattribs]

rewireTypes_FactAttribute (FAttrib t var doc) decls =
    FAttrib (rewireTypes_TypeRef t decls) var doc

rewireTypes_TypeRef (UnknownType name) decls = decls Map.! name
rewireTypes_TypeRef t _ = t
