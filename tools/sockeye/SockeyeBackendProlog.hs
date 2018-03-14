  {-
  SockeyeBackendProlog.hs: Backend for generating ECLiPSe-Prolog for Sockeye

  Part of Sockeye

  Copyright (c) 2018, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}


{-
  TODO: This currently works on a subset of the parser AST. Ideally, there would be
  a transformation first that:
  * Removes wildcards and replaces it with forall loops (introducing a new variable)
  * Expands natural expression into a seperate definition blocks (introducing new local
    variable for each block)
  * Everytime a range is encountered, it's passed to a natural limit/base range (no more bit ops)
  * Pushes the type of accepted/translated blocks own to the specific blocks, this should
    also merge the translte/convert types into one.
-}

module SockeyeBackendProlog
( compile, compileDirect ) where

import qualified Data.Map as Map
import Data.Char
import Data.List
import Text.Printf
import Control.Exception (throw, Exception)

import qualified SockeyeSymbolTable as ST
import qualified SockeyeAST as SAST
import qualified SockeyeParserAST as AST

data PrologBackendException
  = MultiDimensionalQuantifierException
  | NYIException String
  deriving(Show)

instance Exception PrologBackendException

{- The structure of the code generator should be very similar to the old Prolog Backend -}
compile :: ST.Sockeye -> SAST.Sockeye -> String
compile symTable ast = "Prolog backend not yet implemented"

compileDirect :: AST.Sockeye -> String
compileDirect = generate

{- Code Generator -}
class PrologGenerator a where
    generate :: a -> String

instance PrologGenerator AST.Sockeye where
  generate s = let
    files = map snd (Map.toList (AST.files s))
    in concat (map generate files)

instance PrologGenerator AST.SockeyeFile where
  generate f = concat (map generate (AST.modules f))

gen_node_param_list :: [AST.NodeDeclaration] -> [String]
gen_node_param_list ndl = map AST.nodeName ndl

gen_nat_param_list :: [AST.ModuleParameter] -> [String]
gen_nat_param_list pp = map AST.paramName pp

instance PrologGenerator AST.Module where
  generate m = let
    name = "add_" ++ AST.moduleName m
    mi = gen_module_info m
    p1 = gen_nat_param_list (AST.parameters m)
    bodyChecks = ["is_list(Id)"]
    nodeDecls = map gen_node_decls (AST.nodeDecls m)
    instDecls = map gen_inst_decls (AST.instDecls m)
    bodyDefs = concat $ map (gen_body_defs mi) (AST.definitions m)

    body = intercalate ",\n    " $ bodyChecks ++ nodeDecls ++ instDecls ++ bodyDefs
    in name ++ stringify (["Id"] ++ p1) ++ " :- \n    " ++ body ++ ".\n\n"
    where
      stringify [] = ""
      stringify pp = parens $ intercalate "," pp

-- Inside each function we add variable that contains
--  * nodeId
--  * params
--  * constants
-- This will return the name of these variables
local_nodeid_name :: String -> String
local_nodeid_name x = "ID_" ++ x

local_inst_name :: String -> String
local_inst_name x = "ID_" ++ x

-- Prefix tat as well?
local_param_name :: String -> String
local_param_name x = x

local_const_name :: String -> String
local_const_name x = "CONST_" ++ x

-- Generates something a la:
-- (ID_RAM) = (['ram', Id])
gen_inst_decls :: AST.InstanceDeclaration -> String
gen_inst_decls x =
  let
    var = local_nodeid_name $ AST.instName x
    decl = list_prepend (doublequotes $ AST.instName x) (local_param_name "Id")
    in var ++ " = " ++ decl

-- Generates something a la:
-- (ID_RAM, INKIND_RAM, OUTKIND_RAM) = (['ram' | Id], memory, memory)
gen_node_decls :: AST.NodeDeclaration -> String
gen_node_decls x =
  let
    var = local_nodeid_name $ AST.nodeName x
    decl_kind_in = generate (AST.originDomain (AST.nodeType x))
    decl_kind_out = generate (AST.targetDomain (AST.nodeType x))
    decl_id = list_prepend (doublequotes $ AST.nodeName x) (local_param_name "Id")
    decl_tup = tuple [decl_id, decl_kind_in, decl_kind_out]

    -- Build the variable list
    pf = AST.nodeName x
    var_tup = tuple [local_nodeid_name pf, "INKIND_" ++ pf, "OUTKIND_" ++ pf]
    in var_tup ++ " = " ++ decl_tup


-- This transformation is probably better to be in an AST transform
data MyAddressBlock = MyAddressBlock {
  domain :: !AST.Domain,
  addresses     :: AST.Address,
  properties    :: AST.PropertyExpr
  }

pack_address_block :: ModuleInfo -> AST.AddressBlock -> MyAddressBlock
pack_address_block mi ab = MyAddressBlock {
  domain = AST.Memory,
  addresses = SAST.addresses ab,
  properties = SAST.properties ab
}

data OneMapSpec = OneMapSpec
  {
  srcNode :: AST.UnqualifiedRef,
  srcAddr :: MyAddressBlock,
  targetNode :: AST.NodeReference,
  targetAddr :: MyAddressBlock
  }


map_spec_flatten :: ModuleInfo -> AST.Definition -> [OneMapSpec]
map_spec_flatten mi def = case def of
  (AST.Maps _ n maps) ->
    [OneMapSpec n (src_ab n $ AST.mapAddr map_spec) (AST.targetNode map_target) (dest_ab n $ AST.targetAddr map_target)
      | map_spec <- maps, map_target <- (AST.mapTargets map_spec)]
  _ -> []
  where
    nt uqr = (node_type mi) Map.! (AST.refName uqr)
    src_ab uqr ab = MyAddressBlock {
      domain = ST.originDomain $ nt uqr,
      properties = SAST.properties ab,
      addresses = SAST.addresses ab
    }
    dest_ab uqr ab = MyAddressBlock {
      domain = ST.targetDomain $ nt uqr,
      addresses = SAST.addresses ab,
      properties = SAST.properties ab
    }

data ModuleInfo = ModuleInfo
  {
    params :: [String],
    node_type :: Map.Map String ST.NodeType
  }

gen_module_info :: AST.Module -> ModuleInfo
gen_module_info x =
  ModuleInfo {
    params = ["Id"] ++ mparams ++ nodes ++ insts,
    node_type = Map.fromList [(AST.nodeName z, AST.nodeType $ z) | z <- AST.nodeDecls x]
  }
  where
    insts = [local_inst_name $ AST.instName d | d <- AST.instDecls x]
    mparams = (gen_nat_param_list $ AST.parameters x)
    nodes = [local_nodeid_name $ AST.nodeName d | d <- AST.nodeDecls x]

add_param :: ModuleInfo -> String -> ModuleInfo
add_param mi s = ModuleInfo { params = (params mi) ++ [s], node_type = node_type mi}

param_str :: ModuleInfo -> String
param_str mi = case params mi of
  [] -> ""
  li -> "," ++ intercalate "," [predicate "param" [p] | p <- li]


generate_conj :: ModuleInfo -> [AST.Definition] -> String
generate_conj mi li =
   intercalate ",\n" $ concat [gen_body_defs mi inn | inn <- li]

-- generate forall with a explicit variable name
forall_qual :: ModuleInfo -> String -> AST.NaturalSet -> [AST.Definition] -> String
forall_qual mi varName ns body =
  "(" ++
   predicate "iblock_values" [generate ns, it_list] ++ "," ++
   "(" ++
   predicate "foreach" [it_var, it_list]
   ++ param_str mi
   ++ " do \n" ++
   body_str ++ "\n))"
   where
     id_var = "ID_" ++ varName
     it_var = "IDT_" ++ varName
     it_list = "IDL_" ++ varName
     body_str = generate_conj (add_param mi it_var) body

forall_uqr :: ModuleInfo -> AST.UnqualifiedRef -> String -> String
forall_uqr mi ref body_str = case (AST.refIndex ref) of
  Nothing -> printf "(%s = %s, %s)" it_var id_var body_str
  Just ai -> "(" ++
                predicate "iblock_values" [generate ai, it_list] ++ "," ++
                "(" ++
                predicate "foreach" [it_var, it_list]
                ++ param_str mi
                ++ " do " ++
                itid_var ++ " = " ++ list_prepend it_var id_var ++ "," ++
                body_str ++ "))"
  where
    id_var = "ID_" ++ (AST.refName ref)
    it_var = "IDT_" ++ (AST.refName ref)
    itid_var = "IDI_" ++ (AST.refName ref)
    it_list = "IDL_" ++ (AST.refName ref)

gen_bind_defs :: String -> [AST.PortBinding] -> String
gen_bind_defs uql_var binds =
  let
      dest bind = generate $ AST.boundNode bind
      src bind = list_prepend (doublequotes $ AST.refName $ AST.boundPort $ bind) uql_var
      pb bind = assert $ predicate "node_overlay" [src bind, dest bind]
      preds = [pb bind | bind <- binds]
  in intercalate "," preds

gen_index :: AST.UnqualifiedRef -> String
gen_index uqr =
  case (AST.refIndex uqr) of
    Nothing -> local_nodeid_name $ AST.refName uqr
    Just ai -> list_prepend (gen_ai ai) (local_nodeid_name $ AST.refName uqr)
  where
    gen_ai (AST.ArrayIndex _ ws) =  list [gen_wildcard_simple w | w <- ws]
    gen_wildcard_simple (AST.ExplicitSet _ ns) = gen_natural_set ns
    gen_natural_set (ST.NaturalSet _ nrs) = gen_natural_ranges nrs
    gen_natural_ranges [nr] = gen_ns_simple nr
    gen_ns_simple (ST.SingletonRange _ base) = gen_exp_simple base
    gen_exp_simple (AST.Variable _ vn) = "IDT_" ++ vn
    gen_exp_simple (AST.Literal _ int) = show int



gen_body_defs :: ModuleInfo -> AST.Definition -> [String]
gen_body_defs mi x = case x of
  (AST.Accepts _ n accepts) -> [(assert $ predicate "node_accept" [generate n, generate (new_ab acc)])
    | acc <- accepts]
  (AST.Maps _ _ _) -> [(assert $ predicate "node_translate"
    [generate $ srcNode om, generate $ srcAddr om, generate $ targetNode om, generate $ targetAddr om])
    | om <- map_spec_flatten mi x]
  (AST.Overlays _ src dest) -> [assert $ predicate "node_overlay" [generate src, generate dest]]
  -- (AST.Instantiates _ i im args) -> [forall_uqr mi i (predicate ("add_" ++ im) ["IDT_" ++ (AST.refName i)])]
  (AST.Instantiates _ i im args) -> [ predicate ("add_" ++ im) [gen_index i] ]
  -- (AST.Binds _ i binds) -> [forall_uqr mi i $ gen_bind_defs ("IDT_" ++ (AST.refName i)) binds]
  (AST.Binds _ i binds) -> [gen_bind_defs (gen_index i) binds]
  (AST.Forall _ varName varRange body) -> [forall_qual mi varName varRange body]
  (AST.Converts _ _ _ ) -> throw $ NYIException "Converts"
  where
    new_ab ab = pack_address_block mi ab

instance PrologGenerator AST.UnqualifiedRef where
  generate uq = case (AST.refIndex uq) of
    Nothing -> local_nodeid_name $ AST.refName uq
    Just ai -> list_prepend (generate ai) (local_nodeid_name $ AST.refName uq)

instance PrologGenerator AST.WildcardSet where
  generate a = case a of
    AST.ExplicitSet _ ns -> generate ns
    AST.Wildcard _ -> "NYI!?"

instance PrologGenerator AST.ArrayIndex where
  generate (AST.ArrayIndex _ wcs) = brackets $ intercalate "," [generate x | x <- wcs]

instance PrologGenerator AST.MapSpec where
  generate ms = struct "map" [("src_block", generate (AST.mapAddr ms)),
    ("dests", list $ map generate (AST.mapTargets ms))]

instance PrologGenerator AST.MapTarget where
  generate mt = struct "dest" [("id", generate (AST.targetNode mt)),
    ("base", generate (AST.targetAddr mt))]

instance PrologGenerator AST.NodeReference where
  generate nr = case nr of
    AST.InternalNodeRef _ nn -> gen_index nn
    AST.InputPortRef _ inst node -> list_prepend (doublequotes $ AST.refName node) (gen_index inst)

instance PrologGenerator MyAddressBlock where
  -- TODO: add properties
  -- We have to generate something like this, probably involves an extra step in the AST.
  -- pred_99(propspec) :- member(prop1, propspec), member(prop2, propspec
  -- node_accept( ..., block{propspec: pred_99}).
  -- to check: B = block{propspec: PS}, call(PS, current_properties)
  generate ab = list $ [generate $ domain ab] ++ blocks
    where
      blocks = gen_a $ addresses ab
      gen_a (AST.Address _ ws) = map gen_ws ws
      gen_ws (AST.ExplicitSet _ ns) = generate ns
      gen_ws (AST.Wildcard _ ) = "NYI"

instance PrologGenerator AST.Domain where
  generate d = case d of
    AST.Memory -> atom "memory"
    AST.Interrupt -> atom "interrupt"
    AST.Power -> atom "power"
    AST.Clock -> atom "clock"

instance PrologGenerator AST.AddressBlock where
  -- TODO: add properties
  -- We have to generate something like this, probably involves an extra step in the AST.
  -- pred_99(propspec) :- member(prop1, propspec), member(prop2, propspec
  -- node_accept( ..., block{propspec: pred_99}).
  -- to check: B = block{propspec: PS}, call(PS, current_properties)
  generate ab = generate $ SAST.addresses ab


instance PrologGenerator AST.Address where
  generate a = case a of
    AST.Address _ ws -> tuple $ map generate ws

instance PrologGenerator AST.NaturalSet where
  generate a = case a of
    AST.NaturalSet _ nrs -> list $ map generate nrs

instance PrologGenerator AST.NaturalRange where
  generate nr = case nr of
    AST.SingletonRange _ b -> struct "block"
      [("base", generate b), ("limit", generate b)]
    AST.LimitRange _ b l -> struct "block"
      [("base", generate b), ("limit", generate l)]
    AST.BitsRange _ b bits -> "BITSRANGE NYI"
      -- struct "block" [("base", generate b), ("limit", show 666)]

instance PrologGenerator AST.NaturalExpr where
  generate nr = case nr of
    SAST.Constant _ v -> local_const_name v
    SAST.Variable _ v -> "IDT_" ++ v
    SAST.Parameter _ v -> local_param_name v
    SAST.Literal _ n -> show n
    SAST.Addition _ a b -> "(" ++ generate a ++ ")+(" ++ generate b ++ ")"
    SAST.Subtraction _ a b -> "(" ++ generate a ++ ")-(" ++ generate b ++ ")"
    SAST.Multiplication _ a b -> "(" ++ generate a ++ ")*(" ++ generate b ++ ")"
    SAST.Slice _ a bitrange -> "SLICE NYI"
    SAST.Concat _ a b -> "CONCAT NYI"


{- Helper functions -}
atom :: String -> String
atom "" = ""
atom name@(c:cs)
    | isLower c && allAlphaNum cs = name
    | otherwise = quotes name
    where
        allAlphaNum cs = foldl (\acc c -> isAlphaNum c && acc) True cs

predicate :: String -> [String] -> String
predicate name args = name ++ (parens $ intercalate "," args)

struct :: String -> [(String, String)] -> String
struct name fields = name ++ (braces $ intercalate "," (map toFieldString fields))
    where
        toFieldString (key, value) = key ++ ":" ++ value

tuple :: [String] -> String
tuple elems = parens $ intercalate "," elems

list :: [String] -> String
list elems = brackets $ intercalate "," elems

list_prepend :: String -> String -> String
list_prepend a li = brackets $ a ++ " | " ++ li

enclose :: String -> String -> String -> String
enclose start end string = start ++ string ++ end

parens :: String -> String
parens = enclose "(" ")"

brackets :: String -> String
brackets = enclose "[" "]"

braces :: String -> String
braces = enclose "{" "}"

quotes :: String -> String
quotes = enclose "'" "'"

doublequotes :: String -> String
doublequotes = enclose "\"" "\""


nat_range_from :: AST.NaturalRange -> String
nat_range_from nr = case nr of
  AST.SingletonRange _ b -> generate b
  AST.LimitRange _ b _ -> generate b
  AST.BitsRange _ _ _ -> "BitsRange NOT IMPLEMENTED"

nat_range_to :: AST.NaturalRange -> String
nat_range_to nr = case nr of
  AST.SingletonRange _ b -> generate b
  AST.LimitRange _ _ l -> generate l
  AST.BitsRange _ _ _ -> "BitsRange NOT IMPLEMENTED"

-- Params are variables passed into the for body
for_body_inner :: [String] -> String -> String -> (Int, AST.NaturalRange)  -> String
for_body_inner params itvar body itrange  =
  let
    itvar_local = itvar ++ (show $ fst itrange)
    from = nat_range_from $ (snd itrange)
    to = nat_range_to $ (snd itrange)
    for = printf "for(%s,%s,%s)" itvar_local from to :: String
    paramf x  = printf "param(%s)" x :: String
    header = intercalate "," ([for] ++ map paramf params)
    in printf "(%s \ndo\n %s \n)" header body

enumerate = zip [0..]

for_body :: [String] -> String -> AST.NaturalSet -> String -> String
for_body params itvar (AST.NaturalSet _ ranges) body =
  foldl fbi body (enumerate ranges)
  where
    fbi = for_body_inner params itvar


assert :: String -> String
assert x = "assert" ++ parens x
