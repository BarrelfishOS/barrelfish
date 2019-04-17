{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

module SockeyeBackendLISA
    ( compileDirect
    )
where

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Ord
import Data.Aeson
import Control.Exception (throw, Exception)
import Debug.Trace
import GHC.Generics
import Text.Read

import qualified SockeyeSymbolTable as SST
import qualified SockeyeAST as SAST
import qualified SockeyeParserAST as SPAST

{- Auxiliary File -}

data AuxData = AuxData
    { connections :: [AuxConnection]
    , parameters :: [AuxParameter]
    }
    deriving (Generic, Show)

instance FromJSON AuxData

data AuxConnection = AuxConnection
    { moduleName :: String
    , source :: AuxConnectionDef
    , target :: AuxConnectionDef
    }
    deriving (Generic, Show)

instance FromJSON AuxConnection

instance LISAGenerator AuxConnection where
    generate (AuxConnection _ source target) = (generate source) ++ " => " ++ (generate target) ++ ";"

data AuxConnectionDef = AuxConnectionDef
    { name :: String
    , port :: String
    , address :: Maybe String
    }
    deriving (Generic, Show)

instance FromJSON AuxConnectionDef

instance LISAGenerator AuxConnectionDef where
    generate (AuxConnectionDef name port (Just address)) = name ++ "." ++ port ++ "[" ++ address ++ "]"
    generate (AuxConnectionDef name port Nothing) = name ++ "." ++ port

data AuxParameter = AuxParameter
    { moduleName :: String
    , component :: String
    , name :: String
    , translation :: Maybe String
    , value :: Maybe String
    }
    deriving (Generic, Show)

instance FromJSON AuxParameter

{- LISA Backend -}

class LISAGenerator a where
    generate :: a -> String

compileDirect :: SPAST.Sockeye -> Maybe AuxData -> String
compileDirect s aux = let
    -- sockeye = preprocess s
    sockeye = s
    files = map snd (Map.toList (SPAST.files sockeye))
    modules = map (\file -> generateModules (SPAST.modules file) aux) files
    in
        intercalate "\n" modules

{- Ports -}

data Ports = Ports
    { ports_auxData :: Maybe AuxData
    , ports_nodeDecls :: [SPAST.NodeDeclaration]
    }
    deriving (Show)

instance LISAGenerator Ports where
    generate (Ports _ nodeDecls) = let
        ports = map generate nodeDecls
        in
            intercalate "\n" ports

{- Resources -}

data Resources = Resources
    { res_auxData :: Maybe AuxData
    , res_parameters    :: [SPAST.ModuleParameter]
    , res_constants     :: [SPAST.NamedConstant]
    }
    deriving (Show)

instance LISAGenerator Resources where
    generate r = let
        parameters = map generate (res_parameters r)
        constants = map (\(SPAST.NamedConstant _ name value) -> "PARAMETER { type(int), default(" ++ (show value) ++ ") } " ++ name ++ ";") (res_constants r)
        in
            "resources {\n"
            ++ (intercalate "\n" $ parameters ++ constants)
            ++ "\n}"

{- Composition -}

data Composition = Composition
    { comp_moduleName :: String
    , comp_auxData :: Maybe AuxData
    , comp_modules :: [SPAST.Module]
    , comp_declarations :: [SPAST.NodeDeclaration]
    , comp_definitions :: [SPAST.Definition]
    }
    deriving (Show)

instance LISAGenerator Composition where
    generate (Composition moduleName auxData modules declarations definitions) = let
        composition = (filter (not . null) (map (\def -> generateInstantiation moduleName auxData def modules) definitions))

        filteredDefs = filter (\x -> (||) (isOverlays x) $ (||) (isAccepts x) (isMaps x)) definitions
        zipped = matchDefinitionsAndDeclarations declarations filteredDefs
        associatedComponents = nub $ concatMap (generateAssociatedComponents) zipped
        derivedComponents = associatedComponents
        in
            "composition {\n"
            ++ (intercalate "\n" $ composition ++ derivedComponents)
            ++ "\n}"

generateInstantiation :: String -> Maybe AuxData -> SPAST.Definition -> [SPAST.Module] -> String
generateInstantiation moduleName auxData (SPAST.Instantiates meta inst mod args) modules = let
    params = case find (((==) mod) . SPAST.moduleName) modules of
        Nothing -> error $ "Module parameters not found for " ++ mod ++ " : " ++ (show meta)
        Just x -> map SPAST.paramName (SPAST.parameters x)
    mapParamName _name = case auxData of
        Nothing -> _name
        Just AuxData{parameters=params} -> case find (\AuxParameter{moduleName=modName, name=__name, component=cName} -> (modName == moduleName) && (_name == __name) && (SAST.refName inst) == cName) params of
            Just AuxParameter{translation=(Just translation)} -> translation
            _ -> _name
    mapParamValue _name value = case auxData of
        Nothing -> value
        Just AuxData{parameters=params} -> case find (\AuxParameter{moduleName=modName, name=__name, component=cName} -> (modName == moduleName) && (_name == __name) && (SAST.refName inst) == cName) params of
            Just AuxParameter{value=(Just _value)} -> _value
            _ -> value
    in
        (generate inst) ++ ": " ++ mod ++ "(" ++ (intercalate ", " (map (\(name, arg) -> "\"" ++ (mapParamName name) ++ "\"=" ++ (mapParamValue name $ generate arg)) (zip params args))) ++ ");"
generateInstantiation _ _ _ _ = ""

generateAssociatedComponents :: (SPAST.NodeDeclaration, SPAST.Definition) -> [String]
generateAssociatedComponents ((SPAST.NodeDeclaration _ _ (SPAST.NodeType _ SPAST.Memory _ _ _) _ _), (SPAST.Maps _ node _)) = [(generate node) ++ "_DECODER: PVBusDecoder();"]
generateAssociatedComponents ((SPAST.NodeDeclaration _ _ (SPAST.NodeType _ SPAST.Memory (SST.TypeLiteral _ (SST.AddressType _ sets)) _ _) _ _), (SPAST.Accepts _ node _)) = [(generate node) ++ "_DECODER: PVBusDecoder();", (generate node) ++ "_MEMORY: RAMDevice(\"size\"=" ++ (snd $ getNaturalSetBounds $ head sets) ++ ");"]
generateAssociatedComponents ((SPAST.NodeDeclaration _ _ (SPAST.NodeType _ SPAST.Memory (SST.TypeName _ name) _ _) _ _), (SPAST.Accepts _ node _)) = [(generate node) ++ "_DECODER: PVBusDecoder();", (generate node) ++ "_MEMORY: RAMDevice(\"size\"=" ++ (name) ++ ");"]
generateAssociatedComponents ((SPAST.NodeDeclaration _ _ (SPAST.NodeType _ SPAST.Memory _ _ _) _ _), (SPAST.Overlays _ node _)) = [(generate node) ++ "_DECODER: PVBusDecoder();"]
generateAssociatedComponents _ = []

data Connection = Connection
    { conn_moduleName :: String
    , conn_auxData :: Maybe AuxData
    , conn_declarations :: [SPAST.NodeDeclaration]
    , conn_definitions :: [SPAST.Definition]
    }
    deriving (Show)

instance LISAGenerator Connection where
    generate (Connection moduleName auxData declarations definitions) = let
        decls = declarations
        sortedConnections = sortBy overlaysFirst (filter (not . isInstantiation) definitions)
        part = partition (\x -> (||) (isOverlays x) $ (||) (isAccepts x) (isMaps x)) sortedConnections
        derivedConnections = matchDefinitionsAndDeclarations decls (fst part)
        basicConnections = snd part
        connection = nub $ filter (not . null) $ (map generateDecoderConnection derivedConnections) ++ (map generateAssociatedConnection derivedConnections) ++ (map generate basicConnections)
        auxConnections = case auxData of
            Nothing -> []
            Just AuxData{connections=conns} -> map generate (filter (\AuxConnection{moduleName=_name} -> _name == moduleName) conns)
        in
            "connection {\n"
            ++ (intercalate "\n" $ connection ++ auxConnections)
            ++ "\n}"

{- Sockeye AST -}
instance LISAGenerator SAST.UnqualifiedRef where
    generate (SAST.UnqualifiedRef _ name index) = name ++ (generate index)

instance LISAGenerator SAST.NodeReference where
    generate (SAST.InternalNodeRef _ ref) = generate ref
    generate (SAST.InputPortRef _ inst node) = (generate inst) ++ "." ++ (generate node)

instance LISAGenerator SAST.ArrayIndex where
    generate (SAST.ArrayIndex _ set) = head (map generate set)

instance LISAGenerator SAST.Address where
    generate (SAST.Address _ set) = head (map generate set)

instance LISAGenerator SAST.AddressBlock where
    generate (SAST.AddressBlock _ addr _) = generate addr

instance LISAGenerator SAST.WildcardSet where
    generate (SAST.ExplicitSet _ set) = generate set
    generate (SAST.Wildcard _) = ""

instance LISAGenerator SAST.NaturalSet where
    generate set = let
        bounds = getNaturalSetBounds set
        in
            if ((fst bounds) == (snd bounds)) then "[" ++ (fst bounds) ++ "]" else "[" ++ (fst bounds) ++ ".." ++ (snd bounds) ++ "]"

instance LISAGenerator SAST.NaturalRange where
    generate range = let
        bounds = getNaturalRangeBounds range
        in
            "[" ++ (fst bounds) ++ ".." ++ (snd bounds) ++ "]"

instance LISAGenerator SAST.NaturalExpr where
    generate (SAST.Addition _ exprL exprR) = "(" ++ (generate exprL) ++ ") + (" ++ (generate exprR) ++ ")"
    generate (SAST.Subtraction _ exprL exprR) = "(" ++ (generate exprL) ++ ") - (" ++ (generate exprR) ++ ")"
    generate (SAST.Multiplication _ exprL exprR) = "(" ++ (generate exprL) ++ ") * (" ++ (generate exprR) ++ ")"
    generate (SAST.Slice meta expr range) = let
        value = generate expr
        lower = rangeLowerBound range
        upper = rangeUpperBound range
        shift = value ++ " >> " ++ lower
        mask = "(1ul << ((" ++ upper ++ ") - (" ++ lower ++ ") + 1)) - 1"
        in
            "(" ++ shift ++ ") & (" ++ mask ++ ")"
    generate (SAST.Concat _ exprL (SAST.Slice meta exprR range)) = let
        value = generate exprL
        concat = generate (SAST.Slice meta exprR range)
        lower = rangeLowerBound range
        upper = rangeUpperBound range
        shift = "((" ++ value ++ ") << (" ++ upper ++ ") - (" ++ lower ++ ") + 1))"
        in
            shift ++ " | " ++ concat
    generate (SAST.Concat meta _ _) = error ("Invalid concat expression: " ++ (show meta))
    generate (SAST.Parameter _ name) = name
    generate (SAST.Constant _ name) = name
    generate (SAST.Variable _ name) = name
    generate (SAST.Literal _ value) = show value

instance LISAGenerator SAST.PropertyExpr where
    generate (SAST.And meta _ _) = error ("Property expression not supported: " ++ (show meta))
    generate (SAST.Or meta _ _) = error ("Property expression not supported: " ++ (show meta))
    generate (SAST.Not meta _) = error ("Property expression not supported: " ++ (show meta))
    generate (SAST.Property meta _) = error ("Property expression not supported: " ++ (show meta))
    generate (SAST.True) = error ("True Property expression not supported")
    generate (SAST.False) = error ("False Property expression not supported")

{- Sockeye Symbol Table -}
instance LISAGenerator SST.Instance where
    generate _ = "NYI"

instance LISAGenerator SST.Node where
    generate _ = "NYI"

instance LISAGenerator SST.Domain where
    generate SST.Memory = "PVBus"
    generate SST.Interrupt = "Signal"
    generate SST.Power = error "Power domain is not supported"
    generate SST.Clock = "ClockSignal"

instance LISAGenerator SST.EdgeType where
    generate (SST.TypeLiteral _ address) = generate address
    generate (SST.TypeName _ name) = name

instance LISAGenerator SST.NamedConstant where
    generate _ = "NYI"

instance LISAGenerator SST.ArraySize where
    generate (SST.ArraySize meta sets) = let 
        bounds = getNaturalSetBounds $ head sets
        lower = fst bounds
        err = case readMaybe lower :: Maybe Integer of
            Nothing -> error ("Failed to parse lower bound as integer: " ++ (show meta))
            Just 0 -> ""
            Just _ -> error ("Lower bound of array size is not 0: " ++ (show meta))
        upper = snd bounds
        in
            if err == "" then (case readMaybe upper :: Maybe Integer of
                Nothing -> error ("Failed to parse upper bound as integer: " ++ (show meta))
                Just int -> "[" ++ (show $ int + 1) ++ "]") else ""


instance LISAGenerator SST.AddressType where
    generate (SST.AddressType _ sets) = generate $ head sets

{- Sockeye Parser AST -}
instance LISAGenerator SPAST.NamedConstant where
    generate _ = "NYI"

instance LISAGenerator SPAST.NamedType where
    generate _ = "NYI"

instance LISAGenerator SPAST.PortBinding where
    generate (SPAST.PortBinding _ port node) = "." ++ (generate port) ++ " => " ++ "self." ++ (generate node)

instance LISAGenerator SPAST.MapTarget where
    generate (SPAST.MapTarget _ node addr) = (generate node) ++ (generate addr)

instance LISAGenerator SPAST.MapSpec where
    generate (SPAST.MapSpec _ addrBlock targets) = intercalate "ERROR" (map (\target -> (generate addrBlock) ++ " => " ++ (generate target)) targets)

instance LISAGenerator SPAST.Definition where
    generate (SPAST.Accepts _ node addrBlocks) = intercalate "\n" (map (\addrBlock -> "self." ++ (generate node) ++ (generate addrBlock) ++ " => " ++ "self." ++ (generate node) ++ (generate addrBlock) ++ ";") addrBlocks)
    generate (SPAST.Maps _ node specs) = intercalate "\n" (map (\spec -> "self." ++ (generate node) ++ (generate spec) ++ ";") specs)
    generate (SPAST.Converts meta _ _) = error ("Converts not supported: " ++ (show meta))
    generate (SPAST.Overlays meta node (SAST.InternalNodeRef _ target)) = "self." ++ (generate node) ++ " => self." ++ (generate target) ++ ";"
    generate (SPAST.BlockOverlays meta _ _ _) = error ("BlockOverlays not supported: " ++ (show meta))
    generate (SPAST.Instantiates meta ref mod args) = error ("Invalid definition statement: " ++ (show meta))
    generate (SPAST.Binds _ node bindings) = intercalate "\n" (map (\binding -> (generate node) ++ (generate binding) ++ ";") bindings)
    generate (SPAST.Forall meta _ _ _) = error ("Forall not supported: " ++ (show meta))

generateAssociatedConnection :: (SPAST.NodeDeclaration, SPAST.Definition) -> String
generateAssociatedConnection ((SPAST.NodeDeclaration _ _ (SST.NodeType _ SST.Memory _ _ _) _ _), (SPAST.Maps _ node specs)) = intercalate "\n" (map (\spec -> (generate node) ++ "_DECODER.pvbus_m_range" ++ (generate spec) ++ ";") specs)
generateAssociatedConnection ((SPAST.NodeDeclaration _ _ (SST.NodeType _ SST.Memory _ _ _) _ _), (SPAST.Accepts _ node addrBlocks)) = intercalate "\n" (map (\addrBlock -> (generate node) ++ "_DECODER.pvbus_m_range" ++ (generate addrBlock) ++ " => " ++ (generate node) ++ "_MEMORY.pvbus" ++ (generate addrBlock) ++ ";") addrBlocks)
generateAssociatedConnection ((SPAST.NodeDeclaration _ _ (SST.NodeType _ SST.Memory edge _ _) _ _), (SPAST.Overlays _ node target)) = (generate node) ++ "_DECODER.pvbus_m_range" ++ (generate edge) ++ " => self." ++ (generate target) ++ ";"
generateAssociatedConnection (_, def) = generate def

generateDecoderConnection :: (SPAST.NodeDeclaration, SPAST.Definition) -> String
generateDecoderConnection ((SPAST.NodeDeclaration _ _ (SST.NodeType _ SST.Memory _ _ _) name _), _) = "self." ++ name ++ " => " ++ name ++ "_DECODER.pvbus_s;"
generateDecoderConnection _ = ""

instance LISAGenerator SPAST.NodeKind where
    generate SPAST.InputPort = "master"
    generate SPAST.InternalNode = "internal"
    generate SPAST.OutputPort = "slave"

instance LISAGenerator SPAST.NodeDeclaration where
    generate (SPAST.NodeDeclaration _ nodeKind (SPAST.NodeType _ domain _ _ _) name size) = let
        _kind = generate nodeKind
        protocol = generate domain
        in
            _kind ++ " port<" ++ protocol ++ "> " ++ name ++ (generate size) ++ " {}"

instance LISAGenerator SPAST.InstanceDeclaration where
    generate (SPAST.InstanceDeclaration meta _ _ _) = error ("Instances declarations are not supported: " ++ (show meta))

instance LISAGenerator SPAST.ModuleParameter where
    generate (SPAST.ModuleParameter _ name set) = "PARAMETER { type(int), min(" ++ (fst (getNaturalSetBounds set)) ++ "), max(" ++ (snd (getNaturalSetBounds set)) ++ ") } " ++ name ++ ";"

generateModules :: [SPAST.Module] -> Maybe AuxData -> String
generateModules ms aux = intercalate "\n" (map generateModule (filter (not . SPAST.moduleExtern) ms))
        where 
            generateModule m = let
                moduleName = SPAST.moduleName m
                ports = generate (Ports aux (SPAST.nodeDecls m))
                resources = generate (Resources aux (SPAST.parameters m) (SPAST.constants m))
                composition = generate (Composition moduleName aux ms (SPAST.nodeDecls m) (SPAST.definitions m))
                connection = generate (Connection moduleName aux (SPAST.nodeDecls m) (SPAST.definitions m))
                in
                    "component " ++ moduleName ++ " {\n\n"
                        ++ ports ++ "\n\n"
                        ++ resources ++ "\n\n"
                        ++ composition ++ "\n\n"
                        ++ connection ++ "\n\n"
                        ++ "}"

instance (LISAGenerator a) => LISAGenerator (Maybe a) where
    generate (Just some) = generate some
    generate Nothing = ""

{- Helper Functions-}

getNaturalSetBounds :: SAST.NaturalSet -> (String, String)
getNaturalSetBounds (SAST.NaturalSet _ (range:[])) = getNaturalRangeBounds range
getNaturalSetBounds (SAST.NaturalSet meta (range:ranges)) = let
    bounds = getNaturalRangeBounds range
    end = getNaturalSetBounds (SAST.NaturalSet meta ranges)
    in
        ("(" ++ (fst bounds) ++ " * " ++ (fst end) ++ ")", "(" ++ (snd bounds) ++ " * " ++ (snd end) ++ ")")

getNaturalRangeBounds :: SAST.NaturalRange -> (String, String)
getNaturalRangeBounds range = (rangeLowerBound range, rangeUpperBound range)

matchDefinitionsAndDeclarations :: [SPAST.NodeDeclaration] -> [SPAST.Definition] -> [(SPAST.NodeDeclaration, SPAST.Definition)]
matchDefinitionsAndDeclarations decls defs = zipped
    where
        findDecl d = find (\(SPAST.NodeDeclaration _ _ _ name _) -> name == (SAST.refName $ SPAST.node d)) decls 
        zipped = zip (map (\def -> if (isJust $ findDecl def) then (fromJust $ findDecl def) else (error ("No declaration found for definition: " ++ (show $ SPAST.defMeta def)))) defs) defs

isAccepts :: SPAST.Definition -> Bool
isAccepts (SPAST.Accepts _ _ _) = True
isAccepts _ = False

isMaps :: SPAST.Definition -> Bool
isMaps (SPAST.Maps _ _ _) = True
isMaps _ = False

isOverlays :: SPAST.Definition -> Bool
isOverlays (SPAST.Overlays _ _ _) = True
isOverlays _ = False

isInstantiation :: SPAST.Definition -> Bool
isInstantiation (SPAST.Instantiates _ _ _ _) = True
isInstantiation _ = False

rangeUpperBound :: SAST.NaturalRange -> String
rangeUpperBound (SAST.SingletonRange _ base) = (generate base)
rangeUpperBound (SAST.LimitRange _ _ limit) = (generate limit)
rangeUpperBound (SAST.BitsRange _ base bits) = (generate base) ++ " + ((1ul << " ++ (generate bits) ++ ") - 1)"

rangeLowerBound :: SAST.NaturalRange -> String
rangeLowerBound (SAST.SingletonRange _ base) = (generate base)
rangeLowerBound (SAST.LimitRange _ base _) = (generate base)
rangeLowerBound (SAST.BitsRange _ base _) = (generate base)

overlaysFirst :: SPAST.Definition -> SPAST.Definition -> Ordering
overlaysFirst SPAST.Overlays{} SPAST.Overlays{} = EQ
overlaysFirst SPAST.Overlays{} _ = LT
overlaysFirst _ SPAST.Overlays{} = GT
overlaysFirst _ _ = EQ
