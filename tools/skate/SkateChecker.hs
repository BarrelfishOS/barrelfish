{-
  SkateChecker: Checker for the AST

  Part of Skate: a Schema specification languge

  Copyright (c) 2017, ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}

module SkateChecker where

import Data.Bits
import System.FilePath.Posix
import Text.Printf

import SkateParser
import SkateSchema
import SkateTypes

import qualified SkateTypeTable as TT
import qualified SkateDeclarationTable as DT

{-
==============================================================================
= Public Functions
==============================================================================
-}

{- run various checks -}
run_all_checks :: String -> SchemaRecord -> IO String
run_all_checks inFile schemaRecord = do {
    printf "Running tests on  '%s'\n" inFile;
    _ <- checkFilename (skateSchemaGetAst schemaRecord) inFile;
    printf "Checking Namespaces in '%s'\n" inFile;
    _ <- checkDeclarations (namespaces schemaRecord) [];
    printf "Checking Flags in '%s'\n" inFile;
    _ <- checkDeclarations (flags schemaRecord) [];
    printf "Checking Constants in '%s'\n" inFile;
    _ <- checkDeclarations (constants schemaRecord) [];
    printf "Checking Enumerations in '%s'\n" inFile;
    _ <- checkDeclarations (enumerations schemaRecord) [];
    printf "Checking Facts in '%s'\n" inFile;
    _ <- checkDeclarations (facts schemaRecord) (allTypes schemaRecord) ;
    return ""
}


{-
==============================================================================
= Module Private Functions
==============================================================================
-}


{-----------------------------------------------------------------------------
- Checking Declarations
------------------------------------------------------------------------------}

fieldExists :: String -> String -> Bool
fieldExists a s = (a == s)

valueExists :: Integer -> Integer -> Bool
valueExists a s = (a == s)

checkUnique :: String -> String -> [String] -> IO ()
checkUnique fi i defs = do {
    if not (null (filter (fieldExists i) defs)) then do {
        ioError $ userError ("error: double definition of '" ++ i ++ "' in '" ++ fi ++ "'");
    } else do {
        return()
    }
}

checkUniqueVal :: String -> Integer -> [Integer] -> IO ()
checkUniqueVal fi i defs = do {
    if not (null (filter (valueExists i) defs)) then do {
        ioError $ userError ("error: double definition of value '" ++ show(i) ++ "' in '" ++ fi ++ "'");
    } else do {
        return()
    }
}


checkOneDeclaration :: Declaration -> [TT.TTEntry] -> IO ()
checkOneDeclaration de@(Fact i d a sp) ttbl = do {checkFactAttributes i a ttbl [];}
checkOneDeclaration de@(Flags i d w f sp) ttbl = do {checkFlagDefs i w f ttbl [] [];}
checkOneDeclaration de@(Constants i d t f sp) ttbl = do {checkConstantDefs i t f ttbl [];}
checkOneDeclaration de@(Enumeration i d f sp) ttbl = do {checkEnumDefs i f ttbl [];}
checkOneDeclaration de@(Namespace i d  _ sp) ttbl = do {return()}
checkOneDeclaration s _ = do {ioError $ userError ("internal error: encoutered unsupported declaration type." ++ (show s))}

checkDeclarations :: [Declaration] -> [TT.TTEntry] -> IO ()
checkDeclarations (xs:x) ttbl = do {checkOneDeclaration xs ttbl; checkDeclarations x ttbl}
checkDeclarations [] _ = do {return ()}

{-----------------------------------------------------------------------------
- Checking Facts
------------------------------------------------------------------------------}

fieldTypeLookup :: [TT.TTEntry] -> TT.RecType -> String -> String -> IO ()
fieldTypeLookup ttbl rt t s = do {
    if not (TT.exist ttbl rt t) then do {
        if not (TT.exist ttbl rt (make_qualified_identifer s t)) then do {
            ioError $ userError ("error: unknown type '" ++ (show rt) ++ " " ++ t ++ "'" );
        } else do {
            return ()
        }
    } else do {
        return ()
    }
}

fieldTypeCheck :: TypeRef -> [TT.TTEntry] -> IO ()
fieldTypeCheck tr@(TEnum t s) ttbl = fieldTypeLookup ttbl TT.TTEnum t s
fieldTypeCheck tr@(TConstant t s) ttbl =fieldTypeLookup ttbl TT.TTConstant t s
fieldTypeCheck tr@(TFact  t s) ttbl =fieldTypeLookup ttbl TT.TTFact t s
fieldTypeCheck tr@(TFlags t s) ttbl =fieldTypeLookup ttbl TT.TTFlags t s
fieldTypeCheck tr@(TBuiltIn t) ttbl = do {return ()}


checkFactAttributes :: String -> [FactAttrib] -> [TT.TTEntry] -> [String] -> IO ()
checkFactAttributes fi (xs@(FactAttrib i d t sp):x) ttbl attribs = do {
    checkUnique ("fact " ++ fi) i attribs;
    _ <- fieldTypeCheck t ttbl;
    checkFactAttributes fi x ttbl (attribs ++ [i]);
    return ()
}
checkFactAttributes _ [] _ _ = do {return ()}

{-----------------------------------------------------------------------------
- Checking Constants
------------------------------------------------------------------------------}

checkConstantDefsInt :: String -> TypeRef -> [ConstantDef] -> [TT.TTEntry] -> [String] -> IO ()
checkConstantDefsInt fi t (xs@(ConstantDefInt i d v sp):x) ttbl attribs = do {
    checkUnique ("constant " ++ fi) i attribs;
    checkConstantDefsInt fi t x ttbl (attribs ++ [i]);
}
checkConstantDefsInt fi _ (xs@(ConstantDefStr _ _ _ sp):x) _ _ = do {
    ioError $ userError ("error: constant type mismatch '" ++ fi ++ " expected Integer, was String");
}
checkConstantDefsInt fi _ [] _ _ = do {return ()}

checkConstantDefsString :: String -> TypeRef -> [ConstantDef] -> [TT.TTEntry] -> [String] -> IO ()
checkConstantDefsString fi t (xs@(ConstantDefStr i d v sp):x) ttbl attribs = do {
    checkUnique ("constant " ++ fi) i attribs;
    checkConstantDefsString fi t x ttbl (attribs ++ [i]);
}
checkConstantDefsString fi _ (xs@(ConstantDefInt _ _ _ _):x) _ _ = do {
    ioError $ userError ("error: constant type mismatch '" ++ fi ++ " expected Integer, was String");
}
checkConstantDefsString _ _ [] _ _ = do {return ()}


checkConstantDefs :: String -> TypeRef -> [ConstantDef] -> [TT.TTEntry] -> [String] -> IO ()
checkConstantDefs fi t (xs@(ConstantDefInt i d v sp):x) ttbl attribs = do {checkConstantDefsInt fi t (xs:x) ttbl attribs}
checkConstantDefs fi t (xs@(ConstantDefStr i d v sp):x) ttbl attribs = do {checkConstantDefsString fi t (xs:x) ttbl attribs}

{-----------------------------------------------------------------------------
- Checking Flags
------------------------------------------------------------------------------}

checkFlagDefs :: String -> Integer -> [FlagDef] -> [TT.TTEntry] -> [String] -> [Integer] -> IO ()
checkFlagDefs fi w (xs@(FlagDef i d t sp):x) ttbl defs bits = do {
    checkUnique ("flags " ++ fi) i defs;
    checkUniqueVal ("flags " ++ fi) t bits;
    if t < w then do {
        checkFlagDefs fi w x ttbl (defs ++ [i]) (bits ++ [t]);
    } else do {
        ioError $ userError ("error: bit position of flag '" ++ i ++ "' exceeds declared width of '" ++ fi ++ "");
    }
}
checkFlagDefs _ _ [] _ _ _ = do {return ()}


{-----------------------------------------------------------------------------
- Checking Enumerations
------------------------------------------------------------------------------}

checkEnumDefs :: String -> [EnumDef] -> [TT.TTEntry] -> [String] -> IO ()
checkEnumDefs fi (xs@(EnumDef i d sp):x) ttbl defs = do {
    checkUnique ("enumeration " ++ fi) i defs;
    checkEnumDefs fi x ttbl (defs ++ [i]);
}
checkEnumDefs _ [] _ _ = do {return ()}

{-----------------------------------------------------------------------------
- Checking Namespaces
------------------------------------------------------------------------------}



{-----------------------------------------------------------------------------
- Checking File name
------------------------------------------------------------------------------}

{- verifies that the filename matches with the query definition -}
checkFilename :: SkateParser.Schema -> String -> IO ()
checkFilename schema fname = do
    let
        SkateParser.Schema sname _ _ _ _ = schema
    if sname == takeBaseName fname
    then return ()
    else ioError $ userError (
        "Schema name '" ++ sname ++ "' has to equal filename in " ++ fname)





 --   case (Checks.check_all inFile schema) of
 --     Just errors ->
 --         do { (hPutStrLn stderr (unlines [ e ++ "\n"  | e <-errors]))
 --            ; System.Exit.exitWith (ExitFailure 1)
 --            }
 --     Nothing -> do { return "" }
