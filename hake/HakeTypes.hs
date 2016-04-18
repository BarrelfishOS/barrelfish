--------------------------------------------------------------------------
-- Copyright (c) 2007-2010, ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:
-- ETH Zurich D-INFK, Universitätstasse 6, CH-8092 Zurich. Attn: Systems Group.
--
-- Basic Hake rule combinators
-- 
--------------------------------------------------------------------------

module HakeTypes where

import Data.Typeable

data TreeRef = SrcTree | BuildTree | InstallTree
             deriving (Show,Eq,Ord)

-- Note on Abs:
-- The first parameter is a rule referring to an absolute resource whereas the
-- second one is converted as all other rules. Dependencies and targets are
-- generated from the second one to.

data RuleToken = In     TreeRef String String -- Input to the computation
               | Dep    TreeRef String String -- Extra (implicit) dependency
               | NoDep  TreeRef String String -- File that's not a dependency
               | PreDep TreeRef String String -- One-time dependency
               | Out    String String         -- Output of the computation
               | Target String String         -- Target that's not involved
               | Str String                   -- String with trailing " "
               | NStr String                  -- Just a string               
               | ContStr Bool String String   -- Conditional string 
               | ErrorMsg String              -- Error message: $(error x)
               | NL                           -- New line
               | Abs RuleToken RuleToken      -- Absolute path rule token
                 deriving (Show,Eq,Ord)

-- Convert a rule into an absolute rule
makeAbs :: RuleToken -> RuleToken
makeAbs rule = Abs rule rule

data HRule = Rule [ RuleToken ]
           | Include RuleToken
           | Error String
           | Phony String Bool [ RuleToken ]
           | Rules [ HRule ]
             deriving (Show,Typeable)

frArch :: RuleToken -> String
frArch (In _ a _ ) = a
frArch (Out a _ ) = a
frArch (Dep _ a _ ) = a
frArch (NoDep _ a _ ) = a
frArch (PreDep _ a _ ) = a
frArch (Target a _ ) = a
frArch (Abs rule _) = frArch rule
frArch t = ""

frPath :: RuleToken -> String
frPath (In _ _ p) = p
frPath (Out _ p) = p
frPath (Dep _ _ p) = p
frPath (NoDep _ _ p) = p
frPath (PreDep _ _ p) = p
frPath (Target _ p) = p
frPath (Abs rule _) = frPath rule
frPath t = ""

frTree :: RuleToken -> TreeRef
frTree (In t _ _) = t
frTree (Dep t _ _) = t
frTree (NoDep t _ _) = t
frTree (PreDep t _ _) = t
frTree (Abs rule _) = frTree rule
frTree t = BuildTree

isFileRef :: RuleToken -> Bool
isFileRef (Str _ ) = False
isFileRef (NStr _ ) = False
isFileRef (ErrorMsg _) = False
isFileRef NL = False
isFileRef (Abs rule _) = isFileRef rule
isFileRef _ = True

isDependency :: RuleToken -> Bool
isDependency (In _ _ _) = True
isDependency (Dep _ _ _) = True
isDependency (Abs rule _) = isDependency rule
isDependency _ = False

isPredependency :: RuleToken -> Bool
isPredependency (PreDep _ _ _) = True
isPredependency (Abs rule _) = isPredependency rule
isPredependency _ = False

isOutput :: RuleToken -> Bool
isOutput (Out _ _) = True
isOutput (Target _ _) = True
isOutput (Abs rule _) = isOutput rule
isOutput _ = False

formatToken :: RuleToken -> String
formatToken (In _ a f) = f ++ " "
formatToken (Out a f) = f ++ " "
formatToken (Dep _ a f) = f ++ " "
formatToken (NoDep _ a f) = f ++ " "
formatToken (PreDep _ a f) = f ++ " "
formatToken (Target a f) = f ++ " "
formatToken (Str s) = s ++ " "
formatToken (NStr s) = s 
formatToken (Abs rule _) = formatToken rule
formatToken (ErrorMsg s) = "$(error " ++ s ++ ")"
formatToken (NL) = "\n\t"

-------------------------------------------------------------------------
--
-- Data type for default options to compilers, assemblers, dependency
-- generators, and the like
--
-------------------------------------------------------------------------

data Options = Options {
      optArch :: String,
      optArchFamily :: String,
      optFlags :: [RuleToken],
      optCxxFlags :: [RuleToken],
      optDefines :: [RuleToken],
      optIncludes :: [RuleToken],
      optDependencies :: [RuleToken],
      optLdFlags :: [RuleToken],
      optLdCxxFlags :: [RuleToken],
      optLibs :: [RuleToken],
      optCxxLibs :: [RuleToken],
      optInterconnectDrivers :: [String],
      optFlounderBackends :: [String],
      extraFlags :: [String],
      extraCxxFlags :: [String],
      extraDefines :: [String],
      extraIncludes :: [RuleToken],
      extraDependencies :: [RuleToken],
      extraLdFlags :: [RuleToken],
      optSuffix :: String
    }
      
