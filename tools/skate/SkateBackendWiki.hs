{-
  SkateBackendWiki: Backend to generate a Wiki documentation

  Part of Skate: a Schema specification languge

  Copyright (c) 2017, ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}

module SkateBackendWiki   where

import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Clock.POSIX

import Data.List
import Data.Char

import Text.ParserCombinators.Parsec.Pos


import  SkateParser
import SkateTypes
import qualified AbsSyntaxWiki as W
import SkateTypes
import SkateSchema


{- starts the compilation process of the schema -}
compile :: String -> String -> SchemaRecord -> String
compile infile outfile sr =
    h ++ i ++ W.lineBreak ++ b ++ W.lineBreak ++ f
    where
        Schema sname sdesc decls imps _ = (schema sr)
        h = wikiHeader sname sdesc infile
        i = wikiImports imps
        b = wikiBody decls sname
        f = wikiFooter ""


{- generate the header used in the Wiki syntax -}
wikiHeader :: String -> String -> String -> String
wikiHeader sname desc infile = W.title ("Schema: " ++ desc)
    ++ W.textit "This document has been generated using Skate." ++ W.lineBreak
    ++ W.textit "Input File: " ++ W.inlinecode infile ++ W.lineBreak
    ++ W.textit "Schema Identifier: " ++ W.inlinecode sname ++ W.lineBreak
    ++ W.tableOfContent



wikiImports :: [String] -> String
wikiImports imps = h ++ i
    where
        h = W.heading "Imports"
        i = (if imps == [] then "No imports." else (W.unOrderedList 1 imps))


wikiBody :: [Declaration] -> String-> String
wikiBody decls sname = heading ++ concat declstr
    where
        heading =  W.hline
        declstr = [wikiPrintDecl d 2 sname| d <- decls]


wikiPrintDecl :: Declaration -> Int -> String -> String
wikiPrintDecl d@(Fact fn fd attr sp) l prefix= (wikiPrintFact fn fd attr prefix l)
wikiPrintDecl d@(Flags f fd w defs sp) l prefix = (wikiPrintFlags f w fd defs) prefix l
wikiPrintDecl d@(Constants n cd t defs sp) l prefix = wikiPrintConstants n t cd defs prefix l
wikiPrintDecl d@(Enumeration n ed defs sp) l prefix = wikiPrintEnum n ed defs prefix l
wikiPrintDecl d@(Namespace n nd defs sp) l prefix = wikiPrintNameSpace n nd defs l prefix
wikiPrintDecl d@(Section n defs sp) l prefix = wikiPrintSection n defs l prefix
wikiPrintDecl d@(Text t sp) l prefix = wikiPrintText t


{----------------------------------------------------------------------------}

wikiPrintFact :: String -> String -> [FactAttrib] -> String -> Int -> String
wikiPrintFact n d attrib prefix l = title ++ W.newLine
    ++ "Fact Name: " ++ (W.inlinecode name) ++ W.lineBreak
    ++ "Prolog: " ++ W.newLine  ++ (W.code prolog) ++ W.lineBreak
    ++ "Fields: " ++ W.newLine
    ++ (W.tableHeading ["Name", "Type", "Description"]) ++ (concat tableRows)
    ++ W.lineBreak
    where
        title = W.headingGeneric ("Fact: " ++ d) l
        tableRows = [W.tableRow (wikiPrintFactAttrib a) | a <- attrib]
        name = makeDeclName prefix n
        prologfields = (intersperse "," [wikiPrintFactFieldNames a | a <- attrib])
        prolog = name ++ "(" ++ (concat prologfields) ++ ")"

wikiPrintFactAttrib :: FactAttrib -> [String]
wikiPrintFactAttrib fa@(FactAttrib n d t _) = [n, (show t), d]

wikiPrintFactFieldNames :: FactAttrib -> String
wikiPrintFactFieldNames fa@(FactAttrib n _ _ _) = n


{----------------------------------------------------------------------------}


wikiPrintFlags :: String -> Integer -> String -> [FlagDef] -> String -> Int -> String
wikiPrintFlags n w d f prefix l = title ++ W.newLine
    ++ "Flags Name: " ++ name ++ W.lineBreak
    ++ "Prolog: " ++ W.newLine ++ (W.code (concat prolog)) ++ W.newLine
    ++ "Flags: " ++ W.lineBreak
    ++ (W.tableHeading ["Flag", "Value", "Description" ]) ++ (concat tableRows)
    ++ W.lineBreak
    where
        title = W.headingGeneric ("Flags: " ++ d) l
        flags = [wikiPrintFlagDefs fd n prefix | fd <- f]
        tableRows = [W.tableRow [fn, fv, fd] | (fn, fd, fv) <- flags]
        prolog = []
        name = makeDeclName prefix n

wikiPrintFlagDefs :: FlagDef -> String -> String -> (String, String, String)
wikiPrintFlagDefs fd@(FlagDef n d v _) flag prefix = (fname, d, fval)
    where
        fname = makeFlagName prefix flag n
        fval = (show (v))



{----------------------------------------------------------------------------}


wikiPrintConstants :: String -> TypeRef -> String -> [ ConstantDef ] -> String -> Int-> String
wikiPrintConstants n t d defs prefix l =  title
    ++ W.lineBreak
    where
        title = W.headingGeneric ("Constants: " ++ d) l


{----------------------------------------------------------------------------}


wikiPrintEnum :: String -> String ->  [ EnumDef ] -> String -> Int -> String
wikiPrintEnum n d defs prefix l = title
    ++ W.lineBreak
    where
        title = W.headingGeneric ("Enumeration: " ++ d) l


{----------------------------------------------------------------------------}


wikiPrintNameSpace :: String -> String ->  [ Declaration ] -> Int -> String -> String
wikiPrintNameSpace n d decls l prefix =  h ++ W.lineBreak
    ++ "Namespace identifier: " ++ W.inlinecode (makeNameSpacePrefix prefix n)
    ++ W.lineBreak
    ++ (concat ns)
    ++ W.lineBreak
    ++ "End of namespace " ++ n
    ++ W.newLine ++ W.hline
    ++ W.lineBreak
    where
        h = W.headingGeneric ("Namespace: " ++ d) l
        newprefix = makeNameSpacePrefix prefix n
        ns = [wikiPrintDecl d (l + 1) newprefix | d <- decls]


{----------------------------------------------------------------------------}


wikiPrintSection :: String -> [ Declaration ] -> Int -> String -> String
wikiPrintSection  n decls l prefix = h ++ (concat subsection) ++ W.newLine
    ++ W.lineBreak
    where
        h = W.headingGeneric n l
        subsection = [wikiPrintDecl d (l + 1) prefix | d <- decls]


{----------------------------------------------------------------------------}


wikiPrintText :: String -> String
wikiPrintText t = t ++ W.lineBreak


{----------------------------------------------------------------------------}


wikiPrintTypedef :: TypeBuiltIn -> String -> String -> String
wikiPrintTypedef t s prefix = s ++ W.lineBreak


{----------------------------------------------------------------------------}


wikiFooter :: String -> String
wikiFooter intf = "footer"


wikiFootNote :: String
wikiFootNote = W.footnote "This has been generated by Skate v. xx.xx on date."


{----------------------------------------------------------------------------}


{-Generic -}

makeNameSpacePrefix :: String -> String -> String
makeNameSpacePrefix prefix n = prefix ++ "_" ++ n

makeDeclName :: String -> String -> String
makeDeclName prefix n = prefix ++ "__" ++ n

makeFlagName :: String -> String -> String -> String
makeFlagName prefix flag name = map toUpper fname
    where
        fname = prefix ++ "_" ++ flag ++ "_" ++ name
