{-
  SkateParser: The Skate file parser

  Part of Skate: a Schema specification languge

  Copyright (c) 2017, ETH Zurich.
  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Universit\"atstr. 6, CH-8092 Zurich. Attn: Systems Group.
-}
module SkateParser where

import Prelude
import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )
import Data.Char
import Numeric
import Data.List
import Text.Printf

import SkateTypes


{-
==============================================================================
= Token data types
==============================================================================
-}

{- import data type -}
data Import = Import String

{- Facts -}
data FactAttrib = FactAttrib String String TypeRef

{- Flags -}
data FlagDef = FlagDef String String Integer

{- Constants -}
data ConstantDef = ConstantDefInt String String Integer
                 | ConstantDefStr String String String

{- Enumerations -}
data EnumDef = EnumDef String String


{- declarations -}
data Declaration = Fact String String [ FactAttrib ]
                  | Flags String String Integer [ FlagDef ]
                  | Constants String String TypeRef [ ConstantDef ]
                  | Enumeration String String [ EnumDef ]
                  | Namespace String String [ Declaration ]
                  | Section String [ Declaration ]
                  | Text String

{--}
instance Show Declaration where
    show de@(Fact i d _) = "Fact '" ++ i ++ "'"
    show de@(Flags  i d _ _) = "Flags '" ++ i ++ "'"
    show de@(Enumeration  i d _) = "Enumeration '" ++ i ++ "'"
    show de@(Constants  i d _ _) = "Constants '" ++ i ++ "'"
    show de@(Namespace  i d _) = "Namespace '" ++ i ++ "'"
    show de@(Section  i _) = "Section '" ++ i ++ "'"
    show de@(Text i) = "Text Block"

{- the schema -}
data Schema = Schema String String [ Declaration ] [ String ]


{-
==============================================================================
= The Skate Token Parser
==============================================================================
-}

{- create the Skate Lexer -}
lexer = P.makeTokenParser (
    javaStyle  {
        {- list of reserved Names -}
        P.reservedNames = [
            "schema", "fact",
            "flags", "flag",
            "constants", "const",
            "enumeration", "enum",
            "text", "section"
        ],
        {- list of reserved operators -}
        P.reservedOpNames = ["*","/","+","-"],

        {- valid identifiers -}
        P.identStart = letter,
        P.identLetter = alphaNum,

        {- Skate is not case sensitive. -}
        P.caseSensitive = False,

        {- comment start and end -}
        P.commentStart = "/*",
        P.commentEnd = "*/",
        P.commentLine = "//",
        P.nestedComments = False
    })

{- Token definitions -}
whiteSpace = P.whiteSpace lexer
reserved   = P.reserved lexer
identifier = P.identifier lexer
stringLit  = P.stringLiteral lexer
comma      = P.comma lexer
commaSep   = P.commaSep lexer
commaSep1  = P.commaSep1 lexer
parens     = P.parens lexer
braces     = P.braces lexer
squares    = P.squares lexer
semiSep    = P.semiSep lexer
symbol     = P.symbol lexer
natural    = P.natural lexer
integer    = try ((P.lexeme lexer) binLiteral)
             <|> P.integer lexer

{- Parsing an integer number -}
binDigit = oneOf "01"
binLiteral = do {
    _ <- char '0';
    _ <- oneOf "bB";
    digits <- many1 binDigit;
    let n = foldl (\x d -> 2*x + (digitToInt d)) 0 digits
    ; seq n (return (fromIntegral n))
}


{------------------------------------------------------------------------------
- Parser start point
------------------------------------------------------------------------------}

{- parse the the Skate file -}
parse = do {
    whiteSpace;
    imps <- many importfacts;
    reserved "schema";
    name <- identifier;
    desc <- option name stringLit;
    decls <- braces (many1 schemadecl);
    _ <- symbol ";" <?> " ';' missing from end of " ++ name ++ " schema def";
    return (Schema name desc decls [i | (Import i) <- imps])
}


{------------------------------------------------------------------------------
- Token rules for the Schema
------------------------------------------------------------------------------}

schemadecl = factdecl      <|> constantdecl  <|> flagsdecl  <|> enumdecl <|>
             namespacedecl <|> sectiondecl   <|> textdecl


{------------------------------------------------------------------------------
- Imports
------------------------------------------------------------------------------}

importfacts = do {
    reserved "import";
    i <- identifier;
    _ <- symbol ";" <?> " ';' missing from end of " ++ i ++ " import";
    return (Import i)
}


{------------------------------------------------------------------------------
- Namespace
------------------------------------------------------------------------------}

namespacedecl = do {
    reserved "namespace";
    i <- identifier;
    d <- stringLit;
    decls <- braces (many1 schemadecl);
    _ <- symbol ";" <?> " ';' missing from end of " ++ i ++ " namespace";
    return (Namespace i d decls);
};


{------------------------------------------------------------------------------
- Facts
------------------------------------------------------------------------------}

factdecl = do {
    reserved "fact";
    i <- identifier;
    d <- stringLit;
    f <- braces (many1 factattrib);
    _ <- symbol ";" <?> " ';' missing from end of " ++ i ++ " fact";
    return (Fact i d f)
}

factattrib = do {
    t <- fieldType;
    i <- identifier;
    d <- stringLit;
    _ <- symbol ";" <?> " ';' missing from end of " ++ i ++ " fact attribute";
    return (FactAttrib i d t)
}


{------------------------------------------------------------------------------
- Flags
------------------------------------------------------------------------------}

flagsdecl = do {
    reserved "flags";
    i <- identifier;
    b <- integer;
    d <- stringLit;
    flagvals <- braces (many1 flagvals);
    _ <- symbol ";" <?> " ';' missing from end of " ++ i ++ " flags";
    return (Flags i d b flagvals)
}

{- identifier = value "opt desc"; -}
flagvals = do {
    p <- integer;
    i <- identifier;
    d <- stringLit;
    _ <- symbol ";" <?> " ';' missing from end of " ++ i ++ " flag val";
    return (FlagDef i d p)
};


{------------------------------------------------------------------------------
- Constants
------------------------------------------------------------------------------}


{- constants fname "desc" {[constantvals]}; -}
constantdecl = do {
    reserved "constants";
    i <- identifier;
    t <- fieldType;
    d <- stringLit;
    vals <- braces (many1 (constantvals t));
    _ <- symbol ";" <?> " ';' missing from end of " ++ i ++ " constants";
    return (Constants i d t vals)
}

constantvals (TBuiltIn String) = constantvalsstring
constantvals (TBuiltIn UInt8) =  constantvalsnum
constantvals (TBuiltIn UInt16) =  constantvalsnum
constantvals (TBuiltIn UInt32)  =  constantvalsnum
constantvals (TBuiltIn UInt64)  =  constantvalsnum
constantvals (TBuiltIn UIntPtr) =  constantvalsnum
constantvals (TBuiltIn Int8) =  constantvalsnum
constantvals (TBuiltIn Int16) =  constantvalsnum
constantvals (TBuiltIn Int32) =  constantvalsnum
constantvals (TBuiltIn Int64)  =  constantvalsnum
constantvals (TBuiltIn IntPtr) =  constantvalsnum
constantvals (TBuiltIn Size) =  constantvalsnum
constantvals (TBuiltIn Char) =  constantvalsnum
constantvals (TBuiltIn Bool) =  constantvalsnum
constantvals s = error $ "Invalid constant type " ++ (show s)

constantvalsnum = do {
    i <- identifier;
    _ <- symbol "=";
    v <- integer;
    d <- stringLit;
    _ <- symbol ";" <?> " ';' missing from end of " ++ i ++ " constant";
    return (ConstantDefInt i d v)
};

constantvalsstring = do {
    i <- identifier;
    _ <- symbol "=";
    v <- stringLit;
    d <- stringLit;
    _ <- symbol ";" <?> " ';' missing from end of " ++ i ++ " constant";
    return (ConstantDefStr i d v)
};


{------------------------------------------------------------------------------
- Enumerations
------------------------------------------------------------------------------}

enumdecl = do {
    reserved "enumeration";
    i <- identifier;
    d <- stringLit;
    enums <- braces (many1 enumdef);
     _ <- symbol ";" <?> " ';' missing from end of " ++ i ++ " enumeration";
    return (Enumeration i d enums)
}

enumdef = do {
    i <- identifier;
    d <- stringLit;
    _ <- symbol ";" <?> " ';' missing from end of " ++ i ++ " enum item";
    return (EnumDef i d)
};


{------------------------------------------------------------------------------
- Sections and Text blocks
------------------------------------------------------------------------------}

sectiondecl = do {
    reserved "section";
    i <- stringLit;
    decls <- braces (many1 schemadecl);
    _ <- symbol ";" <?> " ';' missing from end of " ++ i ++ " section";
    return (Section i decls);
};

textdecl = do {
    reserved "text";
    t <- braces (many1 stringLit);
    _ <- symbol ";" <?> " ';' missing from end of text block";
    return (Text (concat (intersperse " " t)) );
};


{------------------------------------------------------------------------------
Parsing Types
------------------------------------------------------------------------------}

fieldType = fieldTypeFactRef  <|> fieldTypeConstRef <|> fieldTypeEnumRef <|>
            fieldTypeFlagsRef <|> fieldTypeBuiltIn

fieldTypeBuiltIn = do {
    n <- identifier;
    return (TBuiltIn (findBuiltIntType n))
};

{- Parsing qualified identifiers -}
qualifiedPart = do {
    symbol ".";
    i <- identifier;
    return ("." ++ i);
}

qualifiedIdentiferLiteral = do {
    i <- identifier;
    ids <- many qualifiedPart;
    return (i ++ (concat ids))
}

fieldTypeFactRef  = do {
    reserved "fact";
    n <- qualifiedIdentiferLiteral;
    return (TFact n);
}

fieldTypeConstRef  = do {
    reserved "const";
    n <- identifier;
    return (TConstant n);
}

fieldTypeEnumRef  = do {
    reserved "enum";
    n <- identifier;
    return (TEnum n);
}

fieldTypeFlagsRef = do {
    reserved "flag";
    n <- identifier;
    return (TFlags n);
}
