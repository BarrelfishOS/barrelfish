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



-- The Sockeye Lexer
lexer = P.makeTokenParser ( 
    javaStyle  { 
        P.reservedNames = [ "fact", "query", "key", "unique"], 
        P.reservedOpNames = ["*","/","+","-"], 
        P.commentStart = "/*", 
        P.commentEnd = "*/"
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
                  | Flags String Integer String [ FlagDef ]
                  | Constants String TypeRef String [ ConstantDef ]
                  | Enumeration String String [ EnumDef ]
                  | Namespace String String [ Declaration ]
                  | Section String [ Declaration ]
                  | Text String 
                  | TypeDef TypeBuiltIn String 
                  -- TODO: add the queries
                  
{- the schema -}
data Schema = Schema String String [ Declaration ] [ String ]


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
- Token rules
------------------------------------------------------------------------------}


schemadecl = factdecl <|> constantdecl  <|> flagsdecl    <|> enumdecl
                      <|> namespacedecl <|>  sectiondecl <|> textdecl  
                      <|> typedef



{------------------------------------------------------------------------------
- Imports
------------------------------------------------------------------------------}


{- import other_schema; -}
importfacts = do {
    reserved "import";
    i <- identifier;
    _ <- symbol ";";
    return (Import i)
}


{------------------------------------------------------------------------------
- Namespace
------------------------------------------------------------------------------}


{- namespace name "desc" {}; -}
namespacedecl = do {
    reserved "namespace";
    n <- identifier;
    d <- option n stringLit;
    decls <- braces (many1 schemadecl);
    _ <- symbol ";" ;
    return (Namespace n d decls);
};



{------------------------------------------------------------------------------
- Type Definitions
------------------------------------------------------------------------------}

{- typedef -}
typedef = do {
    reserved "typedef" ;
    t <- identifier;
    n <- identifier;
    _ <- symbol ";";
    return (TypeDef (findBuiltIntType t) n)  
}



{------------------------------------------------------------------------------
- Facts
------------------------------------------------------------------------------}


{- fact fname "desc" {factattrib}; -}
factdecl = do {
    reserved "fact";
    i <- identifier;
    d <- option i stringLit;
    f <- braces (many1 factattrib);
    _ <- symbol ";";
    return (Fact i d f)
}

factattrib = do {
    t <- fieldType;
    i <- identifier;
    d <- option i stringLit;
    _ <- symbol ";";
    return (FactAttrib i d t)
}


{------------------------------------------------------------------------------
- Flags
------------------------------------------------------------------------------}


{- flags fname width "desc" {[flagvals]}; -}
flagsdecl = do {
    reserved "flags";
    i <- identifier; 
    b <- integer;
    d <- stringLit;
    flagvals <- braces (many1 flagvals);
    _ <- symbol ";" ;
    return (Flags i b d flagvals)
}

{- identifier = value "opt desc"; -}
flagvals = do {
    i <- identifier;
    _ <- symbol "=";
    v <- integer;
    d <- stringLit;
    _ <- symbol ";";
    return (FlagDef i d v)
};


{------------------------------------------------------------------------------
- Constants 
------------------------------------------------------------------------------}


{- constants fname "desc" {[constantvals]}; -}
constantdecl = do {
    reserved "constants";
    name <- identifier;
    t <- fieldType;
    desc <- stringLit ;
    vals <- braces (many1 (constantvals t));
    _ <- symbol ";" ;
    return (Constants name t desc vals)
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
    _ <- symbol ";";
    return (ConstantDefInt i d 1)
};

constantvalsstring = do {
    i <- identifier;
    _ <- symbol "=";
    v <- stringLit;
    d <- stringLit;
    _ <- symbol ";";
    return (ConstantDefStr i d v)
};


{------------------------------------------------------------------------------
- Enumerations
------------------------------------------------------------------------------}


{- fact fname "desc" {}; -}
enumdecl = do {
    reserved "enumeration";
    n <- identifier;
    d <- option n stringLit;
    enums <- braces (many1 enumdef);
     _ <- symbol ";";
    return (Enumeration n d enums)
}

enumdef = do {
    n <- identifier;
    d <- option n stringLit;
    _ <- symbol ";";
    return (EnumDef n d)
};


{------------------------------------------------------------------------------
- Sections and Text blocks
------------------------------------------------------------------------------}


{- section name "desc" {}; -}
sectiondecl = do {  
    reserved "section";
    n <- stringLit;
    decls <- braces (many1 schemadecl);
    _ <- symbol ";" ;
    return (Section n decls);
};

{- text {}; -}
textdecl = do {
    reserved "text";
    t <- stringLit;
    return (Text t);
};


{------------------------------------------------------------------------------
Parsing Types
------------------------------------------------------------------------------}


fieldType = fieldTypeFactRef <|> fieldTypeConstRef <|> fieldTypeEnumRef  
                             <|> fieldTypeFlagsRef <|> fieldTypeBuiltIn

fieldTypeBuiltIn = do {
    n <- identifier;
    return (TBuiltIn (findBuiltIntType n))
};

fieldTypeFactRef  = do {
    reserved "fact";
    n <- identifier;
    return (TFact n);
}

fieldTypeConstRef  = do { 
    reserved "constant";
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



{-
===============================================================================
-}


{-
builtinTypes = map show [UInt8 ..] 

fieldType = try (fieldTypeBuiltIn [])
           <|> (fieldTypeRef [])

fieldTypeRef typeDecls = do {
    t <- identifier;
    --  ; b <- identifyBuiltin typeDecls t
    return $ FactType t
}

fieldTypeBuiltIn typeDecls = do { 
    t <- identifier;
    b <- identifyBuiltin typeDecls t; 
    return b
}

identifyBuiltin typeDcls typeName = do {
    if typeName `elem` builtinTypes then
        return $ Builtin $ (read typeName::TypeBuiltin)
    else
        return $ Builtin UInt8
}
    --      case typeName `lookup` typeDcls of
    --        Just (Typedef (TAlias new orig)) -> return $ TypeAlias new orig
    -}