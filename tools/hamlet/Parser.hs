{- 
  Parser.hs: Parser for the Hamlet language 

  Copyright (c) 2009, ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}
  
module Parser where

import HamletAst

import qualified System
import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )
import Char
import Numeric
import Data.List
import Text.Printf

import System
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.IO
import System.FilePath.Posix


parseCaps filename = parseFromFile capsFile filename
                      
lexer = P.makeTokenParser $! (javaStyle
                              { P.reservedNames = [ "param", 
                                                    "define",
                                                    "cap",
                                                    "retype_to",
                                                    "retype_multi_to",
                                                    "eq",
                                                    "compare",
                                                    "with",
                                                    "is_always_copy",
                                                    "is_never_copy",
                                                    "mem_to_phys",
                                                    "sizeof"
                                                  ]
                              , P.reservedOpNames = [":", "+"]
                              , P.commentStart = "/*"
                              , P.commentEnd = "*/"
                              })

whiteSpace = P.whiteSpace lexer 
reserved   = P.reserved lexer
identifier = P.identifier lexer
integer    = P.integer lexer
stringLit  = P.stringLiteral lexer
comma      = P.comma lexer
commaSep   = P.commaSep lexer
commaSep1  = P.commaSep1 lexer
parens     = P.parens lexer
braces     = P.braces lexer
brackets    = P.brackets lexer
semiSep    = P.semiSep lexer
symbol     = P.symbol lexer

missingSep name = 
    do
      symbol ";" 
    <?> " ';' missing from end of " ++ name ++ " definition"

capsFile = 
    do 
      whiteSpace
      capWordSize <- paramCapWordSize
      defs <- many definesCst
      caps <- many1 capabilitiesDef
      return $! Capabilities capWordSize defs caps

paramCapWordSize =
    do
      reserved "param"
      reserved "cap_raw_word"
      size <- integer
      missingSep "cap_raw_word"
      return $! (fromInteger size :: Int)

definesCst =
    do
      reserved "define"
      name <- identifier
      val <- integer
      missingSep ("define " ++ name)
      return $! Define name (fromInteger val)
      

capabilitiesDef = 
    do 
      reserved "cap"
      name <- identifier
      (generalEq,
       fields,
       retypeCap,
       retypePath) <- braces $ capabilityDef name
      missingSep ("cap" ++ name)
      return $! Capability (CapName name)
                          generalEq fields 
                          retypeCap
                          retypePath

capabilityDef name = 
    do
      generalEq <- generalEqualityP name
      (retypeCap, retypePath) <- retypeInfo name
      fields <- many capField
      return $! (generalEq, 
                 fields, 
                 retypeCap,
                 retypePath)

retypeInfo name = 
    do
      do 
        reserved "retype_to"
        retPath <- braces $ commaSep1 $ retypePathP
        missingSep ("retype_to " ++ name)
        return $! ( Just Unique,
                    retPath )
      <|> do
        reserved "retype_multi_to"
        retPath <- braces $ commaSep1 $ retypePathP
        missingSep ("return_multi_to" ++ name)
        return $!( Just Multiple,
                   retPath )
      <|> do
        return $! ( Nothing, [] )
      
retypePathP = 
    do
      name <- identifier
      symbol ":"
      comparF <- braces compareFields
      return $! RetypePath (CapName name) comparF

compareFields =
    do
      try $ do 
        field1 <- fieldEq
        comma
        field2 <- fieldEq
        return $! Interval field1 field2
      <|> do
        field <- identifier
        return $! Address (NameField field)
      
fieldEq =
      do
        reserved "mem_to_phys"
        field <- parens identifier
        return $! MemToPhysOp field
      <|> do
        reserved "sizeof"
        field <- parens identifier
        return $! SizeOfOp field
      <|> try (do
         field1 <- identifier
         symbol "+"
         field2 <- identifier
         return $! Sum field1 field2)
      <|> do
        field <- identifier
        return $! LeqName  field

generalEqualityP name =
    do
      do
        reserved "is_always_copy"
        missingSep ("is_always_copy of " ++ name)
        return $! Just True
      <|> do
        reserved "is_never_copy"
        missingSep ("is_never_copy of " ++ name)
        return $! Just False
      <|> do 
        return $! Nothing

capField =
    do
      do
        reserved "eq"
        (typ, name) <- capTypeField
        return $! CapField DecideEquality
                          typ
                          name
      <|> do
        (typ, name) <- capTypeField
        return $! CapField IgnoredField
                          typ
                          name

capTypeField = 
    do
      typ <- do
               typ <- stringLit
               return $! typ
             <|> do
               typ <- identifier
               return $! typ
      name <- identifier
      missingSep ("field " ++ name)
      return $! ( (read typ :: Type), 
                  NameField name )
      

