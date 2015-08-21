{- 
 
   Parser.hs: Parser for the Flounder interface definition language
                      
   Part of Flounder: a strawman device definition DSL for Barrelfish
   
  Copyright (c) 2009, ETH Zurich.

  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
-}
  
module SockeyeParser where

import SockeyeSyntax

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

parse_intf predefDecls filename = parseFromFile (intffile predefDecls) filename
parse_include predefDecls filename = parseFromFile (includefile predefDecls) filename

lexer = P.makeTokenParser (javaStyle
                           { P.reservedNames = [ "schema", 
                                                 "fact",
                                                 "query",
                                                 "key",
                                                 "unique"
                                               ]
                           , P.reservedOpNames = ["*","/","+","-"]
                           , P.commentStart = "/*"
                           , P.commentEnd = "*/"
                           })

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

builtinTypes = map show [UInt8 ..] ++ ["int"] -- int is legacy -AB

-- identifyBuiltin :: [(String, Declaration)] -> String -> TypeRef
identifyBuiltin typeDcls typeName = 
    do {
      if typeName `elem` builtinTypes then
          return $ Builtin $ (read typeName::TypeBuiltin)
      else 
          case typeName `lookup` typeDcls of
            Just (Typedef (TAliasT new orig)) -> return $ TypeAlias new orig
            Just _ -> return $ TypeVar typeName 
            Nothing -> 
                do {
                ; pos <- getPosition
                -- This is ugly, I agree:
                ; return $ error ("Use of undeclared type '" ++ typeName ++ "' in "
                                  ++ show (sourceName pos) ++ " at l. "
                                  ++ show (sourceLine pos) ++ " col. "
                                  ++ show (sourceColumn pos))
                }
    }

intffile predefDecls = do { whiteSpace
             ; i <- pSchema predefDecls
             ; return i
              }

includefile predefDecls = do { whiteSpace
             ; typeDecls <- typeDeclaration predefDecls
             ; return typeDecls
              }

pSchema predefDecls = do { reserved "schema"
           ; name <- identifier 
           ; descr <- option name stringLit
           ; decls <- braces $ do {
                               ; typeDecls <- typeDeclaration predefDecls
                               ; factDecls <- many1 $ pFact typeDecls
                               ; queryDecls <- many $ pQuery typeDecls
                               ; return ((map snd typeDecls) ++ factDecls ++ queryDecls)
                               }
           ; symbol ";" <?> " ';' missing from end of " ++ name ++ " interface specification"
           ;  return (Schema name descr decls)
           }


typeDeclaration typeDcls = do {
                           ; decl <- try (do {
                                           ; x <- transparentAlias 
                                           ; return $ Just x
                                           })
                                     <|> try (do {
                                               ; x <- typedefinition typeDcls
                                               ; return $ Just x
                                               })
                                    <|> return Nothing
                           ; case decl of 
                               Nothing -> return typeDcls
                               Just x -> typeDeclaration (x : typeDcls)
                           }       

pFact typeDcls = do { def <- pFct typeDcls
                    ; return $ Factdef def
                    }

pQuery typeDcls = do {def <- pQry typeDcls
                   ; return $ Querydef def
                   }                   

pQry typeDcls = do { reserved "query"
                   ; i <- identifier
                   ; d <- option i stringLit
                   ; attrib <- braces $ commaSep (queryParams typeDcls)
                   ; symbol ";"
                   ; return $ Query i d attrib
                   }


pFct typeDcls = do { reserved "fact"
                   ; i <- identifier
                   ; d <- option i stringLit
                   ; attrib <- braces $ do { attrDecls <- many $ factAttribs typeDcls
                                           ; return attrDecls
                                           }
                   ; symbol ";"
                   ; return $ Fact i d attrib
                   }


factAttribs typeDecls = do { b <-factAttribType typeDecls
                           ; i <- identifier
                           ; d <- option i stringLit
                           ; symbol ";"
                           ; return (FAttrib b (Name i) d)
                           }
 
--- XXX: verify that the fact is already defined
factAttribTypeRef typeDecls = do {  reserved "fact"
                                 ; t <- identifier 
                               --  ; b <- identifyBuiltin typeDecls t
                                 ; return $ FactType t
                              --   ; return b
                                 }

factAttribTypeBultIn typeDecls = do { t <- identifier 
                                    ; b <- identifyBuiltin typeDecls t
                                    ; return b
                                    }


factAttribType typeDcls = try (factAttribTypeRef typeDcls)
                        <|> (factAttribTypeBultIn typeDcls)




queryParams typeDecls = do { i <- identifier
                           ; symbol "="
                           ; v <- identifier
                           ; symbol ";"
                           ; return $ QParam (Name i) i 
                           }


transparentAlias = do { whiteSpace 
                      ; reserved "alias"
                      ; newType <- identifier
                      ; originType <- identifier
                      ; symbol ";"
                      ; return (newType, Typedef $ TAliasT newType 
                                                           (read originType::TypeBuiltin))
                      }

typedefinition typeDcls = do { whiteSpace
                             ; reserved "typedef"
                             ; (name, typeDef) <- typedef_body typeDcls
                             ; symbol ";"
                             ; return (name, Typedef typeDef)
                             }

typedef_body typeDcls = try enum_typedef
                        <|> (alias_typedef typeDcls)
 

enum_typedef = do { reserved "enum"
                  ; v <- braces $ commaSep1 identifier
                  ; i <- identifier
                  ; return (i, (TEnum i v))
                  }

alias_typedef typeDcls = do { t <- identifier
                            ; i <- identifier
                            ; b <- identifyBuiltin typeDcls t
                            ; return (i, (TAlias i b))
                            }

integer = P.integer lexer
