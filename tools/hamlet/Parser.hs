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
import Text.ParserCombinators.Parsec.Char as C
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )
import Char
import Numeric
import Data.List
import Data.Maybe
import Text.Printf

import System
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.IO
import System.FilePath.Posix


parseCaps filename = parseFromFile capsFile filename

lexer = P.makeTokenParser $! (javaStyle
                              { P.reservedNames = [ "is_always_copy"
                                                  , "is_never_copy"
                                                  , "from"
                                                  ]
                              , P.reservedOpNames = ["+"]
                              , P.identLetter = C.alphaNum <|> C.char '_'
                              })

whiteSpace = P.whiteSpace lexer 
reserved   = P.reserved lexer
identifier = P.identifier lexer
reservedOp = P.reservedOp lexer
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
    symbol ";" <?> " ';' missing from end of " ++ name

capsFile = 
    do 
      whiteSpace
      defs <- many definesCst
      caps <- many1 capabilitiesDef
      return $! Capabilities defs caps

-- parse global definition
definesCst =
    do
      reserved "define"
      name <- identifier
      val <- integer
      missingSep (name ++ " define")
      return $! Define name (fromInteger val)

-- parse a single capability definition
capabilitiesDef = 
    do 
      reserved "cap"
      name <- identifier
      geq <- generalEqualityP name
      from <- if isNothing geq then fromP name else return Nothing
      fromSelf <- if isNothing geq then fromSelfP name else return False
      (fields, rangeExpr, eqFields) <- braces $ capabilityDef name
      missingSep ("cap " ++ name ++ " definition")
      return $! Capability (CapName name) geq from fromSelf fields rangeExpr eqFields

-- parse optional general equality (always/never copy)
generalEqualityP name = do
    (reserved "is_always_copy" >> (return $ Just True))
    <|> (reserved "is_never_copy" >> (return $ Just False))
    <|> (return Nothing)

-- parse optional "from <base cap name>"
fromP name = withFromP <|> return Nothing
    where withFromP = do
            reserved "from"
            from <- identifier
            return $ Just $ CapName from

-- parse optional "from_self"
fromSelfP name = (reserved "from_self" >> (return True)) <|> (return False)

-- parse the body of a capability definition
capabilityDef name = do

    -- read sequence of field, address, size, and equality definitions
    annotatedFields <- many $ capFieldOrExpr name
    (fields, addresses, sizes, eqExprs) <- return $ unzipDefs annotatedFields

    -- lengths to check
    (numAddrs, numSizes) <- return (length addresses, length sizes)

    -- check that there are either 0 or 1 of both address and size definitions
    if numAddrs > 1
       then unexpected ("multiple address definitions for cap " ++ name)
       else return ()
    if numSizes > 1
       then unexpected ("multiple size definitions for cap " ++ name)
       else return ()
    if numAddrs > 0 && numSizes < 1
       then unexpected ("have address definition but no size definition for cap " ++ name)
       else return ()
    if numAddrs < 1 && numSizes > 0
       then unexpected ("have size definition but no address definition for cap " ++ name)
       else return ()

    -- merged address and size expressions if present
    return $ let rangeExpr = if null addresses then Nothing else Just (head addresses, head sizes)
                 in (fields, rangeExpr, eqExprs)

  where
    -- un-maybe lists from capfields parsing
    unzipDefs annotatedFields = (fs, as, ss, es)
      where fs = catMaybes afs
            as = catMaybes aas
            ss = catMaybes ass
            es = catMaybes ess
            (afs, aas, ass, ess) = unzip4 annotatedFields

capFieldOrExpr name = (reserved "address" >> (addrField <|> addrExpr))
                      <|>
                      (((reserved "size" >> (return False)) <|>
                        (reserved "size_bits" >> (return True)))
                       >>= (\isBits -> sizeField isBits <|> sizeExpr isBits))
                      <|>
                      (reserved "eq" >> eqField)
                      <|>
                      regField
    where
      addrField = do
        -- handle field marked as address
        field <- capTypeField
        return $
          let expr = AddressExpr $ NameExpr $ fieldName field
          in (Just field, Just expr, Nothing, Nothing)
      addrExpr = do
        -- handle address expression
        addrExpr <- braces addressExprP
        missingSep ("address definition for " ++ name)
        return (Nothing, Just addrExpr, Nothing, Nothing)
      sizeField isBits = do
        -- handle field marked as size or size_bits
        field <- capTypeField
        return $
          let mkSize = if isBits then SizeBitsExpr else SizeExpr
              expr = mkSize $ NameExpr $ fieldName field
          in (Just field, Nothing, Just expr, Nothing)
      eqField = do
        -- handle field marked as eq
        field <- capTypeField
        return (Just field, Nothing, Nothing, Just $ NameField $ fieldName field)
      sizeExpr isBits = do
        -- handle size expression
        expr <- braces (if isBits then sizeBitsExprP else sizeExprP)
        missingSep ("size definition for " ++ name)
        return (Nothing, Nothing, Just expr, Nothing)
      regField = do
        -- handle regular field
        field <- capTypeField
        return (Just field, Nothing, Nothing, Nothing)
      fieldName (CapField _ (NameField n)) = n

-- parse cap field (name, type, semicolon)
capTypeField = do
    typ <- stringLit <|> identifier
    name <- identifier
    missingSep ("field " ++ name)
    return $ CapField (read typ) (NameField name)

-- parse address expression
addressExprP = (reserved "mem_to_phys" >> parens exprP >>= (return . MemToPhysOp))
               <|> (exprP >>= (return . AddressExpr))
-- parse size expression
sizeExprP = (reserved "sizeof" >> identifier >>= (return . SizeOfOp))
            <|> (exprP >>= (return . SizeExpr))
-- parse size_bits expression
sizeBitsExprP = exprP >>= (return . SizeBitsExpr)
-- parse subexpression for the above
exprP = do
    left <- identifier
    (do reservedOp "+"
        right <- identifier
        return $ AddExpr left right
     <|> (return $ NameExpr left))
