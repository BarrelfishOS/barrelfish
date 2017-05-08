{-
  SockeyeParser.hs: Parser for Sockeye

  Part of Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

import Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (javaStyle)

import qualified Data.Map as Map

import SockeyeAST

lexer = P.makeTokenParser (
    javaStyle  {
        {- list of reserved Names -}
        P.reservedNames = [
            "is", "are",
            "accept", "map",
            "over",
            "to", "at"
        ],

        {- valid identifiers -}
        P.identStart = letter,
        P.identLetter = alphaNum,

        {- comment start and end -}
        P.commentStart = "/*",
        P.commentEnd = "*/",
        P.commentLine = "//",
        P.nestedComments = False
    })

identifier = P.identifier lexer
reserved = P.reserved lexer
natural = P.natural lexer
brackets = P.brackets lexer
symbol = P.symbol lexer
stringLiteral = P.stringLiteral lexer
commaSep = P.commaSep lexer

sockeyeFile = do
    nodes <- many net
    return $ Net $ Map.fromList nodes

net = do
    nodeId <- identifier
    reserved "is"
    node <- node
    return (nodeId, node)

node = do
    accept <- accept
    translate <- tranlsate
    return Node { accept    = accept
                , translate = translate
                }
    where accept = do
            reserved "accept"
            brackets $ commaSep block
          tranlsate = do
            reserved "map"
            brackets $ commaSep mapping

mapping = do
    fromBlock <- block
    reserved "to"
    name <- name
    return (fromBlock, name)

name = do
    nodeId <- identifier
    reserved "at"
    block <- block
    return Name { nodeId = nodeId
                , addrBlock  = block
                }

block = do
    base <- natural
    symbol "-"
    limit <- natural
    return Block { base  = fromIntegral base
                 , limit = fromIntegral limit
                 }

parseSockeye = parse sockeyeFile "(unknown)"
