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

module SockeyeParser
( parseSockeye ) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

import qualified SockeyeASTParser as AST

{- Parser main function -}
parseSockeye :: String -> String -> Either ParseError (AST.Sockeye AST.ParserMeta)
parseSockeye = parse sockeyeFile

data TopLevel
    = ModuleDecl (AST.Module AST.ParserMeta)
    | TypeDecl (AST.NamedType AST.ParserMeta)

data ModuleBody
    = ConstDecl (AST.NamedConstant AST.ParserMeta)
    | InstDecl (AST.InstanceDeclaration AST.ParserMeta)
    | NodeDecl (AST.NodeDeclaration AST.ParserMeta)
    | Def (AST.Definition AST.ParserMeta)

{- Sockeye parsing -}
sockeyeFile = do
    whiteSpace
    pos <- getPosition
    (modules, types) <- do
        stmts <- many $ choice [moduleDecl, typeDecl]
        return $ foldr splitDecl ([], []) stmts
    eof
    return AST.Sockeye
        { AST.sockeyeMeta = pos
        , AST.modules     = modules
        , AST.types       = types
        }
    where
        moduleDecl = fmap ModuleDecl sockeyeModule
        typeDecl = fmap TypeDecl namedType
        splitDecl (ModuleDecl m) (ms, ts) = (m:ms, ts)
        splitDecl (TypeDecl t)   (ms, ts) = (ms, t:ts)

sockeyeModule = do
    pos <- getPosition
    reserved "module"
    name <- moduleName
    params <- option [] (parens $ commaSep moduleParam)
    (consts, insts, nodes, defs) <- braces moduleBody
    return AST.Module 
        { AST.moduleMeta  = pos
        , AST.moduleName  = name
        , AST.parameters  = params
        , AST.constants   = consts
        , AST.instDecls   = insts
        , AST.nodeDecls   = nodes
        , AST.definitions = defs
        }
    <?> "module specification"

moduleParam = do
    pos <- getPosition
    range <- parens naturalSet <?> "parameter range"
    paramName <- parameterName
    return AST.ModuleParameter
        { AST.paramMeta  = pos
        , AST.paramName  = paramName
        , AST.paramRange = range
        }

moduleBody = do
    body <- many $ choice [constDecl, instDecl, nodeDecl, def]
    return $ foldr splitBody ([], [], [], []) body
    where
        constDecl = fmap ConstDecl namedConstant
        instDecl = fmap InstDecl instanceDeclaration
        nodeDecl = fmap NodeDecl nodeDeclaration
        def = fmap Def definition
        splitBody (ConstDecl c) (cs, is, ns, ds) = (c:cs, is, ns, ds)
        splitBody (InstDecl i)  (cs, is, ns, ds) = (cs, i:is, ns, ds)
        splitBody (NodeDecl n)  (cs, is, ns, ds) = (cs, is, n:ns, ds)
        splitBody (Def d)       (cs, is, ns, ds) = (cs, is, ns, d:ds)

instanceDeclaration = do
    pos <- getPosition
    reserved "instance"
    name <- identifierName
    size <- optionMaybe arraySize
    reserved "of"
    modName <- moduleName
    return $ case size of
        Nothing -> AST.SingleInstance pos name modName
        Just s -> AST.ArrayInstance
            { AST.instDeclMeta = pos
            , AST.instanceName   = name
            , AST.instanceModule = modName
            , AST.instArrSize = s
            }
    <?> "instance declaration"

nodeDeclaration = do
    pos <- getPosition
    kind <- nodeKind
    originDomain <- domain
    originType <- nodeType
    (targetDomain, targetType) <- option (originDomain, Nothing) $ do
        reserved "to"
        d <- domain
        t <- optionMaybe nodeType
        return (d, t)
    name <- identifierName
    size <- optionMaybe arraySize
    return $ case size of
        Nothing -> AST.SingleNode
            { AST.nodeDeclMeta = pos
            , AST.nodeKind     = kind
            , AST.originDomain = originDomain
            , AST.originType   = originType
            , AST.targetDomain = targetDomain
            , AST.targetType   = targetType
            , AST.nodeName     = name
            }
        Just s -> AST.ArrayNode
            { AST.nodeDeclMeta = pos
            , AST.nodeKind     = kind
            , AST.originDomain = originDomain
            , AST.originType   = originType
            , AST.targetDomain = targetDomain
            , AST.targetType   = targetType
            , AST.nodeName     = name
            , AST.nodeArrSize  = s 
            }
    <?> "node declaration"

nodeKind = option AST.InternalNode $ choice [input, output]
    where
        input = do
            reserved "input"
            return AST.InputPort
        output = do
            reserved "output"
            return AST.OutputPort

domain = choice [memory, intr, power, clock] <?> "node domain"
    where
        memory = do
            reserved "memory"
            return AST.Memory
        intr = do
            reserved "intr"
            return AST.Interrupt
        power = do
            reserved "power"
            return AST.Power
        clock = do
            reserved "clock"
            return AST.Clock

nodeType = choice [literal, named]
    where
        literal = do
            pos <- getPosition
            addrType <- addressType
            return $ AST.TypeLiteral pos addrType
        named = do
            pos <- getPosition
            name <- parens typeName
            return $ AST.TypeName pos name
            <?> "(<type name>)"

definition = choice [forall, def]
    where
        def = do
            receiver <- unqualifiedRef
            choice
                [ accepts receiver
                , maps receiver
                , converts receiver
                , overlays receiver
                , instantiates receiver
                , binds receiver
                ]

accepts node = do
    pos <- getPosition
    reserved "accepts"
    blocks <- brackets $ semiSep addressBlock
    return $ AST.Accepts pos node blocks

maps node = do
    pos <- getPosition
    reserved "maps"
    maps <- brackets $ semiSep mapSpec
    return $ AST.Maps pos node maps

mapSpec = do
    pos <- getPosition
    addr <- addressBlock
    reserved "to"
    targets <- commaSep1 mapTarget
    return $ AST.MapSpec pos addr targets
    where
        mapTarget = do
            pos <- getPosition
            node <- nodeReference
            reserved "at"
            addr <- addressBlock
            return $ AST.MapTarget pos node addr

converts node = do
    pos <- getPosition
    reserved "converts"
    converts <- brackets $ semiSep convertSpec
    return $ AST.Converts pos node converts

convertSpec = mapSpec

overlays node = do
    pos <- getPosition
    reserved "overlays"
    overlay <- nodeReference
    return $ AST.Overlays pos node overlay

instantiates inst = do
    pos <- getPosition
    reserved "instantiates"
    modName <- moduleName
    args <- option [] (parens $ commaSep naturalExpr)
    return AST.Instantiates
        { AST.defMeta    = pos
        , AST.inst       = inst
        , AST.instModule = modName
        , AST.arguments  = args
    }

binds inst = do
    pos <- getPosition
    reserved "binds"
    bindings <- brackets $ semiSep portBinding
    return $ AST.Binds pos inst bindings
    where
        portBinding = do
            pos <- getPosition
            port <- unqualifiedRef
            reserved "to"
            node <- nodeReference
            return $ AST.PortBinding pos port node

forall = do
    pos <- getPosition
    reserved "forall"
    var <- variableName
    reserved "in"
    range <- parens $ semiSep1 naturalSet
    body <- braces $ many definition
    return AST.Forall
        { AST.defMeta        = pos
        , AST.boundVarName   = var
        , AST.varRange       = range
        , AST.quantifierBody = body
        }

unqualifiedRef = do
    pos <- getPosition
    name <- identifierName
    index <- optionMaybe $ arrayIndex
    return $ maybe (AST.SingleRef pos name) (AST.ArrayRef pos name) index

nodeReference = do
    pos <- getPosition
    ref1 <- unqualifiedRef
    ref2 <- optionMaybe $ (reservedOp "." >> unqualifiedRef)
    return $ maybe (AST.InternalNodeRef pos ref1) (AST.InputPortRef pos ref1) ref2

namedType = do
    pos <- getPosition
    reserved "type"
    name <- typeName
    addrType <- addressType
    return $ AST.NamedType pos name addrType
    <?> "named type"

namedConstant = do
    pos <- getPosition
    reserved "const"
    name <- constName
    value <- natural
    return $ AST.NamedConstant pos name value
    <?> "named constant"

addressType = do
    pos <- getPosition
    addrType <- parens $ semiSep1 naturalSet
    return $ AST.AddressType pos addrType
    <?> "address type literal"

address = do
    pos <- getPosition 
    addr <- parens $ semiSep1 wildcardSet
    return $ AST.Address pos addr
    <?> "address tuple"

addressBlock = do
    pos <- getPosition
    addr <- address
    props <- option AST.True propertyExpr
    return $ AST.AddressBlock pos addr props

arraySize = do
    pos <- getPosition
    size <- brackets $ semiSep1 naturalSet
    return $ AST.ArraySize pos size
    <?> "array size"

arrayIndex = do
    pos <- getPosition
    index <- brackets $ semiSep1 wildcardSet
    return $ AST.ArrayIndex pos index
    <?> "array index"

naturalSet = do
    pos <- getPosition
    ranges <- commaSep1 naturalRange
    return $ AST.NaturalSet pos ranges
    <?> "set of naturals"

wildcardSet = choice [wildcard, explicit]
    where
        explicit = do
            pos <- getPosition
            set <- naturalSet
            return $ AST.ExplicitSet pos set
        wildcard = do
            reservedOp "*"
            return AST.Wildcard

naturalRange = do
    pos <- getPosition
    base <- naturalExpr
    choice [bits pos base, limit pos base, singleton pos base]
    <?> "range of naturals"
    where
        bits pos base = do
            reserved "bits"
            bits <- naturalExpr
            return $ AST.BitsRange pos base bits
        limit pos base = do
            reserved "to"
            limit <- naturalExpr
            return $ AST.LimitRange pos base limit
        singleton pos base = return $ AST.SingletonRange pos base

naturalExpr = buildExpressionParser opTable term <?> "arithmetic expression"
    where
        term = parens naturalExpr <|> var <|> lit
        opTable =
            [ [ Postfix slice ]
            , [ Infix mult AssocLeft ]
            , [ Infix add AssocLeft, Infix sub AssocLeft ]
            , [ Infix concat AssocLeft ]
            ]
        var = do
            pos <- getPosition
            name <- variableName
            return $ AST.Variable pos name
        lit = do
            pos <- getPosition
            value <- natural
            return $ AST.Literal pos value
        slice = do
            pos <- getPosition
            range <- brackets naturalSet
            return $ flip (AST.Slice pos) range
        mult = do
            pos <- getPosition
            reservedOp "*"
            return $ AST.Multiplication pos
        add = do
            pos <- getPosition
            reservedOp "+"
            return $ AST.Addition pos
        sub = do
            pos <- getPosition
            reservedOp "-"
            return $ AST.Subtraction pos
        concat = do
            pos <- getPosition
            reservedOp "++"
            return $ AST.Concat pos

propertyExpr = buildExpressionParser opTable term <?> "property expression"
    where
        term = parens propertyExpr <|> prop
        opTable =
            [ [ Prefix not ]
            , [ Infix and AssocLeft, Infix or AssocLeft ]
            ]
        prop = do
            pos <- getPosition
            name <- propertyName
            return $ AST.Property pos name
        not = do
            pos <- getPosition
            reservedOp "!"
            return $ AST.Not pos
        and = do
            pos <- getPosition
            reservedOp "&&"
            return $ AST.And pos
        or = do
            pos <- getPosition
            reservedOp "||"
            return $ AST.Or pos

{- Helper functions -}
lexer = P.makeTokenParser (
    emptyDef {
        {- List of reserved Names -}
        P.reservedNames = keywords,

        {- List of operators -}
        P.reservedOpNames = operators,

        {- Valid identifiers -}
        P.identStart = letter,
        P.identLetter = alphaNum <|> char '_',

        {- comment start and end -}
        P.commentStart = "/*",
        P.commentEnd = "*/",
        P.commentLine = "//",
        P.nestedComments = False,

        {- Sockeye is case sensitive -}
        P.caseSensitive = True
    })

whiteSpace    = P.whiteSpace lexer
reserved      = P.reserved lexer
reservedOp    = P.reservedOp lexer
parens        = P.parens lexer
brackets      = P.brackets lexer
braces        = P.braces lexer
symbol        = P.symbol lexer
commaSep      = P.commaSep lexer 
commaSep1     = P.commaSep1 lexer
semiSep       = P.semiSep lexer
semiSep1      = P.semiSep1 lexer
identString   = P.identifier lexer
natural       = P.natural lexer

keywords =
    [ "import", "module"
    , "input", "output"
    , "type", "const"
    , "memory", "intr", "power", "clock", "instance"
    , "of"
    , "forall", "in"
    , "accepts", "maps", "converts", "overlays"
    , "instantiates", "binds"
    , "to", "at"
    , "bits"
    ]

operators =
    [ "+", "-", "*", "/", "++"
    , "!", "&&", "||"
    , "."
    ]

typeName       = identString <?> "type name"
constName      = identString <?> "constant name"
moduleName     = identString <?> "module name"
parameterName  = identString <?> "parameter name"
variableName   = identString <?> "variable name"
propertyName   = identString <?> "property name"
identifierName = identString <?> "identifier"
