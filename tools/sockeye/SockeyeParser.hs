{-
  SockeyeParser.hs: Parser for Sockeye

  Part of Sockeye

  Copyright (c) 2018, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeParser
( parseSockeye ) where

import System.FilePath

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

import SockeyeASTMeta
import qualified SockeyeParserAST as AST

{- Parser main function -}
parseSockeye :: String -> String -> Either ParseError AST.SockeyeFile
parseSockeye = parse sockeyeFile

data TopLevel
    = ModuleDecl AST.Module
    | TypeDecl AST.NamedType

data ModuleBody
    = ConstDecl AST.NamedConstant
    | InstDecl AST.InstanceDeclaration
    | NodeDecl AST.NodeDeclaration
    | Def AST.Definition

{- Sockeye parsing -}
sockeyeFile = do
    whiteSpace
    pos <- getPositionMeta
    imports <- many sockeyeImport
    (modules, types) <- do
        stmts <- many $ choice [moduleDecl, typeDecl]
        return $ foldr splitDecl ([], []) stmts
    eof
    return AST.SockeyeFile
        { AST.sockeyeFileMeta = pos
        , AST.imports         = imports
        , AST.modules         = modules
        , AST.types           = types
        }
    where
        moduleDecl = fmap ModuleDecl sockeyeModule
        typeDecl = fmap TypeDecl namedType
        splitDecl (ModuleDecl m) (ms, ts) = (m:ms, ts)
        splitDecl (TypeDecl t)   (ms, ts) = (ms, t:ts)

sockeyeImport = do
    pos <- getPositionMeta
    reserved "import"
    path <- many1 (alphaNum <|> char '_' <|> char '-' <|> char '/') <* whiteSpace
    explImports <- optionMaybe (parens $ commaSep1 importAlias)
    return AST.Import
        { AST.importMeta  = pos
        , AST.importFile  = path <.> "soc"
        , AST.explImports = explImports
        }
    <?> "import"

importAlias = do
    pos <- getPositionMeta
    origName <- identString <?> "module or type to import"
    alias <- option origName importAlias
    return AST.ImportAlias
        { AST.importAliasMeta = pos
        , AST.originalName    = origName
        , AST.importAlias     = alias
        }
    where
        importAlias = do
            reserved "as"
            identString <?> "import alias"

sockeyeModule = do
    pos <- getPositionMeta
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
    pos <- getPositionMeta
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
    pos <- getPositionMeta
    reserved "instance"
    name <- identifierName
    size <- optionMaybe arraySize
    reserved "of"
    modName <- moduleName
    return AST.InstanceDeclaration
        { AST.instDeclMeta = pos
        , AST.instName     = name
        , AST.instModName  = modName
        , AST.instArrSize  = size
        }
    <?> "instance declaration"

nodeDeclaration = do
    pos <- getPositionMeta
    kind <- nodeKind
    t <- nodeType
    name <- identifierName
    size <- optionMaybe arraySize
    return AST.NodeDeclaration
        { AST.nodeDeclMeta = pos
        , AST.nodeKind     = kind
        , AST.nodeType     = t
        , AST.nodeName     = name
        , AST.nodeArrSize  = size 
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

nodeType = do
    pos <- getPositionMeta
    originDomain <- domain
    originType <- edgeType
    (targetDomain, targetType) <- option (originDomain, Nothing) $ do
        reserved "to"
        d <- domain
        t <- optionMaybe edgeType
        return (d, t)
    return AST.NodeType
        { AST.nodeTypeMeta = pos
        , AST.originDomain = originDomain
        , AST.originType   = originType
        , AST.targetDomain = targetDomain
        , AST.targetType   = targetType
        }


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

edgeType = choice [literal, named]
    where
        literal = do
            pos <- getPositionMeta
            addrType <- addressType
            return $ AST.TypeLiteral pos addrType
        named = do
            pos <- getPositionMeta
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
    pos <- getPositionMeta
    reserved "accepts"
    blocks <- brackets $ semiSep addressBlock
    return $ AST.Accepts pos node blocks

maps node = do
    pos <- getPositionMeta
    reserved "maps"
    maps <- brackets $ semiSep mapSpec
    return $ AST.Maps pos node maps

mapSpec = do
    pos <- getPositionMeta
    addr <- addressBlock
    reserved "to"
    targets <- commaSep1 mapTarget
    return $ AST.MapSpec pos addr targets
    where
        mapTarget = do
            pos <- getPositionMeta
            node <- nodeReference
            reserved "at"
            addr <- addressBlock
            return $ AST.MapTarget pos node addr

converts node = do
    pos <- getPositionMeta
    reserved "converts"
    converts <- brackets $ semiSep convertSpec
    return $ AST.Converts pos node converts

convertSpec = mapSpec

overlays node = do
    pos <- getPositionMeta
    reserved "overlays"
    overlay <- nodeReference
    return $ AST.Overlays pos node overlay

instantiates inst = do
    pos <- getPositionMeta
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
    pos <- getPositionMeta
    reserved "binds"
    bindings <- brackets $ semiSep portBinding
    return $ AST.Binds pos inst bindings
    where
        portBinding = do
            pos <- getPositionMeta
            port <- unqualifiedRef
            reserved "to"
            node <- nodeReference
            return $ AST.PortBinding pos port node

forall = do
    pos <- getPositionMeta
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
    pos <- getPositionMeta
    name <- identifierName
    index <- optionMaybe arrayIndex
    return AST.UnqualifiedRef
        { AST.refMeta  = pos
        , AST.refName  = name
        , AST.refIndex = index
        }

nodeReference = do
    pos <- getPositionMeta
    ref1 <- unqualifiedRef
    ref2 <- optionMaybe $ (reservedOp "." >> unqualifiedRef)
    return $ maybe (AST.InternalNodeRef pos ref1) (AST.InputPortRef pos ref1) ref2

namedType = do
    pos <- getPositionMeta
    reserved "type"
    name <- typeName
    addrType <- addressType
    return $ AST.NamedType pos name addrType
    <?> "named type"

namedConstant = do
    pos <- getPositionMeta
    reserved "const"
    name <- constName
    value <- natural
    return $ AST.NamedConstant pos name value
    <?> "named constant"

addressType = do
    pos <- getPositionMeta
    addrType <- parens $ semiSep1 naturalSet
    return $ AST.AddressType pos addrType
    <?> "address type literal"

address = do
    pos <- getPositionMeta 
    addr <- parens $ semiSep1 wildcardSet
    return $ AST.Address pos addr
    <?> "address tuple"

addressBlock = do
    pos <- getPositionMeta
    addr <- address
    props <- option AST.True propertyExpr
    return $ AST.AddressBlock pos addr props

arraySize = do
    pos <- getPositionMeta
    size <- brackets $ semiSep1 naturalSet
    return $ AST.ArraySize pos size
    <?> "array size"

arrayIndex = do
    pos <- getPositionMeta
    index <- brackets $ semiSep1 wildcardSet
    return $ AST.ArrayIndex pos index
    <?> "array index"

naturalSet = do
    pos <- getPositionMeta
    ranges <- commaSep1 naturalRange
    return $ AST.NaturalSet pos ranges
    <?> "set of naturals"

wildcardSet = choice [wildcard, explicit]
    where
        explicit = do
            pos <- getPositionMeta
            set <- naturalSet
            return $ AST.ExplicitSet pos set
        wildcard = do
            pos <- getPositionMeta
            reservedOp "*"
            return $ AST.Wildcard pos

naturalRange = do
    pos <- getPositionMeta
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
            pos <- getPositionMeta
            name <- variableName
            return $ AST.Variable pos name
        lit = do
            pos <- getPositionMeta
            value <- natural
            return $ AST.Literal pos value
        slice = do
            pos <- getPositionMeta
            range <- brackets naturalRange
            return $ flip (AST.Slice pos) range
        mult = do
            pos <- getPositionMeta
            reservedOp "*"
            return $ AST.Multiplication pos
        add = do
            pos <- getPositionMeta
            reservedOp "+"
            return $ AST.Addition pos
        sub = do
            pos <- getPositionMeta
            reservedOp "-"
            return $ AST.Subtraction pos
        concat = do
            pos <- getPositionMeta
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
            pos <- getPositionMeta
            name <- propertyName
            return $ AST.Property pos name
        not = do
            pos <- getPositionMeta
            reservedOp "!"
            return $ AST.Not pos
        and = do
            pos <- getPositionMeta
            reservedOp "&&"
            return $ AST.And pos
        or = do
            pos <- getPositionMeta
            reservedOp "||"
            return $ AST.Or pos

{- Helper functions -}
lexer = P.makeTokenParser (
    emptyDef {
        {- List of reserved names -}
        P.reservedNames =
            [ "import", "as"
            , "module"
            , "input", "output"
            , "type", "const"
            , "memory", "intr", "power", "clock", "instance"
            , "of"
            , "forall", "in"
            , "accepts", "maps", "converts", "overlays"
            , "instantiates", "binds"
            , "to", "at"
            , "bits"
            ],

        {- List of operators -}
        P.reservedOpNames = 
            [ "+", "-", "*", "/", "++"
            , "!", "&&", "||"
            , "."
            ],

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
commaSep      = P.commaSep lexer 
commaSep1     = P.commaSep1 lexer
semiSep       = P.semiSep lexer
semiSep1      = P.semiSep1 lexer
identString   = P.identifier lexer
natural       = P.natural lexer

typeName       = identString <?> "type name"
constName      = identString <?> "constant name"
moduleName     = identString <?> "module name"
parameterName  = identString <?> "parameter name"
variableName   = identString <?> "variable name"
propertyName   = identString <?> "property name"
identifierName = identString <?> "identifier"

getPositionMeta = fmap ParserMeta getPosition
