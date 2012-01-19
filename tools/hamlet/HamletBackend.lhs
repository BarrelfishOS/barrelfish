%include polycode.fmt

%if false
  Error: DSL for error definition
   
  Copyright (c) 2009 ETH Zurich.
  All rights reserved.
  
  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%endif

%if false

> {-# OPTIONS_GHC -fglasgow-exts -XBangPatterns #-}

%endif

%if false

> module HamletBackend where

> import Data.Maybe
> import Data.List
> import Data.Char
> import qualified Data.Map as Map

> import Debug.Trace

> import Semantics
> import Constructs
> import PureExpressions
> import Expressions

> import Constructs.Conditionals
> import Constructs.References
> import Constructs.Functions
> import Constructs.Enumerations
> import Constructs.Structures
> import Constructs.Unions

> import Libc.Assert

> import Libbarrelfish.HasDescendants
> import Libbarrelfish.MemToPhys

> import HamletAst

%endif


> strict x = x


> boolT :: TypeExpr
> boolT = typedef uint64T "bool"
>
> false :: PureExpr
> false = uint64 $ 0
>
> true :: PureExpr
> true = uint64 $ 1

> objtypeT :: TypeExpr
> objtypeT = typedef uint64T "enum objtype"

> ctePtrT :: TypeExpr
> ctePtrT = typedef uint64T "struct cte *"

> lvaddrT :: TypeExpr
> lvaddrT = typedef uint64T "lvaddr_t"

> genpaddrT :: TypeExpr
> genpaddrT = typedef uint64T "genpaddr_t"

> gensizeT :: TypeExpr
> gensizeT = typedef uint64T "gensize_t"

\section{|Objtype| Enumeration}

> {-
> ofObjTypeEnum :: Enumeration ->
>                  CapName -> 
>                  FoFCode PureExpr
> ofObjTypeEnum !enums (CapName x) = strict $! newEnum "objtype" enums name
>     where name = "ObjType_" ++ x
> {-# INLINE ofObjTypeEnum #-}
> -}


> ofObjTypeEnum :: CapName -> PureExpr
> ofObjTypeEnum (CapName x) =  CLRef Global uint64T (Provided ("ObjType_" ++ x))


> mkObjTypeEnum :: [Capability] -> Enumeration
> mkObjTypeEnum caps = enums `seq` objTypeNum `seq`
>                      objTypeNum : enums 
>     where (n,enums) = foldl' mkEnumField (0,[]) caps
>           mkEnumField (!i,!acc) !cap = i' `seq` 
>                                        (i',acc' )
>               where CapName capName = name cap 
>                     strName = "ObjType_" ++ capName
>                     acc' = strName `seq` (strName, i) : acc
>                     i' = i + 1
>           objTypeNum = ("ObjType_Num",n)


> -- objType :: [Capability] -> PureExpr
> -- objType caps = TEnum "objtype" $ mkObjTypeEnum caps

\section{Capabality Structure}

> capRightsT :: TypeExpr
> capRightsT = typedef uint8T "CapRights"


> capNameOf :: Capability -> String
> capNameOf cap = capName
>     where CapName capName = name cap

> lower :: String -> String
> lower = map' $! toLower

> map' f ![] = []
> map' f !(x:xs) = l `seq` l
>     where k = map' f xs
>           l = f x : k
> mapp = map'' []
>        where
>        map'' !acc !f ![] = acc
>        map'' !acc !f !(x:xs) = 
>            x' `seq`
>            map'' (x':acc) f xs
>                where x' = f x

> mkCapsStruct :: Capabilities -> TFieldList
> mkCapsStruct caps = strict
>     [("type", objtypeT),
>      ("rights", capRightsT),
>      ("u", capUnionT)]
>     where capUnionT = unionST "capability_u" ((map' (\cap -> (lower $ capNameOf cap, mkCapStructT cap))
>                                                  (capabilities caps)) )
>                      -- XXX: Why do I need to define types here when they are already
>                      -- defined somewhere else? Also as hamlet doesn't handle uintptr_t,
>                      --  lvaddr is defined as uint64 although that is not always the case.
>                      -- Using a type not defined here will generate "type"*
>                      -- which is obviously broken. -Akhi
>           mkCapStructT cap = structST (capNameOf cap) (map' mkCapField (fields cap))
>           mkCapField (CapField typ (NameField name)) = (name, toFofType typ)
>           toFofType UInt8 = uint8T
>           toFofType UInt16 = uint16T
>           toFofType UInt32 = uint32T
>           toFofType UInt64 = uint64T
>           toFofType Int = int32T
>           toFofType GenPAddr = typedef uint64T "genpaddr_t"
>           toFofType GenSize = typedef uint64T "gensize_t"
>           toFofType LPAddr   = typedef uint64T "lpaddr_t"
>           toFofType GenVAddr = typedef uint64T "genvaddr_t"
>           toFofType LVAddr   = typedef uint64T "lvaddr_t"
>           toFofType CAddr = typedef uint32T "caddr_t"
>           toFofType (Pointer s) = typedef uint64T (s ++ "*")
>           toFofType CapRights = capRightsT

> capsStructT :: Capabilities -> TypeExpr
> capsStructT cap = structST  "capability" (mkCapsStruct cap)

\section{Get address and size}

\subsection{Convert common expressions}

Generate an expression for the size of a value expressed in "bits"
i.e. {\tt (((gensize\_t)1) << bits)}

> mkSize :: PureExpr -> PureExpr
> mkSize bits = (cast gensizeT (uint64 1)) .<<. bits

> readCapAttr :: PureExpr -> String -> FoFCode PureExpr
> readCapAttr cap attr = readRef cap >>= readRef >>= (\c -> readStruct c attr)

Try and look up a name in a definitions and failing that generated code to look
it up in the fields of a cap.

> lookupName :: [(String, Int)] -> PureExpr -> String -> String -> FoFCode PureExpr
> lookupName defs cap capType name =
>     case (name `lookup` defs) of
>       Nothing  -> do
>                     capU <- readCapAttr cap "u"
>                     capCStruct <- readUnion capU $ lower $ capType
>                     readStruct capCStruct name
>       Just val -> return $ uint64 $ toInteger val

Generate code to calculate the result of an expression.

> exprCode :: [(String, Int)] -> PureExpr -> String -> Expr -> FoFCode PureExpr
> exprCode defs cap capType (NameExpr n) = lookupName defs cap capType n
> exprCode defs cap capType (AddExpr left right) =
>     do
>       lval <- lookupName defs cap capType left
>       rval <- lookupName defs cap capType right
>       return (lval .+. rval)

Generate code to calculate the "address" property of a cap.

> addressExprCode :: [(String, Int)] -> PureExpr -> String -> AddressExpr -> FoFCode PureExpr
> addressExprCode defs cap capType (MemToPhysOp expr) =
>     do
>       lval <- exprCode defs cap capType expr
>       mem_to_phys $ cast lvaddrT lval
> addressExprCode defs cap capType (AddressExpr expr) =
>       exprCode defs cap capType expr

Generate code to calculate the "size" property of a cap.

> sizeExprCode :: [(String, Int)] -> PureExpr -> String -> SizeExpr -> FoFCode PureExpr
> sizeExprCode defs cap capType (SizeExpr expr) = exprCode defs cap capType expr
> sizeExprCode defs cap capType (SizeBitsExpr expr) =
>     do
>       bitsExpr <- exprCode defs cap capType expr
>       return $ mkSize $ bitsExpr

\subsection{get\_address}

> get_address :: Capabilities -> FoFCode PureExpr
> get_address caps =
>     def [] "get_address"
>         (mkGetPropSwitch caps mkGetAddr)
>         genpaddrT
>         [(ptrT $ ptrT $ capsStructT caps, Nothing)]
>     where
>         nullptr = cast genpaddrT $ uint64 0
>         mkGetAddr :: [(String, Int)] -> Capability -> PureExpr -> FoFCode PureExpr
>         mkGetAddr defines capType cap =
>             case rangeExpr capType of
>               Just expr -> mkGetAddrExpr defines capType cap $ fst expr
>               Nothing   -> returnc nullptr
>         mkGetAddrExpr defines capType cap expr = 
>           do
>             res <- addressExprCode defines cap capName expr
>             returnc res
>           where
>             capName = case name capType of (CapName s) -> s

> get_size :: Capabilities -> FoFCode PureExpr
> get_size caps =
>     def [] "get_size"
>         (mkGetPropSwitch caps mkGetSize)
>         gensizeT
>         [(ptrT $ ptrT $ capsStructT caps, Nothing)]
>     where
>         mkGetSize :: [(String, Int)] -> Capability -> PureExpr -> FoFCode PureExpr
>         mkGetSize defines capType cap =
>             case rangeExpr capType of
>               Just expr -> mkGetSizeExpr defines capType cap $ snd expr
>               Nothing   -> returnc $ cast gensizeT $ uint64 0
>         mkGetSizeExpr defines capType cap expr = 
>           do
>             res <- sizeExprCode defines cap capName expr
>             returnc res
>           where
>             capName = case name capType of (CapName s) -> s

> mkGetPropSwitch :: Capabilities
>                    -> ([(String, Int)] -> Capability -> PureExpr -> FoFCode PureExpr)
>                    -> ([PureExpr] -> FoFCode PureExpr)
> mkGetPropSwitch caps mkGetProp = get_prop_int
>       where
>         get_prop_int :: [PureExpr] -> FoFCode PureExpr
>         get_prop_int (cap : []) = 
>           do
>             let cases = map (mkGetPropCase cap) (capabilities caps)
>             capTypeInt <- readCapAttr cap "type"
>             switch capTypeInt cases ((assert false) >> (returnc $ uint64 0))
>         mkGetPropCase cap capType =
>             ((ofObjTypeEnum $ name capType), mkGetProp defineList capType cap)
>         defineList = mkDefineList $ defines caps

\section{Compare}

$get\_type\_root$ gets a number indicating which tree root of the type forest a
given type belongs to.

> get_type_root :: Capabilities -> FoFCode PureExpr
> get_type_root caps =
>     def [] "get_type_root"
>         get_type_root_int
>         uint8T
>         [(objtypeT, Nothing)]
>     where
>         get_type_root_int [typ] =
>           -- big switch over all types, each just returns the result
>           switch typ cases (assert false >> returnc false)
>         cases = map (mkCase . name) $ capTypes
>         mkCase capName = (ofObjTypeEnum capName, mkGetRoot capName)
>         -- case body just returns the calculated number
>         mkGetRoot capName = returnc $ uint8 $ fromIntegral $ fromJust $ elemIndex (typeRoot capName) rootTypeNames
>         capTypes = capabilities caps
>         -- cap name -> cap lookup list
>         capTypesLookup = map (\c -> (name c, c)) capTypes
>         -- list of names of root types. the index in this list is the final
>         -- root index
>         rootTypeNames = map name $ filter (isNothing . from) capTypes
>         typeRoot capName =
>           -- recursively walk up retype relations
>           case from $ fromJust $ lookup capName capTypesLookup of
>             Just n -> typeRoot n
>             Nothing -> capName

$compare\_caps$ returns -1, 0 or 1 indicating the ordering of the given caps.

\texttt{compare\_caps(left, right) $op$ 0} implies \texttt{left $op$ right}, where op is a numerical comparison.

> compare_caps :: Capabilities ->
>                 PureExpr ->
>                 PureExpr ->
>                 PureExpr ->
>                 FoFCode PureExpr
> compare_caps caps get_type_root get_address get_size =
>     def [] "compare_caps"
>         (compare_caps_int caps get_type_root get_address get_size)
>         int8T
>         [(ptrT $ ptrT $ capsStructT caps, Just "left"),
>          (ptrT $ ptrT $ capsStructT caps, Just "right"),
>          (boolT, Just "tiebreak")]

> compare_caps_int :: Capabilities ->
>                     PureExpr ->
>                     PureExpr ->
>                     PureExpr ->
>                     [PureExpr] ->
>                     FoFCode PureExpr
> compare_caps_int caps get_type_root get_address get_size
>                  [left_cap_pp, right_cap_pp, tiebreak] =
>     do
>       leftCap <- getCap left_cap_pp
>       rightCap <- getCap right_cap_pp
>       leftType <- readStruct leftCap "type"
>       rightType <- readStruct rightCap "type"
>       -- perform a bunch of global tests
>       sequence $ map (doCmp left_cap_pp leftCap leftType
>                             right_cap_pp rightCap rightType) tests
>       -- at this point we know the caps are the same type
>       -- also, they are at the same address and have the same size
>       -- if the cap type has additional "eq" attributes, we now compare those
>       let haveEqCaps = filter (not . null . eqFields) $ capabilities caps
>           mkCase capType = ((ofObjTypeEnum $ name capType),
>                             (mkEqCmp leftCap rightCap capType))
>           eqCases = map mkCase haveEqCaps
>       switch leftType eqCases (return false)
>       -- finally, if the tie break param is true we compare the pointers
>       ifc (return tiebreak) (mkCmp left_cap_pp right_cap_pp (.<.)) (return false)
>       returnc $ int8 0
>     where
>       getCap cap_pp = (readRef cap_pp >>= readRef)
>
>       -- type-independant tests
>       tests = [(getRoot, (.<.)),
>                (getAddr, (.<.)),
>                (getSize, (.>.)), -- note reversed ordering
>                (getType, (.<.))]
>       getRoot cpp c ct = call get_type_root [ct]
>       getAddr cpp c ct = call get_address [cpp]
>       getSize cpp c ct = call get_size [cpp]
>       getType cpp c ct = return ct
>
>       -- comparison code generators
>       mkCmp left right op =
>         ifc (return (left .!=. right)) 
>             (returnc $ test (left `op` right) (int8 (-1)) (int8 1))
>             (return false)
>       doCmp lcpp lc lct rcpp rc rct (f, op) =
>         do
>           l <- f lcpp lc lct
>           r <- f rcpp rc rct
>           mkCmp l r op
>       mkEqCmp leftCap rightCap capType =
>         do
>           let eqs = map (\(NameField n) -> n) $ eqFields capType
>           leftCapU <- readStruct leftCap "u"
>           rightCapU <- readStruct rightCap "u"
>           let capName = case name capType of CapName n -> n
>           leftCapS <- readUnion leftCapU $ lower capName
>           rightCapS <- readUnion rightCapU $ lower capName
>           sequence $ map (doFieldCmp leftCapS rightCapS) eqs
>           return false
>       doFieldCmp lc rc n =
>         do
>           l <- readStruct lc n
>           r <- readStruct rc n
>           mkCmp l r (.<.)

\section{Is well founded}

\subsection{Get children}

\verb|getChildren| computes the direct children of a cap.
\verb|isChild| returns True iff c is a child of p.

> getChildren cap capabilities =
>     [ c | c <- capabilities, isChild c cap ] ++
>     (if fromSelf cap then [cap] else [])
> isChild :: Capability -> Capability -> Bool
> isChild c p = case (from c) of
>                   Nothing -> False
>                   Just cn -> (name p) == cn

\subsection{Compute Well-found-ness Relation}

{\tt is\_well\_founded} checks if {\tt src\_type} can be retyped to {\tt
dest\_type}.

> is_well_founded :: [Capability] ->
>                    FoFCode PureExpr
> is_well_founded caps =
>     def [] "is_well_founded"
>             (is_well_founded_int caps)
>             boolT
>             [(objtypeT, Just "src_type"),
>              (objtypeT, Just "dest_type")]

> is_well_founded_int caps (src_type : dest_type : []) =
>     do
>       switch dest_type cases (assert false >> returnc false)
>     where
>       cases = map mkCase caps
>       mkCase capType = ((ofObjTypeEnum $ name capType), (checkIsParent src_type capType))
>       checkIsParent parent capType = returnc ((checkIsFrom parent capType)  .|. (checkIsFromSelf parent capType))
>       checkIsFrom parent capType =
>         case from capType of
>           Just capName -> (parent .==. (ofObjTypeEnum capName))
>           Nothing      -> false
>       checkIsFromSelf parent capType =
>         if fromSelf capType
>            then (parent .==. (ofObjTypeEnum $ name capType))
>            else false

> is_equal_types :: FoFCode PureExpr
> is_equal_types =
>     def [] "is_equal_type"
>         is_equal_int
>         boolT
>         [(objtypeT, Nothing), (objtypeT, Nothing)]
>     where is_equal_int (src_type : dest_type : []) =
>             do returnc (src_type .==. dest_type)

\section{Is revoked first}

This function queries if the given capability can be retyped in its current
state.

\subsection{Compute Revocation Paths}

The Boolean value in the tuples indicates whether the cap can be retyped
multiple times.

> revokePaths :: [Capability] -> 
>                [(PureExpr, Maybe Bool)]
> revokePaths caps = revokePaths' [] caps
>     where revokePaths' !acc [] = acc
>           revokePaths' !acc (x:xs) = 
>               revokePaths' (revokePath x:acc) xs
>           revokePath cap = strict ( ofObjTypeEnum $ name cap, multiRet cap)
>           multiRet cap = if null (getChildren cap caps) then Nothing else Just $ multiRetype cap

\subsection{Generate Code}

> is_revoked_first :: [Capability] ->
>                     FoFCode PureExpr
> is_revoked_first caps =
>     def [] 
>         "is_revoked_first"
>         (is_revoked_first_int caps)
>         boolT
>         [(ctePtrT, Nothing),
>          (objtypeT, Nothing)]

> is_revoked_first_int :: [Capability] ->
>                         [PureExpr] ->
>                         FoFCode PureExpr
> is_revoked_first_int caps (src_cte : src_type : []) =
>     do
>     casesV <- sequence $ map' (revokeCode src_cte) revokeP
>     switch src_type
>            casesV
>            (do
>             returnc false)
>         -- revokeP contains all cap types that can be retyped
>         where revokeP = [(st, fromJust rp) | (st, rp) <- revokePaths caps, isJust rp]

Return true if cte has type that is multi-retypable or cap has no descendants,
false otherwise.

> revokeCode :: PureExpr ->
>               (PureExpr, Bool) ->
>               FoFCode (PureExpr, FoFCode PureExpr)
> revokeCode cte (codeV,mult) =
>                   (do
>                     return $! (codeV, 
>                             if mult then returnc true else
>                                 (do
>                                  b <- has_descendants cte
>                                  ifc (return $! b)
>                                      (returnc false)
>                                      (returnc true))))

\section{Is copy}

Check whether two capabilities represent the same object. This is different
from \texttt{compare\_caps(left, right, false) == 0} in that it respects
general equality specifiers like \texttt{is\_always\_copy} and
\texttt{is\_never\_copy}.

> is_copy ::  Capabilities -> 
>             PureExpr ->
>             FoFCode PureExpr
> is_copy caps compare_caps =
>     def [] 
>         "is_copy" 
>         (is_copy_int caps compare_caps)
>         boolT
>         [(ptrT $ ptrT thisCapsStructT, Just "left"),
>          (ptrT $ ptrT thisCapsStructT, Just "right")]
>     where thisCapsStructT = capsStructT caps 

> is_copy_int :: Capabilities -> 
>                PureExpr ->
>                [PureExpr] ->
>                FoFCode PureExpr
> is_copy_int caps compare_caps (leftPP : rightPP : []) =
>     do 
>       -- compare types
>       leftCap <- getCap leftPP
>       rightCap <- getCap rightPP
>       leftType <- readStruct leftCap "type"
>       rightType <- readStruct rightCap "type"
>       ifc (return (leftType .!=. rightType))
>           (returnc false) (return false)
>       -- we now have equal types, check general equality for the type
>       switch leftType generalEqCases (return false)
>       -- don't have general equality, call compare_caps with tiebreak = false
>       res <- call compare_caps [leftPP, rightPP, false]
>       returnc (res .==. (int8 0))
>     where
>       getCap cap_pp = (readRef cap_pp >>= readRef)
>       -- in general equality switch, only handle cases with such an equality
>       generalEqCases = map mkGeqCase $
>                        filter (isJust . generalEquality) $
>                        capabilities caps
>       -- case body: just return the value of the equality
>       mkGeqCase capType = ((ofObjTypeEnum $ name capType),
>                            (returnc $ fofBool $ fromJust $ generalEquality capType))
>       fofBool b = if b then true else false

> {-

\section{Is Ancestor}

\subsection{}

> ancestorPath :: [Capability] ->
>                 [((String, DecideLeq, PureExpr), 
>                   [(String, PureExpr, DecideLeq)])]
> ancestorPath caps  = mapp ancestorPathCap caps
>     where ancestorPathCap cap = cancestors `seq`
>               (strict ( capNameOf cap, 
>                         mkParentDecideLeq cap,
>                         ofObjTypeEnum (name cap)),
>                cancestors)
>                   where cancestors = ancestors [] (retypePath cap)
>           ancestors !acc [] = acc
>           ancestors !acc ((RetypePath (CapName nameCap) decideLeq):xs) =
>               obj `seq`
>               ancestors ((nameCap, obj, decideLeq):acc) xs
>                   where obj = ofObjTypeEnum $ CapName nameCap


> mkParentDecideLeq :: Capability -> DecideLeq
> mkParentDecideLeq cap = 
>     case decideFields of
>       [CapField _ _ f] -> Address f
>       [CapField _ _ (NameField f1),
>        CapField _ _ (NameField f2)] -> Interval (LeqName f1)
>                                                 (LeqName f2)
>       _ -> NoDecideLeq
>     where decideFields = filter (\(CapField decide _ _) ->
>                                   decide == DecideEquality)
>                                 (fields cap)

\subsection{}

> is_ancestor :: Capabilities ->
>                PureExpr ->
>                PureExpr ->
>                FoFCode PureExpr
> is_ancestor caps is_well_founded is_equal_types =
>     def [] 
>         "is_ancestor" 
>         (is_ancestor_int caps is_well_founded is_equal_types)
>         boolT
>         [(ptrT $ ptrT thisCapsStructT, Nothing),
>          (ptrT $ ptrT thisCapsStructT, Nothing)] 
>     where thisCapsStructT = capsStructT caps 


> is_ancestor_int :: Capabilities ->
>                    PureExpr ->
>                    PureExpr ->
>                    [PureExpr] ->
>                    FoFCode PureExpr
> is_ancestor_int caps is_well_founded is_equal_types (childPP : parentPP : []) =
>     ancestors `seq`
>     do
>       childP <- readRef childPP
>       child <- readRef childP
>       parentP <- readRef parentPP
>       parent <- readRef parentP
>       childType <- readStruct child "type"
>       parentType <- readStruct parent "type"
>       isWellFounded <- call is_well_founded [parentType, childType]
>       isEqualTypes <- call is_equal_types [parentType, childType]
>       ifc (return $! isWellFounded .==. false)
>           (do
>             returnc false)
>            (do
>              return $! void)
>       let code = map' (revokeCase (defines caps) isEqualTypes parent child childType) $!
>                       ancestors
>       casesV <- sequence $ code `seq` code
> 
>       switch parentType
>              casesV
>              (do
>                returnc false)
>       returnc false
>           where ancestors = ancestorPath (capabilities caps)


> revokeCase :: [Define] ->
>               PureExpr ->
>               PureExpr ->
>               PureExpr ->
>               PureExpr ->
>               ((String, DecideLeq , PureExpr), 
>                [(String, PureExpr, DecideLeq)]) ->
>               FoFCode (PureExpr, FoFCode PureExpr)
> revokeCase defines
>            isEqualTypes
>            parent 
>            child 
>            childType 
>            ((typeParentCap, decideParentLeq, parentTypeV), cases) =
>     code `seq`
>     do
>     return $! (parentTypeV, code)
>         where code = revokeCaseSwitch defines isEqualTypes parent decideParentLeq
>                                  child childType typeParentCap cases

> revokeCaseSwitch :: [Define] ->
>                     PureExpr ->
>                     PureExpr ->
>                     DecideLeq ->
>                     PureExpr ->
>                     PureExpr ->
>                     String ->
>                     [(String, PureExpr, DecideLeq)] ->
>                     FoFCode PureExpr
> revokeCaseSwitch defines 
>                  isEqualTypes
>                  parent
>                  decideParentLeq
>                  child 
>                  childType 
>                  typeParentCap 
>                  cases =
>     code `seq`
>     do
>       casesV <- sequence code
>       switch childType
>              casesV
>              (do
>                returnc false)
>       returnc false
>           where code = mapp (revokeCaseSwitchCase defines
>                                                   isEqualTypes
>                                                   parent 
>                                                   decideParentLeq
>                                                   typeParentCap
>                                                   child) cases


> revokeCaseSwitchCase :: [Define] ->
>                         PureExpr ->
>                         PureExpr ->
>                         DecideLeq ->
>                         String ->
>                         PureExpr ->
>                         (String, PureExpr, DecideLeq) ->
>                         FoFCode (PureExpr, FoFCode PureExpr)
> revokeCaseSwitchCase _
>                      isEqualTypes
>                      parent
>                      (Address (NameField parentAddr))
>                      typeParentCap 
>                      child 
>                      (typeChildCap, childTypeV, Address (NameField field)) = 
>     do
>     return $! (childTypeV,
>             do
>               parentU <- readStruct parent "u"
>               parentCStruct <- readUnion parentU $! lower $! typeParentCap
>               parentAddr <- readStruct parentCStruct parentAddr
>               
>               childU <- readStruct child "u"
>               childCStruct <- readUnion childU $! lower $! typeChildCap
>               childAddr <- readStruct childCStruct field
>
>               returnc (comp(isEqualTypes) .&. (parentAddr .==. childAddr)))
>               

> revokeCaseSwitchCase defines
>                      isEqualTypes
>                      parent 
>                      (Interval parentBase parentOffset)
>                      typeParentCap
>                      child
>                      (typeChildCap, childTypeV, Interval base
>                                                         offset) = 
>     defs `seq`
>     do
>       return $! (childTypeV,
>               do
>                 parentBaseV <- computeVal defs
>                                          parent
>                                          typeParentCap
>                                          parentBase
>                 parentOffsetV <- computeVal defs
>                                            parent
>                                            typeParentCap
>                                            parentOffset
>                 
>                 childBaseV <- computeVal defs 
>                                         child
>                                         typeChildCap
>                                         base
>                 childOffsetV <- computeVal defs
>                                           child
>                                           typeChildCap
>                                           offset
>                                
>                 let parentEndV = parentBaseV .+. mkSize parentOffsetV
>                 let childEndV = childBaseV .+. mkSize childOffsetV
>
>                 ifc (return isEqualTypes)
>                  (do
>                     returnc (((parentBaseV .<. childBaseV) .&.
>                               (parentEndV .>=. childEndV)) .|.
>                              ((parentBaseV .<=. childBaseV) .&.
>                               (parentEndV .>. childEndV))))
>                  (do
>                     returnc ((parentBaseV .<=. childBaseV) .&.
>                              (parentEndV .>=. childEndV))))
>           where defs = map' (\(Define x y) -> (x,y)) defines

> revokeCaseSwitchCase defines
>                      isEqualTypes
>                      parent 
>                      NoDecideLeq
>                      typeParentCap
>                      child
>                      (typeChildCap, childTypeV, x) = 
>     do
>       return $! (childTypeV,
>                  do
>                  returnc false)


> computeVal :: [(String, Int)] ->
>               PureExpr -> String ->
>               LeqField ->
>               FoFCode PureExpr
> computeVal defs cap typeCap (LeqName x) =
>     case (x `lookup` defs) of
>       Nothing -> do
>                  capU <- readStruct cap "u"
>                  capCStruct <- readUnion capU $! lower $! typeCap
>                  xV <- readStruct capCStruct x
>                  return $! xV
>       Just xV -> do 
>                  return $! uint64 $ toInteger xV
> computeVal _ cap typeCap (MemToPhysOp x) =
>     do
>       capU <- readStruct cap "u"
>       capCStruct <- readUnion capU $! lower $! typeCap
>       xV <- readStruct capCStruct x
>       xVV <- mem_to_phys $ cast lvaddrT xV
>       return $! xVV
> computeVal _ _ _ (SizeOfOp x) = 
>     do
>       return $! (sizeof $ voidT) -- not really that size
> computeVal defs cap typeCap (Sum x y) =
>     do
>       xV <- computeVal defs cap typeCap (LeqName x)
>       yV <- computeVal defs cap typeCap (LeqName y)
>       return $! xV .+. yV

\section{Back-end}

> backend :: Capabilities -> FoFCode PureExpr
> backend caps =
>     capList `seq`
>     iswf `seq`
>     enums `seq`
>     do
>     dummy <- newEnum "objtype" enums "ObjType_Num"
>     isWellFounded <- {-# SCC "iswfd" #-} iswf
>     isEqualTypes <- is_equal_types
>     isRevokedFirst <- is_revoked_first capList
>     isCopy <- is_copy caps 
>     isAncestor <- is_ancestor caps isWellFounded isEqualTypes
>     return false
>           where iswf = {-# SCC "iswf" #-} is_well_founded capList 
>                 capList = {-# SCC "capList" #-} capabilities caps
>                 enums = mkObjTypeEnum capList
>
> userbackend :: Capabilities -> FoFCode PureExpr
> userbackend caps =
>     capList `seq`
>     iswf `seq`
>     enums `seq`
>     do
>     dummy <- newEnum "objtype" enums "ObjType_Num"
>     isWellFounded <- {-# SCC "iswfd" #-} iswf
>     isEqualTypes <- is_equal_types
>     isCopy <- is_copy caps 
>     isAncestor <- is_ancestor caps isWellFounded isEqualTypes
>     return false
>           where iswf = {-# SCC "iswf" #-} is_well_founded capList 
>                 capList = {-# SCC "capList" #-} capabilities caps
>                 enums = mkObjTypeEnum capList

> -}
