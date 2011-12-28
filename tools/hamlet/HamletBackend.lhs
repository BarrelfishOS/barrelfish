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
>                      -- XXX: this is broken -AB
>                      -- fof generates uint64_t *raw, but we need uintptr_t raw[WORDS]
>                      --      ++ [("raw", arrayST (capWordSize caps) uint64T)])
>                      -- XXX: Why do I need to define types here when they are already
>                      -- defined somewhere else? Also as hamlet doesn't handle uintptr_t,
>                      --  lvaddr is defined as uint64 although that is not always the case.
>                      -- Using a type not defined here will generate "type"*
>                      -- which is obviously broken. -Akhi
>           mkCapStructT cap = structST (capNameOf cap) (map' mkCapField (fields cap))
>           mkCapField (CapField _ typ (NameField name)) = (name, toFofType typ)
>           toFofType UInt8 = uint8T
>           toFofType UInt16 = uint16T
>           toFofType UInt32 = uint32T
>           toFofType UInt64 = uint64T
>           toFofType Int = int32T
>           toFofType GenPAddr = typedef uint64T "genpaddr_t"
>           toFofType LPAddr   = typedef uint64T "lpaddr_t"
>           toFofType GenVAddr = typedef uint64T "genvaddr_t"
>           toFofType LVAddr   = typedef uint64T "lvaddr_t"
>           toFofType CAddr = typedef uint32T "capaddr_t"
>           toFofType (Pointer s) = typedef uint64T (s ++ "*")
>           toFofType CapRights = capRightsT

> capsStructT :: Capabilities -> TypeExpr
> capsStructT cap = structST  "capability" (mkCapsStruct cap)

\section{Is well founded}

\subsection{Compute Well-found-ness Relation}


> validPaths :: [Capability] -> 
>              [(PureExpr, [PureExpr])]
> validPaths !caps =
>     {-# SCC "1val" #-}
>     validPaths' [] caps
>     where validPath cap =
>               {-# SCC "2validPath" #-}
>               obj `seq` chds `seq` (obj,  chds )
>               where obj = ofObjTypeEnum $ name cap
>                     chds = childs [] (retypePath cap) 
>           childs !acc [] = 
>               {-# SCC "3childsnil" #-}
>               acc
>           childs !acc ((RetypePath childName _):xs) = 
>               {-# SCC "4childsacc" #-}
>               chdname `seq`
>               childs (chdname : acc) xs
>                      where chdname = ofObjTypeEnum childName
>           validPaths' !acc [] = 
>               {-# SCC "5validPathsnil" #-}
>               acc
>           validPaths' !acc (x:xs) = 
>               {-# SCC "6validPathsacc" #-}
>               validPaths' (validPath x:acc) xs

\subsection{Generate Code}

> is_well_founded :: [Capability] ->
>                    FoFCode PureExpr
> is_well_founded caps =
>     validP `seq`
>     def [] "is_well_founded" 
>             ({-# SCC "is_wf_int" #-} is_well_founded_int caps)
>             boolT 
>             [(objtypeT, Nothing), 
>              (objtypeT, Nothing)]
>         where  is_well_founded_int caps (src_type : dest_type : []) =
>                    {-# SCC "is_well_founded_int" #-} 
>                   do
>                   cases <- sequence validateRetypeCases
>                   switch src_type
>                          cases
>                          defaultCode
>                     where defaultCode = returnc $ false
>                           validateRetype = {-# SCC "validateRetype" #-} validateRetypeCode dest_type
>                           validateRetypeCases = {-# SCC "validateRetypeCases" #-} mapp validateRetype validP
>                validP = {-# SCC "validP" #-} [ (st, vp) | (st, vp) <- validPaths caps, vp /= [] ]

> validateRetypeCode :: PureExpr ->
>                       (PureExpr, [PureExpr]) ->
>                       FoFCode (PureExpr, FoFCode PureExpr)
> validateRetypeCode destType (srcTypeV, validTypesP) =
>     do 
>       return $! (srcTypeV,
>                  (do returnc $ condition validTypesP))
>     where condition validTypes = foldl' orType false validTypes
>           orType acc srcType = acc .|. (destType .==. srcType)

> is_equal_types :: FoFCode PureExpr
> is_equal_types =
>     def [] "is_equal_type"
>         is_equal_int
>         boolT
>         [(objtypeT, Nothing), (objtypeT, Nothing)]
>     where is_equal_int (src_type : dest_type : []) =
>             do returnc (src_type .==. dest_type)

\section{Is revoked first}

\subsection{Compute Revocation Paths}

> revokePaths :: [Capability] -> 
>                [(PureExpr, Maybe Multiplicity)]
> revokePaths caps = revokePaths' [] caps
>     where revokePaths' !acc [] = acc
>           revokePaths' !acc (x:xs) = 
>               revokePaths' (revokePath x:acc) xs
>           revokePath cap = strict ( ofObjTypeEnum $ name cap, retypeCap $ cap )
>     

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
>         

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
>         where revokeP = [(st, rp) | (st, rp) <- revokePaths caps, isJust rp]


> revokeCode :: PureExpr ->
>               (PureExpr, Maybe Multiplicity) ->
>               FoFCode (PureExpr, FoFCode PureExpr)
> revokeCode cte (codeV,mult) =
>                   (do
>                     return $! (codeV, 
>                             case mult of
>                             Nothing -> returnc false
>                             Just Multiple -> returnc true
>                             Just Unique -> 
>                                 (do
>                                  b <- has_descendants cte
>                                  ifc (do return $! b)
>                                      (do returnc false)
>                                      (do returnc true))))

\section{Is copy}

\subsection{Extract Equality Deciding Fields}

> data EqualityParams = OnFields [String]
>                     | AlwaysEqual
>                     | AlwaysDistinct

> {-
> instance DeepSeq EqualityParams where
>     deepSeq (OnFields s) y = deepSeq s y
>     deepSeq AlwaysEqual y = y
>     deepSeq AlwaysDistinct y = y
> -}

> equalityFields :: [Capability] -> 
>                   [(String, PureExpr, EqualityParams)]
> equalityFields caps = equalityFields' [] caps
>     where equalityFields' !acc [] = acc
>           equalityFields' !acc (cap:xs) =
>               equalityFields' (equalityField cap : acc) xs
>           equalityField cap = 
>               case generalEquality cap of
>                 Just True -> strict (capNameOf cap,
>                                      ofObjTypeEnum $ name cap ,
>                                      AlwaysEqual)
>                 Just False -> strict (capNameOf cap,
>                                       ofObjTypeEnum $ name cap ,
>                                       AlwaysDistinct)
>                 Nothing ->
>                    case eqFields of
>                       [] -> error "equalityFields: undefined Equality"
>                       _ -> strict (capNameOf cap,
>                                    ofObjTypeEnum $ name cap ,
>                                    OnFields $ map' (\(CapField _ _ (NameField name)) -> name) 
>                                               eqFields)
>               where eqFields = strict $! filter (\(CapField decideEq _ _) -> 
>                                                  decideEq == DecideEquality)
>                                                 (fields cap)

\subsection{Implement Is Copy}


> is_copy ::  Capabilities -> 
>             FoFCode PureExpr
> is_copy caps =
>     def [] 
>         "is_copy" 
>         (is_copy_int caps)
>         boolT
>         [(ptrT $ ptrT thisCapsStructT, Nothing),
>          (ptrT $ ptrT thisCapsStructT, Nothing)]
>     where thisCapsStructT = capsStructT caps 


> is_copy_int :: Capabilities -> 
>                [PureExpr] ->
>                FoFCode PureExpr
> is_copy_int caps (cap1PP : cap2PP : []) =
>     do 
>     cap1P <- readRef cap1PP
>     cap2P <- readRef cap2PP
>     cap1 <- readRef cap1P
>     cap2 <- readRef cap2P
>     typeCap1 <- readStruct cap1 "type"
>     typeCap2 <- readStruct cap2 "type"
>     ifc (return $! (typeCap1 .!=. typeCap2))
>        (do
>          returnc false)
>        (do
>          return $! void)
>     copyCasesV <- sequence $ copyCases
>     switch typeCap1
>            copyCasesV
>            defaultCase
>         where defaultCase = 
>                   do
>                   assert false
>                   returnc false
>               copyCases = map' (copyCase cap1PP cap2PP) $!
>                           equalityFields (capabilities caps)

> copyCase :: PureExpr -> PureExpr ->
>             (String , PureExpr, EqualityParams) ->
>             FoFCode (PureExpr, FoFCode PureExpr)
> copyCase cap1PP cap2PP (typeCap, caseCodeV, OnFields fields) =
>     condition `seq`
>     do
>       return $! (caseCodeV, do c <- condition; returnc c)
>           where condition = foldl' andTest (return $! true) tests
>                 andTest acc x =
>                     (do
>                       accV <- acc
>                       xV <- x
>                       return $! accV .&. xV)
>                 tests = map' mkTest fields 
>                 mkTest field = (do 
>                                 cap1P <- readRef cap1PP
>                                 cap1 <- readRef cap1P
>                                 uCap1 <- readStruct cap1 "u"
>                                 cap1Struct <- readUnion uCap1 $! lower $! typeCap
>                                 fieldCap1 <- readStruct cap1Struct field
>                         
>                                 cap2P <- readRef cap2PP
>                                 cap2 <- readRef cap2P
>                                 uCap2 <- readStruct cap2 "u"
>                                 cap2Struct <- readUnion uCap2 $! lower $! typeCap
>                                 fieldCap2 <- readStruct cap2Struct field
>                                
>                                 return $! fieldCap1 .==. fieldCap2)
>
> copyCase _ _ (_, caseCodeV, AlwaysEqual) =
>     do 
>       return $! (caseCodeV,
>                  returnc true)
>
> copyCase _ _ (_, caseCodeV, AlwaysDistinct) =
>     do 
>       return $! (caseCodeV,
>                  returnc false)

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


> -- Generate an expression for the size of a value expressed in "bits"
> -- i.e. (((genpaddr_t)1) << bits)
> mkSize :: PureExpr -> PureExpr
> mkSize bits = (cast genpaddrT (uint64 1)) .<<. bits


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
