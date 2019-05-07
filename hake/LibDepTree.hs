--------------------------------------------------------------------------
-- Copyright (c) 2007-2019, ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:
-- ETH Zurich D-INFK, Universitaetstasse 6, CH-8092 Zurich. Attn: Systems Group.
--
-- Default architecture-specific definitions for Barrelfish
--
--------------------------------------------------------------------------

module LibDepTree where

import HakeTypes
import qualified Data.Set as S
import Data.Array (indices)
import Data.Graph
import Data.Maybe
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import qualified RuleDefs -- for libraryPath and applicationPath


type DepElMap = Map.Map DepEl [DepEl]
data LibDepTree2 = LibDepTree2Graph {
  gGraph :: Graph,
  gNodeFromVertex :: Vertex -> (DepEl, DepEl, [DepEl]),
  gVertexFromKey :: DepEl -> Maybe Vertex
}

-- Given an [HRule], extract a LibDepTree2
ldtEmToGraph :: DepElMap -> LibDepTree2
ldtEmToGraph tree = LibDepTree2Graph graph nodeFromVertex vertexFromKey
  where
    (graph, nodeFromVertex, vertexFromKey) = graphFromEdges (wchild ++ wochild)
    wchild = [(a,a,reverse b) | (a,b) <- Map.toList tree]
    wochild = [(c,c,[]) | c <- concat [cs | (_,cs) <- Map.toList tree],
                Map.notMember c tree]

ldtDepElMerge :: DepElMap -> DepElMap -> DepElMap
ldtDepElMerge = Map.unionWith (++)

ldtHRuleToDepElMap :: [String] -> HRule -> DepElMap
ldtHRuleToDepElMap archs hrule = case hrule of
      (Rules rules)        -> foldr ldtDepElMerge Map.empty $ map (ldtHRuleToDepElMap archs) rules
      (HakeTypes.Rule rts) -> foldr ldtDepElMerge Map.empty $ map ext_rt rts
      _                    -> Map.empty

      where
        ext_rt :: RuleToken -> DepElMap
        ext_rt rt = case rt of
          (LDep a b) -> if elem (depElArch a) archs then Map.singleton a [b] else Map.empty
          _ -> Map.empty



ldtFromJust :: Maybe a -> String ->  a
ldtFromJust Nothing err = errorWithoutStackTrace ("No Graph Node for " ++ err) -- yuck
ldtFromJust (Just x) err = x

ldtDepOf :: LibDepTree2 -> DepEl -> [DepEl]
ldtDepOf gr = sortBy rtCmp . map extractDepEl . mTopSort . mVertexFromKey
   where
     triFst (a,_,_) = a
     extractDepEl = triFst . nodeFromVertex
     (graph, nodeFromVertex, vertexFromKey) = (gGraph gr, gNodeFromVertex gr, gVertexFromKey gr)

     mVertexFromKey :: DepEl -> Vertex
     mVertexFromKey x = ldtFromJust (vertexFromKey x) ("No node " ++ show x)

     -- We move the DepMods to the front, so the --whole-archive will be
     -- linked first
     rtCmp :: DepEl -> DepEl -> Ordering
     rtCmp (DepMod _ _) (DepLib _ _) = LT
     rtCmp (DepLib _ _) (DepMod _ _) = GT
     rtCmp _ _  = EQ

     -- The following functions are copied from Data.Graph and extended with
     -- an additional "start" vertex
     postorder :: Tree a -> [a] -> [a]
     postorder (Node a ts) = postorderF ts . (a :)

     postorderF   :: Forest a -> [a] -> [a]
     postorderF ts = foldr (.) id $ map postorder ts

     mPostOrd :: Vertex -> [Vertex]
     mPostOrd start = postorderF (mDff) []
       where
         mDff = dfs graph [start]

     mTopSort :: Vertex -> [Vertex]
     mTopSort start = tail $ reverse $  mPostOrd start

ldtDepDfs :: LibDepTree2 -> DepEl -> Tree DepEl
ldtDepDfs ldt start = fmap extractDepEl $ head (dfs graph [fromJust $ vertexFromKey start])
  where
    triFst (a,_,_) = a
    extractDepEl = triFst . nodeFromVertex
    (graph, nodeFromVertex, vertexFromKey) = (gGraph ldt, gNodeFromVertex ldt, gVertexFromKey ldt)

ldtDriverModules :: LibDepTree2 -> [(DepEl,DepEl)]
-- concat $ map depS (indices $ gGraph t)
ldtDriverModules t = depS =<< indices (gGraph t)
  where
    triFst (a,_,_) = a
    toNode :: Vertex -> DepEl
    toNode = triFst . gNodeFromVertex t

    depS :: Vertex -> [(DepEl,DepEl)]
    depS = step . ldtDepDfs t . toNode

    step :: Tree DepEl -> [(DepEl,DepEl)]
    step (Node (DepApp arch name) cs) = [ ((DepApp arch name),x) | x <- concat $ map nm cs]
      where
        nm (Node (DepMod arch name) _) = [DepMod arch name]
        nm _ = []
    step _ = []

ldtPrettyTree :: Tree DepEl -> String
ldtPrettyTree tr = prettyPrintR tr ""
  where
    prettyPrintR (Node lbl cs) ind = (ind ++ (show lbl)) ++ "\n" ++
                         (concat $ [prettyPrintR c ("+ " ++ ind) | c <- cs])

-- Replace the Ldt tokens with In rule tokens
ldtRuleExpand :: LibDepTree2 -> HRule -> HRule
ldtRuleExpand ldt x = case x of
  Rule tokens -> Rule $ concat [ldtTokenExpand ldt x | x <- tokens]
  Phony s b tokens -> Phony s b $ concat [ldtTokenExpand ldt x | x <- tokens]
  Rules rules -> Rules [ldtRuleExpand ldt x | x <- rules]
  _ -> x -- .. that means we dont support Ldt tokens under an Include


ldtTokenExpand :: LibDepTree2 -> RuleToken -> [RuleToken]
ldtTokenExpand ldt x = case x of
   Ldt tree arch app -> concat $ map toRT $ ldtDepOf ldt (DepApp arch app)
     where
       toRT :: DepEl -> [RuleToken]
       -- XXX: I couldnt figure out where we usually prefix the architecture
       toRT (DepApp xarch x) = [In tree arch $
           ("./" ++ arch ++ (RuleDefs.applicationPath (RuleDefs.options arch) x))]
       toRT (DepLib xarch x) = [In tree arch $
           ("./" ++ arch ++ (RuleDefs.libraryPath (RuleDefs.options arch) x))]
       toRT (DepMod xarch x) =
           [Str "-Wl,--whole-archive"] ++
           (toRT (DepLib xarch x)) ++
           [Str "-Wl,--no-whole-archive"]
   _ -> [x]


ldtDebug :: LibDepTree2 -> DepEl -> IO ()
ldtDebug ldt el = do
  putStrLn $ "Deps of " ++ (show el) ++ ":"
  putStrLn $ ldtPrettyTree $ ldtDepDfs ldt el
  putStrLn $ show $ ldtDepOf ldt el
  return ()
