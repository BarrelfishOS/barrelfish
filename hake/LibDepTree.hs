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
import Data.Graph
import qualified Data.Map.Strict as Map
import qualified RuleDefs -- for libraryPath and applicationPath

-- Given an [HRule], extract a LibDepTree2
ldtExtract :: [HRule] -> LibDepTree2
ldtExtract rules = foldl merge Map.empty $ map ext rules
  where
    ext :: HRule -> LibDepTree2
    ext hrule = case hrule of
        (Rules rules)        -> foldl merge Map.empty $ map ext rules
        (HakeTypes.Rule rts) -> foldl merge Map.empty $ map extract_rt rts
        _                    -> Map.empty

    extract_rt :: RuleToken -> LibDepTree2
    extract_rt rt = case rt of
      (LDep a b) -> Map.singleton a [b]
      _ -> Map.empty

    merge :: LibDepTree2 -> LibDepTree2 -> LibDepTree2
    merge a b = Map.unionWith (++) a b

-- Calculate the reachable elements from start in topological sorted order
ldtDepOf :: LibDepTree2 -> DepEl -> [DepEl]
ldtDepOf tree start = map extractDepEl $ filter (\el -> (elem el reach) && Just el /= vertexFromKey start) allTopo
  where
    reach :: [Vertex]
    reach = case vertexFromKey start of
      Just x -> reachable graph x
      Nothing -> [] -- This should never happen

    allTopo :: [Vertex]
    allTopo = topSort graph   -- topological sort of all elms

    triFst (a,_,_) = a
    extractDepEl = (\l -> triFst $ nodeFromVertex l)
    (graph, nodeFromVertex, vertexFromKey) = graphFromEdges (wchild ++ wochild)
    wchild = [(a,a,b) | (a,b) <- Map.toList tree]
    wochild = [(c,c,[]) | c <- concat [cs | (_,cs) <- Map.toList tree],
                Map.notMember c tree]


-- Replace the Ldt tokens with include tokens
ldtRuleExpand :: HRule -> LibDepTree2 -> HRule
ldtRuleExpand x ldt = case x of
  Rule tokens -> Rule $ concat [tokenExpand x | x <- tokens]
  Phony s b tokens -> Phony s b $ concat [tokenExpand x | x <- tokens]
  Rules rules -> Rules [ldtRuleExpand x ldt | x <- rules]
  _ -> x -- .. that means we dont support Ldt tokens under an Include
  where
    tokenExpand :: RuleToken -> [RuleToken]
    tokenExpand x = case x of
      Ldt tr arch app -> concat [depToRuleToken tr arch x | x <- ldtDepOf ldt (DepApp arch app)]
      _ -> [x]

    depToRuleToken :: TreeRef -> String -> DepEl -> [RuleToken]
    -- XXX: I couldnt figure out where we usually prefix the architecture
    depToRuleToken tree arch (DepApp xarch x) = [In tree arch $
        ("./" ++ arch ++ (RuleDefs.applicationPath (RuleDefs.options arch) x))]
    depToRuleToken tree arch (DepLib xarch x) = [In tree arch $
        ("./" ++ arch ++ (RuleDefs.libraryPath (RuleDefs.options arch) x))]
    depToRuleToken tree arch (DepMod xarch x) =
        [Str "-Wl,--whole-archive"] ++
        (depToRuleToken tree arch (DepLib xarch x)) ++
        [Str "-Wl,--no-whole-archive"]
