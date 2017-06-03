{-# LANGUAGE NoImplicitPrelude #-}

module Exercises where

import Prelude
  (Int
  ,Ord
  ,Eq
  ,flip
  ,undefined
  ,($)
  ,(.)
  ,(<$>)
  ,(<*>)
  ,(>>=))

import Data.Array
  (Array
  ,accumArray
  ,indices
  ,assocs
  ,bounds)

-- A Practical Approach to Graph Manipulation
--
-- source: https://wiki.haskell.org/The_Monad.Reader/Issue5/Practical_Graph_Handling

type Vertex
  = Int

type Table a
  = Array Vertex a

type Graph e
  = Table [(e, Vertex)]

type Bounds
  = (Vertex, Vertex)

type Edge e
  = (Vertex, e, Vertex)

type Labeling a
  = Vertex -> a

data LabGraph n e
  = LabGraph (Graph e) (Labeling n)

vertices :: LabGraph n e -> [Vertex]
vertices (LabGraph g _) =
  indices g

labels :: LabGraph n e -> [n]
labels (LabGraph g l) =
  l <$> indices g

edges :: Graph e -> [Edge e]
edges g =
 [(i, e, v) | (i, es) <- assocs g, (e, v) <- es]

buildG :: Bounds -> [Edge e] -> Graph e
buildG b es =
  accumArray snoc [] b [(i, (e, v)) | (i, e, v) <- es]
  where
    snoc = flip (:)

-- $setup
-- >>> let g = buildG (0,3) [(1, "e1", 0), (3, "e2", 2)]
--
-- | transposeG
--
-- >>> transposeG g
-- array (0,3) [(0,[("e1",1)]),(1,[]),(2,[("e2",3)]),(3,[])]
--
-- >>> (transposeG . transposeG) g
-- array (0,3) [(0,[]),(1,[("e1",0)]),(2,[]),(3,[("e2",2)])]

transposeG :: Graph e -> Graph e
transposeG g =
  buildG (bounds g) [(v, e, i) | (i, e, v) <- edges g]

unfoldG :: (Ord s) => (s -> (n, [(e, s)])) -> s -> (Vertex, LabGraph n e)
unfoldG =
  undefined

foldG :: (Eq r) => r -> (Vertex -> [(e, r)] -> r) -> Graph e -> Vertex -> r
foldG =
  undefined
