-- Perform DFS, return list of nodes visited
type topologicalSortState = (Set Node, [Node], AdjacencyList)

topologicalSort :: AdjacencyList -> [Node]
topologicalSort n2ns = topologicalSort' (Set.empty, [], Map.fromList n2ns) n2ns

topologicalSort' :: topologicalSortState -> AdjacencyList -> [Node]
topologicalSort' (_, nodeSequence, _) [] = nodeSequence
topologicalSort' state ((n, ns):n2ns) = topologicalSort' state' n2ns
  where
    state' = processEdges state n (graph ! n)
    (_, _, graph) = state


processEdges :: topologicalSortState -> Node -> [Node] -> topologicalSortState
processEdges state n [] =

processEdges state n (d:ds) = processEdges state' n ds
  where
    state' = processEdge state n d

processEdge state n d =


processEdges state (n:ns) = processEdges
