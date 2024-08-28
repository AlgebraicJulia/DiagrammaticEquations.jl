using DiagrammaticEquations
using ACSets

export HyperGraph, HyperGraphVertex, HyperGraphEdge, vertex_list, edge_list
export topological_sort_edges, floyd_warshall

struct HyperGraphVertex
  id::Int
  metadata
end

# Assuming we only have a single target
struct HyperGraphEdge
  tgt::Int
  srcs::AbstractVector{Int}
  metadata
end

struct HyperGraph
  vertices::AbstractVector{HyperGraphVertex}
  edges::AbstractVector{HyperGraphEdge}
  metadata
end

# Returns a list of all vertices from ACSet as HyperGraphVertex
function vertex_list() end

# Returns a list of all edges from ACSet as HyperGraphEdge
function edge_list() end

num_vertices(g::HyperGraph) = length(g.vertices)
num_edges(g::HyperGraph) = length(g.edges)

# TODO: Clean this up to use better logic
function start_nodes(g::HyperGraph)
  indices = HyperGraphVertex[]

  for vertex in g.vertices
    v_id = vertex.id

    is_tgt = true
    for edge in g.edges
      if v_id == edge.tgt
        is_tgt = false
        break
      end
    end

    if is_tgt
      push!(indices, vertex)
    end

  end

  indices
end

function has_unique_targets(g::HyperGraph)
  seen_vertices = Set{Int}()
  for edge in g.edges
    if edge.tgt in seen_vertices
      return false
    end
    push!(seen_vertices, edge.tgt)
  end
  return true
end

vertex_id(v::HyperGraphVertex) = return v.id

function topological_sort_edges(g::HyperGraph)
  @assert has_unique_targets(g)

  visited_vertices = falses(num_vertices(g))
  visited_vertices[vertex_id.(start_nodes(g))] .= true

  visited_edges = falses(num_edges(g))

  edge_order = HyperGraphEdge[]

  for _ in 1:num_edges(g)
    for (idx, edge) in enumerate(g.edges)
      if !visited_edges[idx] && all(visited_vertices[edge.srcs])
        visited_edges[idx] = true
        visited_vertices[edge.tgt] = true

        push!(edge_order, edge)
      end
    end
  end

  @assert length(edge_order) == num_edges(g)

  edge_order
end

"""
floyd_warshall(g::HyperGraph)

Return a |variable| × |variable| matrix of shortest paths via the Floyd-Warshall algorithm.

Taking the maximum of the non-infinite short paths from state variables induces a topological ordering.

https://en.wikipedia.org/wiki/Floyd–Warshall_algorithm
"""
function floyd_warshall(g::HyperGraph)
  # Define weights.
  w(e) = -1

  # Init dists
  V = num_vertices(g)
  dist = fill(Inf, (V, V))
  foreach(g.edges) do e
    dist[(e.srcs), e.tgt] .= w(e)
  end

  for v in 1:V
    dist[v,v] = 0
  end

  # Floyd-Warshall
  for k in 1:V
    for i in 1:V
      for j in 1:V
        if dist[i,j] > dist[i,k] + dist[k,j]
          dist[i,j] = dist[i,k] + dist[k,j]
        end
      end
    end
  end

  dist
end
