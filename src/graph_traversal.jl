using DiagrammaticEquations
using ACSets

export TraversalNode, topological_sort_edges, number_of_ops, retrieve_name

struct TraversalNode{T}
  index::Int
  name::T
  dom::AbstractVector
  cod::Int
end

TraversalNode(i, d::SummationDecapode, ::Val{:Op1}) =
  TraversalNode{Symbol}(i, d[i,:op1], [d[i,:src]], d[i,:tgt])

TraversalNode(i, d::SummationDecapode, ::Val{:Op2}) =
  TraversalNode{Symbol}(i, d[i,:op2], [d[i,:proj1],d[i,:proj2]], d[i,:res])

TraversalNode(i, d::SummationDecapode, ::Val{:Σ}) =
  TraversalNode{Symbol}(i, :+, d[incident(d,i,:summation),:summand], d[i,:sum])

retrieve_name(d::SummationDecapode, tsr::TraversalNode) = tsr.name

# Induce a topological ordering of operations from a topological ordering of variables.
# Taking Vᵢ(dom(e)ᵢ) like so is a structure preserving map.
edge2cost(tsv, tsr::TraversalNode) = maximum(tsv[tsr.dom])

number_of_ops(d::SummationDecapode) = nparts(d, :Op1) + nparts(d, :Op2) + nparts(d, :Σ)

start_nodes(d::SummationDecapode) = vcat(infer_states(d), incident(d, :Literal, :type))

# TODO: This could be Catlab'd. Hypergraph category? Migration to a DWD?
"""    function hyper_edge_list(d::SummationDecapode)

Represent a Decapode as a directed hyper-edge list.

Interpret a:
  - unary operation as a hyperedge of order (1,1) ,
  - binary operation as a hyperedge of order (2,1) , and
  - summation as a hyperedge of order (|summands|,1) .
"""
function hyper_edge_list(d::SummationDecapode)
  [map(e -> TraversalNode(e, d, Val(:Op1)), parts(d, :Op1))...,
   map(e -> TraversalNode(e, d, Val(:Op2)), parts(d, :Op2))...,
   map(e -> TraversalNode(e, d, Val(:Σ  )), parts(d, :Σ  ))...]
end

"""    function floyd_warshall(d::SummationDecapode)

Return a |variable| × |variable| matrix of shortest paths via the Floyd-Warshall algorithm.

Taking the maximum of the non-infinite short paths from state variables induces a topological ordering.

https://en.wikipedia.org/wiki/Floyd–Warshall_algorithm
"""
function floyd_warshall(d::SummationDecapode)
  # Define weights.
  w(e) = (length(e.dom) == 1 && e.name ∈ [:∂ₜ,:dt]) ? -Inf : -1
  # Init dists
  V = nparts(d, :Var)
  dist = fill(Inf, (V, V))
  foreach(hyper_edge_list(d)) do e
    dist[(e.dom), e.cod] .= w(e)
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

"""    function topological_sort_verts(d::SummationDecapode)

Topologically sort the variables in a Decapode.

The vector returned by this function maps each vertex to the order that it would be traversed in a topological sort traversal. If you want a list of vertices in the order of traversal, call `sortperm` on the output.
"""
function topological_sort_verts(d::SummationDecapode)
  m = floyd_warshall(d)
  map(parts(d,:Var)) do v
    minimum(filter(!isinf, m[v,infer_terminals(d)]))
  end
end

# TODO: Add in-place version for sorting a given hyperedge list.
"""    function topological_sort_edges(d::SummationDecapode)

Topologically sort the edges in a Decapode.
"""
function topological_sort_edges(d::SummationDecapode)
  tsv = topological_sort_verts(d)
  op_order = hyper_edge_list(d)
  sort(op_order, by = x -> edge2cost(tsv,x))
end

