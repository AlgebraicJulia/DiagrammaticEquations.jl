using DiagrammaticEquations
using ACSets

export TraversalNode, topological_sort_edges, number_of_ops, retrieve_name

abstract type TraversalNode end

struct Op1TravNode <: TraversalNode
  index::Int
end
struct Op2TravNode <: TraversalNode
  index::Int
end
struct ΣTravNode <: TraversalNode
  index::Int
end

retrieve_name(d, tsr::Op1TravNode) = d[tsr.index, :op1]
retrieve_name(d, tsr::Op2TravNode) = d[tsr.index, :op2]
retrieve_name(d, tsr::ΣTravNode) = :+

# Induce a topological ordering of operations from one of variables.
edge2cost(d, tsv, tsr::Op1TravNode) = tsv[d[tsr.index,:src]]
edge2cost(d, tsv, tsr::Op2TravNode) = max(tsv[d[tsr.index,:proj1]], tsv[d[tsr.index,:proj1]])
edge2cost(d, tsv, tsr::ΣTravNode) = maximum(tsv[d[incident(d,tsr.index,:summation),:summand]])

number_of_ops(d::SummationDecapode) = nparts(d, :Op1) + nparts(d, :Op2) + nparts(d, :Σ)

start_nodes(d::SummationDecapode) = vcat(infer_states(d), incident(d, :Literal, :type))

"""    function floyd_warshall(d::SummationDecapode)

Return a |variable| × |variable| matrix of shortest paths via the Floyd-Warshall algorithm.

Taking the maximum of the non-infinite short paths from state variables induces a topological ordering.

https://en.wikipedia.org/wiki/Floyd–Warshall_algorithm
"""
function floyd_warshall(d::SummationDecapode)
  # Init dists.
  V = nparts(d, :Var)
  dist = fill(Inf, (V, V))
  foreach(parts(d,:Op1)) do e
    dist[d[e,:src], d[e,:tgt]] = 1
  end
  foreach(parts(d,:Op2)) do e
    dist[d[e,:proj1], d[e,:res]] = 1
    dist[d[e,:proj2], d[e,:res]] = 1
  end
  foreach(parts(d,:Summand)) do e
    dist[d[e,:summand], d[e,[:summation, :sum]]] = 1
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
    maximum(filter(!isinf, m[start_nodes(d),v]))
  end
end

"""    function topological_sort_edges(d::SummationDecapode)

Topologically sort the edges in a Decapode.
"""
function topological_sort_edges(d::SummationDecapode)
  tsv = topological_sort_verts(d)
  op_order = [Op1TravNode.(parts(d,:Op1))..., Op2TravNode.(parts(d,:Op2))..., ΣTravNode.(parts(d,:Σ))...]
  sort(op_order, by = x -> edge2cost(d,tsv,x))
end

