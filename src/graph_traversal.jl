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

# TODO: If the name of an operator is cached, only one definition is necessary.
retrieve_name(d, tsr::Op1TravNode) = d[tsr.index, :op1]
retrieve_name(d, tsr::Op2TravNode) = d[tsr.index, :op2]
retrieve_name(d, tsr::ΣTravNode) = :+

# TODO: If the codomain of an operator is cached, only one definition is necessary.
# Induce a topological ordering of operations from one of variables.
edge2cost(d, tsv, tsr::Op1TravNode) = tsv[d[tsr.index,:src]]
edge2cost(d, tsv, tsr::Op2TravNode) = max(tsv[d[tsr.index,:proj1]], tsv[d[tsr.index,:proj1]])
edge2cost(d, tsv, tsr::ΣTravNode) = maximum(tsv[d[incident(d,tsr.index,:summation),:summand]])

number_of_ops(d::SummationDecapode) = nparts(d, :Op1) + nparts(d, :Op2) + nparts(d, :Σ)

start_nodes(d::SummationDecapode) = vcat(infer_states(d), incident(d, :Literal, :type))


# TODO: Domain-Codomain pointers could be upstreamed into TraversalNode subtypes. Then this can be cleaned up with multiple dispatch. This would explicitly cache calls to incident.
# TODO: This could be Catlab'd. Hypergraph category? Migration to a DWD?
"""    function hyper_edge_list(d::SummationDecapode)

Represent a Decapode as a directed hyper-edge list.

Interpret a:
  - unary operation as a hyperedge of order (1,1) ,
  - binary operation as a hyperedge of order (2,1) , and
  - summation as a hyperedge of order (|summands|,1) .
"""
function hyper_edge_list(d::SummationDecapode)
  reduce(vcat, [
    map(parts(d,:Op1)) do e
      ([d[e,:src]], d[e,:tgt])
    end,
    map(parts(d,:Op2)) do e
      ([d[e,:proj1],d[e,:proj2]], d[e,:res])
    end,
    map(parts(d,:Σ)) do e
      (d[incident(d,e,:summation),:summand], d[e,:sum])
    end])
end

"""    function floyd_warshall(d::SummationDecapode)

Return a |variable| × |variable| matrix of shortest paths via the Floyd-Warshall algorithm.

Taking the maximum of the non-infinite short paths from state variables induces a topological ordering.

https://en.wikipedia.org/wiki/Floyd–Warshall_algorithm
"""
function floyd_warshall(d::SummationDecapode)
  # Init dists
  V = nparts(d, :Var)
  dist = fill(Inf, (V, V))
  foreach(hyper_edge_list(d)) do e
    dist[e...] .= 1
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

