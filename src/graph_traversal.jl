using DiagrammaticEquations
using ACSets
using DataStructures

export TraversalNode, topological_sort_edges, number_of_ops, retrieve_name

struct TraversalNode{T}
  index::Int
  name::T
end

number_of_ops(d::SummationDecapode) = nparts(d, :Op1) + nparts(d, :Op2) + nparts(d, :Σ)

start_nodes(d::SummationDecapode) = vcat(infer_states(d), incident(d, :Literal, :type))

#https://en.wikipedia.org/wiki/Floyd–Warshall_algorithm#Pseudocode
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

function topological_sort_verts(d::SummationDecapode)
  m = floyd_warshall(d)
  map(parts(d,:Var)) do v
    maximum(filter(!isinf, m[start_nodes(d),v]))
  end
  # Call sortperm for the vertex ordering.
end

function topological_sort_edges(d::SummationDecapode)
  tsv = topological_sort_verts(d)
  op_order = [TraversalNode.(parts(d,:Op1), :Op1)...,
              TraversalNode.(parts(d,:Op2), :Op2)...,
              TraversalNode.(parts(d,:Σ), :Σ)...]
  function by(x)
    @match x.name begin
      :Op1 => tsv[d[x.index,:src]]
      :Op2 => max(tsv[d[x.index,:proj1]], tsv[d[x.index,:proj1]])
      :Σ => maximum(tsv[d[incident(d,x.index,:summation),:summand]])
      _ => error("Unknown function type")
    end
  end
  sort(op_order, by = by)
end

function retrieve_name(d::SummationDecapode, tsr::TraversalNode)
  @match tsr.name begin
    :Op1 => d[tsr.index, :op1]
    :Op2 => d[tsr.index, :op2]
    :Σ => :+
    _ => error("$(tsr.name) is not a valid table for names")
  end
end

