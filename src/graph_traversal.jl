using DiagrammaticEquations
using ACSets

export TraversalNode, topological_sort_edges, n_ops, retrieve_name, start_nodes, edge_inputs, edge_output, edge_function, set_edge_label

struct TraversalNode{T}
  index::Int
  name::T
end

edge_inputs(d::SummationDecapode, idx::Int, ::Val{:Op1}) =
  [d[idx,:src]]
edge_inputs(d::SummationDecapode, idx::Int, ::Val{:Op2}) =
  [d[idx,:proj1], d[idx,:proj2]]
edge_inputs(d::SummationDecapode, idx::Int, ::Val{:Σ}) =
  d[incident(d, idx, :summation), :summand]

edge_output(d::SummationDecapode, idx::Int, ::Val{:Op1}) =
  d[idx,:tgt]
edge_output(d::SummationDecapode, idx::Int, ::Val{:Op2}) =
  d[idx,:res]
edge_output(d::SummationDecapode, idx::Int, ::Val{:Σ}) =
  d[idx, :sum]

edge_function(d::SummationDecapode, idx::Int, ::Val{:Op1}) =
  d[idx,:op1]
edge_function(d::SummationDecapode, idx::Int, ::Val{:Op2}) =
  d[idx,:op2]
edge_function(d::SummationDecapode, idx::Int, ::Val{:Σ}) =
  :+

set_edge_label(d::SummationDecapode, idx::Int, new_label, ::Val{:Op1}) =
  (d[idx,:op1] = new_label)

set_edge_label(d::SummationDecapode, idx::Int, new_label, ::Val{:Op2}) =
(d[idx,:op2] = new_label)

set_edge_label(d::SummationDecapode, idx::Int, new_label, ::Val{:Σ}) = nothing


#XXX: This topological sort is O(n^2).
function topological_sort_edges(d::SummationDecapode)
  visited_Var = falses(nparts(d, :Var))
  visited_Var[start_nodes(d)] .= true
  visited = Dict(:Op1 => falses(nparts(d, :Op1)),
    :Op2 => falses(nparts(d, :Op2)), :Σ => falses(nparts(d, :Σ)))

  op_order = TraversalNode{Symbol}[]

  function visit(op, op_type)
    if !visited[op_type][op] && all(visited_Var[edge_inputs(d,op,Val(op_type))])
      visited[op_type][op] = true
      visited_Var[edge_output(d,op,Val(op_type))] = true
      push!(op_order, TraversalNode(op, op_type))
    end
  end

  for _ in 1:n_ops(d)
    visit.(parts(d,:Op1), :Op1)
    visit.(parts(d,:Op2), :Op2)
    visit.(parts(d,:Σ), :Σ)
  end

  @assert length(op_order) == n_ops(d)
  op_order
end

n_ops(d::SummationDecapode) =
  nparts(d, :Op1) + nparts(d, :Op2) + nparts(d, :Σ)

start_nodes(d::SummationDecapode) =
  vcat(infer_states(d), incident(d, :Literal, :type))

function retrieve_name(d::SummationDecapode, tsr::TraversalNode)
  @match tsr.name begin
    :Op1 => d[tsr.index, :op1]
    :Op2 => d[tsr.index, :op2]
    :Σ => :+
    _ => error("$(tsr.name) is a table without names")
  end
end
