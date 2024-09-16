using DiagrammaticEquations
using ACSets

export TraversalNode, topological_sort_edges, number_of_ops, retrieve_name, start_nodes

struct TraversalNode{T}
  index::Int
  name::T
end

function topological_sort_edges(d::SummationDecapode)
  visited_Var = falses(nparts(d, :Var))
  visited_Var[start_nodes(d)] .= true

  # TODO: Collect these visited arrays into one structure indexed by :Op1, :Op2, and :Σ
  visited_1 = falses(nparts(d, :Op1))
  visited_2 = falses(nparts(d, :Op2))
  visited_Σ = falses(nparts(d, :Σ))

  # FIXME: this is a quadratic implementation of topological_sort inlined in here.
  op_order = TraversalNode{Symbol}[]

  for _ in 1:number_of_ops(d)
    for op in parts(d, :Op1)
      if !visited_1[op] && visited_Var[d[op, :src]]

        visited_1[op] = true
        visited_Var[d[op, :tgt]] = true

        push!(op_order, TraversalNode(op, :Op1))
      end
    end

    for op in parts(d, :Op2)
      if !visited_2[op] && visited_Var[d[op, :proj1]] && visited_Var[d[op, :proj2]]
        visited_2[op] = true
        visited_Var[d[op, :res]] = true
        push!(op_order, TraversalNode(op, :Op2))
      end
    end

    for op in parts(d, :Σ)
      args = subpart(d, incident(d, op, :summation), :summand)
      if !visited_Σ[op] && all(visited_Var[args])
        visited_Σ[op] = true
        visited_Var[d[op, :sum]] = true
        push!(op_order, TraversalNode(op, :Σ))
      end
    end
  end

  @assert length(op_order) == number_of_ops(d)

  op_order
end

function number_of_ops(d::SummationDecapode)
  return nparts(d, :Op1) + nparts(d, :Op2) + nparts(d, :Σ)
end

function start_nodes(d::SummationDecapode)
  return vcat(infer_states(d), incident(d, :Literal, :type))
end

function retrieve_name(d::SummationDecapode, tsr::TraversalNode)
  @match tsr.name begin
    :Op1 => d[tsr.index, :op1]
    :Op2 => d[tsr.index, :op2]
    :Σ => :+
    _ => error("$(tsr.name) is a table without names")
  end
end
