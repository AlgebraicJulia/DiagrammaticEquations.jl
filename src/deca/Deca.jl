module Deca
using ..DiagrammaticEquations
using Catlab

include("deca_acset.jl")
include("deca_visualization.jl")

export normalize_unicode, varname, infer_states, infer_types!, typename, spacename, recursive_delete_parents, recursive_delete_parents!, unicode!, replace_names!, op1_res_rules_1D,
op2_res_rules_1D,
op1_res_rules_2D,
op2_res_rules_2D,
op1_inf_rules_1D,
op2_inf_rules_1D,
op1_inf_rules_2D,
op2_inf_rules_2D,
apply_inference_rule_op1!, apply_inference_rule_op2!

## TODO: where?
function infer_states(d::SummationDecapode)
    filter(parts(d, :Var)) do v
        length(incident(d, v, :tgt)) == 0 &&
        length(incident(d, v, :res)) == 0 &&
        length(incident(d, v, :sum)) == 0 &&
        d[v, :type] != :Literal
    end
end

infer_state_names(d) = d[infer_states(d), :name]


"""
    function recursive_delete_parents!(d::SummationDecapode, to_delete::Vector{Int64})

Delete the given nodes and their parents in the Decapode, recursively.
"""
function recursive_delete_parents(d::SummationDecapode, to_delete::Vector{Int64})
  e = SummationDecapode{Any, Any, Symbol}()
  copy_parts!(e, d, (:Var, :TVar, :Op1, :Op2, :Σ, :Summand))
  isempty(to_delete) || recursive_delete_parents!(e, to_delete)
  return e
end

function recursive_delete_parents!(d::SummationDecapode, to_delete::Vector{Int64})
  # TODO: We assume to_delete vars have no children. Is that okay?
  # TODO: Explicitly check that a Var in to_delete is not a summand.
  vars_to_remove = Vector{Int64}()
  s = Stack{Int64}()
  foreach(v -> push!(s, v), to_delete)
  while true
    curr = pop!(s)
    parents = reduce(vcat,
                     [d[incident(d, curr, :tgt), :src],
                      d[incident(d, curr, :res), :proj1],
                      d[incident(d, curr, :res), :proj2],
                      d[incident(d, curr, [:summation, :sum]), :summand]])

    # Remove the operations which have curr as the result.
    rem_parts!(d, :TVar,    incident(d, curr, :incl))
    rem_parts!(d, :Op1,     incident(d, curr, :tgt))
    rem_parts!(d, :Op2,     incident(d, curr, :proj1))
    rem_parts!(d, :Op2,     incident(d, curr, :proj2))
    rem_parts!(d, :Op2,     incident(d, curr, :res))
    # Note: Delete Summands before Σs.
    rem_parts!(d, :Summand, incident(d, curr, [:summation, :sum]))
    rem_parts!(d, :Σ,       incident(d, curr, :sum))

    # Do not remove parents which are used in some other computation. We rely
    # on the fact that a parent is guaranteed to point to curr.
    filter!(parents) do p
      # p must not be the src of any Op1.
      isempty(incident(d, p, :src)) &&
      # p must not be a proj of any Op2.
      isempty(incident(d, p, :proj1)) &&
      isempty(incident(d, p, :proj2)) &&
      # p must not be a summand of any summation.
      isempty(incident(d, p, :summand))
    end
    foreach(p -> push!(s, p), parents)

    push!(vars_to_remove, curr)

    isempty(s) && break
  end
  rem_parts!(d, :Var, sort!(unique!(vars_to_remove)))
end
end
