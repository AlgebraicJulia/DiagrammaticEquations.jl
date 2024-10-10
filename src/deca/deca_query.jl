using DiagrammaticEquations
using ACSets

export is_var_target, is_var_source, get_variable_parents, get_next_op1s, get_next_op2s

function is_var_target(d::SummationDecapode, var::Int) 
  return !isempty(collected_incident(d, var, [:tgt, :res, :sum]))
end

function is_var_source(d::SummationDecapode, var::Int) 
  return !isempty(collected_incident(d, var, [:src, :proj1, :proj2, :summand]))
end

function get_variable_parents(d::SummationDecapode, var::Int)
  return collected_incident(d, var, [:tgt, :res, :res, [:summation, :sum]], [:src, :proj1, :proj2, :summand])
end

function get_next_op1s(d::SummationDecapode, var::Int)
  collected_incident(d, var, [:src])
end

function get_next_op2s(d::SummationDecapode, var::Int)
  collected_incident(d, var, [:proj1, :proj2])
end

