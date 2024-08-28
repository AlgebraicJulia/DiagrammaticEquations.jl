using DiagrammaticEquations
using SymbolicUtils
using SymbolicUtils.Rewriters
using SymbolicUtils.Code
using MLStyle

import DiagrammaticEquations: HyperGraph, HyperGraphEdge, HyperGraphVertex, vertex_list, edge_list
import DiagrammaticEquations: topological_sort_edges

export TableData, extract_symexprs, number_of_ops, retrieve_name

const DECA_EQUALITY_SYMBOL = (==)

# Decapode graph conversion
struct TableData
  table_index::Int
  table_name::Symbol
end

HyperGraphVertex(d::SummationDecapode, index::Int) = HyperGraphVertex(index, TableData(index, :Var))

HyperGraphEdge(d::SummationDecapode, index::Int, ::Val{:Op1}) = HyperGraphEdge(d[index, :tgt], [d[index, :src]], TableData(index, :Op1))
HyperGraphEdge(d::SummationDecapode, index::Int, ::Val{:Op2}) = HyperGraphEdge(d[index, :res], [d[index,:proj1],d[index,:proj2]], TableData(index, :Op2))
HyperGraphEdge(d::SummationDecapode, index::Int, ::Val{:Σ}) = HyperGraphEdge(d[index, :sum], d[incident(d, index, :summation), :summand], TableData(index, :Σ))

HyperGraph(d::SummationDecapode) = HyperGraph(vertex_list(d), edge_list(d), nothing)

vertex_list(d::SummationDecapode) = map(id -> HyperGraphVertex(d, id), parts(d, :Var))

function edge_list(d::SummationDecapode)
  edges = HyperGraphEdge[]
  for op_table in [:Op1, :Op2, :Σ]
    for op in parts(d, op_table)
      if op_table == :Op1 && d[op, :op1] == DerivOp
        continue
      end
      push!(edges, HyperGraphEdge(d, op, Val(op_table)))
    end
  end
  edges
end

table_data(v::HyperGraphVertex) = v.metadata
table_data(v::HyperGraphEdge) = v.metadata

topological_sort_edges(d::SummationDecapode) = table_data.(topological_sort_edges(HyperGraph(d)))

# Decapode ACSet symbolics conversion

to_symbolics(d::SummationDecapode, data::TableData) = to_symbolics(d, data.table_index, Val(data.table_name))

function to_symbolics(d::SummationDecapode, op_index::Int, ::Val{:Op1})
  input_sym = SymbolicUtils.Sym{Number}(d[d[op_index, :src], :name])
  output_sym = SymbolicUtils.Sym{Number}(d[d[op_index, :tgt], :name])
  op_sym = SymbolicUtils.Sym{(SymbolicUtils.FnType){Tuple{Number}, Number}}(d[op_index, :op1])

  rhs = SymbolicUtils.Term{Number}(op_sym, [input_sym])
  SymbolicUtils.Term{Number}(DECA_EQUALITY_SYMBOL, [output_sym, rhs])
end

function to_symbolics(d::SummationDecapode, op_index::Int, ::Val{:Op2})
  input1_sym = SymbolicUtils.Sym{Number}(d[d[op_index, :proj1], :name])
  input2_sym = SymbolicUtils.Sym{Number}(d[d[op_index, :proj2], :name])
  output_sym = SymbolicUtils.Sym{Number}(d[d[op_index, :res], :name])
  op_sym = SymbolicUtils.Sym{(SymbolicUtils.FnType){Tuple{Number, Number}, Number}}(d[op_index, :op2])

  rhs = SymbolicUtils.Term{Number}(op_sym, [input1_sym, input2_sym])
  SymbolicUtils.Term{Number}(DECA_EQUALITY_SYMBOL, [output_sym, rhs])
end

#XXX: Always converting + -> .+ here since summation doesn't store the style of addition
# function to_symbolics(d::SummationDecapode, op_index::Int, ::Val{:Σ})
#   Expr(EQUALITY_SYMBOL, c.output, Expr(:call, Expr(:., :+), c.inputs...))
# end

function extract_symexprs(d::SummationDecapode)
  topo_list = topological_sort_edges(d)
  sym_list = []
  for node in topo_list
    retrieve_name(d, node) != DerivOp || continue
    push!(sym_list, to_symbolics(d, node))
  end
  sym_list
end

function apply_rewrites(d::SummationDecapode, rewriter)

  rewritten_list = []
  for sym in extract_symexprs(d)
    res_sym = rewriter(sym)
    rewritten_sym = isnothing(res_sym) ? sym : res_sym
    push!(rewritten_list, rewritten_sym)
  end

  rewritten_list
end

function number_of_ops(d::SummationDecapode)
  return nparts(d, :Op1) + nparts(d, :Op2) + nparts(d, :Σ)
end

function retrieve_name(d::SummationDecapode, data::TableData)
  @match data.table_name begin
    :Op1 => d[data.table_index, :op1]
    :Op2 => d[data.table_index, :op2]
    :Σ => :+
    _ => error("$(data.table_name) is a table without names")
  end
end

# TODO: We need a way to get information like the d and ⋆ even when not in the ACSet
# @syms Δ(x) d(x) ⋆(x)
# lap_0_rule = @rule Δ(~x) => ⋆(d(⋆(d(~x))))
# rewriter = Postwalk(RestartedChain([lap_0_rule]))
