using DiagrammaticEquations
using SymbolicUtils
using SymbolicUtils.Rewriters
using SymbolicUtils.Code
using MLStyle

const DECA_EQUALITY_SYMBOL = (==)

to_symbolics(d::SummationDecapode, node::TraversalNode) = to_symbolics(d, node.index, Val(node.name))

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

# TODO: We need a way to get information like the d and ⋆ even when not in the ACSet
# @syms Δ(x) d(x) ⋆(x)
# lap_0_rule = @rule Δ(~x) => ⋆(d(⋆(d(~x))))
# rewriter = Postwalk(RestartedChain([lap_0_rule]))
