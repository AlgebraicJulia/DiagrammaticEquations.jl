using DiagrammaticEquations
using SymbolicUtils
using SymbolicUtils.Rewriters
using SymbolicUtils.Code
using MLStyle

import DiagrammaticEquations.ThDEC: Space

export extract_symexprs, apply_rewrites, merge_equations

const DECA_EQUALITY_SYMBOL = (==)

to_symbolics(d::SummationDecapode, node::TraversalNode) = to_symbolics(d, node.index, Val(node.name))

function to_symbolics(d::SummationDecapode, op_index::Int, ::Val{:Op1})
  input_sym = SymbolicUtils.Sym{Number}(d[d[op_index, :src], :name])
  output_sym = SymbolicUtils.Sym{Number}(d[d[op_index, :tgt], :name])
  op_sym = SymbolicUtils.Sym{(SymbolicUtils.FnType){Tuple{Number}, Number}}(d[op_index, :op1])

  input_sym = setmetadata(input_sym, Sort, oldtype_to_new(d[d[op_index, :src], :type]))
  output_sym = setmetadata(output_sym, Sort, oldtype_to_new(d[d[op_index, :tgt], :type]))

  rhs = SymbolicUtils.Term{Number}(op_sym, [input_sym])
  SymbolicUtils.Term{Number}(DECA_EQUALITY_SYMBOL, [output_sym, rhs])
end

function to_symbolics(d::SummationDecapode, op_index::Int, ::Val{:Op2})
  input1_sym = SymbolicUtils.Sym{Number}(d[d[op_index, :proj1], :name])
  input2_sym = SymbolicUtils.Sym{Number}(d[d[op_index, :proj2], :name])
  output_sym = SymbolicUtils.Sym{Number}(d[d[op_index, :res], :name])
  op_sym = SymbolicUtils.Sym{(SymbolicUtils.FnType){Tuple{Number, Number}, Number}}(d[op_index, :op2])

  input1_sym = setmetadata(input1_sym, Sort, oldtype_to_new(d[d[op_index, :proj1], :type]))
  input2_sym = setmetadata(input2_sym, Sort, oldtype_to_new(d[d[op_index, :proj2], :type]))
  output_sym = setmetadata(output_sym, Sort, oldtype_to_new(d[d[op_index, :res], :type]))

  rhs = SymbolicUtils.Term{Number}(op_sym, [input1_sym, input2_sym])
  SymbolicUtils.Term{Number}(DECA_EQUALITY_SYMBOL, [output_sym, rhs])
end

#XXX: Always converting + -> .+ here since summation doesn't store the style of addition
# function to_symbolics(d::SummationDecapode, op_index::Int, ::Val{:Σ})
#   Expr(EQUALITY_SYMBOL, c.output, Expr(:call, Expr(:., :+), c.inputs...))
# end

function oldtype_to_new(old::Symbol, space::Space = Space(:I, 2))::Sort
  @match old begin
    :Form0 => PrimalForm(0, space)
    :Form1 => PrimalForm(1, space)
    :Form2 => PrimalForm(2, space)

    :DualForm0 => DualForm(0, space)
    :DualForm1 => DualForm(1, space)
    :DualForm2 => DualForm(2, space)

    :Constant => Scalar()
    :Parameter => Scalar()
  end
end

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

function merge_equations(d::SummationDecapode, rewritten_syms)

  lookup = Dict()

  final_list = []

  for node in start_nodes(d)
    sym = SymbolicUtils.Sym{Number}(d[node, :name])
    sym = setmetadata(sym, Sort, oldtype_to_new(d[node, :type]))
    push!(lookup, (sym => sym))
  end

  final_nodes = infer_terminal_names(d)

  for expr in rewritten_syms
    lhs = expr_lhs(expr)
    rhs = expr_rhs(expr)

    recursive_descent(rhs, lookup)

    push!(lookup, (lhs => rhs))

    if lhs.name in final_nodes
      push!(final_list, expr)
    end
  end

  final_list
end

expr_lhs(expr) = expr.arguments[1]
expr_rhs(expr) = expr.arguments[2]

function recursive_descent(expr, lookup)
  # @show expr
  for (i, arg) in enumerate(expr.arguments)
    # @show arg
    if arg in keys(lookup)
      expr.arguments[i] = lookup[arg]
    else
      recursive_descent(arg, lookup)
    end
  end
  return expr
end

function to_acset(og_d, sym_exprs)
  final_exprs = SymbolicUtils.Code.toexpr.(sym_exprs)
  map(x -> x.args[1] = :(==), final_exprs)

  deca_block = quote end

  states = infer_states(og_d)
  terminals = infer_terminals(og_d)

  deca_type_gen = idx -> :($(og_d[idx, :name])::$(og_d[idx, :type]))

  append!(deca_block.args, map(deca_type_gen, vcat(states, terminals)))

  append!(deca_block.args, final_exprs)

  d = SummationDecapode(parse_decapode(deca_block))

  infer_types!(d)
  resolve_overloads!(d)

  d
end

# TODO: We need a way to get information like the d and ⋆ even when not in the ACSet
# @syms Δ(x) d(x) ⋆(x)
# lap_0_rule = @rule Δ(~x) => ⋆(d(⋆(d(~x))))
# rewriter = Postwalk(RestartedChain([lap_0_rule]))
