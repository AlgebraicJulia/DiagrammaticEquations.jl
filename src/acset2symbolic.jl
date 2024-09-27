using DiagrammaticEquations
using ACSets
using SymbolicUtils
using SymbolicUtils.Rewriters
using SymbolicUtils.Code
using MLStyle

import SymbolicUtils: BasicSymbolic, Symbolic

export symbolics_lookup, extract_symexprs, apply_rewrites, merge_equations, to_acset, symbolic_rewriting, symbolics_lookup

const DECA_EQUALITY_SYMBOL = (==)

function symbolics_lookup(d::SummationDecapode)
  Dict{Symbol, BasicSymbolic}(map(parts(d, :Var)) do i
    (d[i, :name], decavar_to_symbolics(d, i))
  end)
end

function decavar_to_symbolics(d::SummationDecapode, index::Int; space = :I)
  new_type = SymbolicUtils.symtype(Deca.DECQuantity, d[index, :type], space)
  SymbolicUtils.Sym{new_type}(d[index, :name])
end

function to_symbolics(d::SummationDecapode, symvar_lookup::Dict{Symbol, BasicSymbolic}, op_index::Int, op_type::Symbol)
  input_syms = getindex.(Ref(symvar_lookup), d[edge_inputs(d,op_index,Val(op_type)), :name])
  output_sym = getindex.(Ref(symvar_lookup), d[edge_output(d,op_index,Val(op_type)), :name])

  op_sym = getfield(@__MODULE__, edge_function(d,op_index,Val(op_type)))

  S = promote_symtype(op_sym, input_syms...)
  rhs = SymbolicUtils.Term{S}(op_sym, input_syms)
  SymbolicEquation{Symbolic}(output_sym, rhs)
end

function symbolic_rewriting(old_d::SummationDecapode, rewriter = nothing)
  d = infer_types!(deepcopy(old_d))

  symvar_lookup = symbolics_lookup(d)
  eqns = merge_equations(d, symvar_lookup, extract_symexprs(d, symvar_lookup))

  to_acset(d, isnothing(rewriter) ? eqns : map(rewriter, eqns))
end

function extract_symexprs(d::SummationDecapode, symvar_lookup::Dict{Symbol, BasicSymbolic})
  non_tangents = filter(x -> retrieve_name(d, x) != DerivOp, topological_sort_edges(d))
  map(non_tangents) do node
    to_symbolics(d, symvar_lookup, node.index, node.name)
  end
end

function merge_equations(d::SummationDecapode, symvar_lookup::Dict{Symbol, BasicSymbolic}, symexpr_list::Vector{SymbolicEquation{Symbolic}})
  final_list = []

  eqn_lookup = Dict{Any,Any}(map(start_nodes(d)) do node
    sym = symvar_lookup[d[node, :name]]
    (sym, sym)
  end)

  for expr in symexpr_list

    # XXX SymbolicUtils.substitute swaps the order of multiplication.
    # example: @decapode begin
    #   u::Form0
    #   G::Form0
    #   κ::Constant
    #   ∂ₜ(G) == κ*★(d(★(d(u))))
    # end
    # will have the kappa*var term rewritten to var*kappa
    merged_rhs = SymbolicUtils.substitute(expr.rhs, eqn_lookup)

    push!(eqn_lookup, (expr.lhs => merged_rhs))

    if expr.lhs.name in infer_terminal_names(d)
      push!(final_list, formed_deca_eqn(expr.lhs, merged_rhs))
    end
  end

  final_list
end

formed_deca_eqn(lhs, rhs) = SymbolicUtils.Term{Number}(DECA_EQUALITY_SYMBOL, [lhs, rhs])

function apply_rewrites(symexprs, rewriter)
  map(symexprs) do sym
    res_sym = rewriter(sym)
    isnothing(res_sym) ? sym : res_sym
  end
end

function to_acset(d::SummationDecapode, sym_exprs)
  #TODO: This step is breaking up summations
  final_exprs = SymbolicUtils.Code.toexpr.(sym_exprs)

  recursive_descent = @λ begin
    e::Expr => begin
      if e.head == :call
        e.args[1] = nameof(e.args[1])
        map(recursive_descent, e.args[2:end])
      end
    end
    sym => nothing
  end
  foreach(recursive_descent, final_exprs)

  states_terminals = map([infer_states(d)..., infer_terminals(d)...]) do idx
    :($(d[idx, :name])::$(d[idx, :type]))
  end

  tangents = map(incident(d, DerivOp, :op1)) do op1
    :($(d[d[op1, :tgt], :name]) == $DerivOp($(d[d[op1, :src], :name])))
  end

  deca_block = quote end
  deca_block.args = [states_terminals..., tangents..., final_exprs...]
  infer_types!(SummationDecapode(parse_decapode(deca_block)))
end

