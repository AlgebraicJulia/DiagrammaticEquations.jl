using DiagrammaticEquations
using ACSets
using SymbolicUtils
using SymbolicUtils: BasicSymbolic, Symbolic

# TODO: Expose only the symbolic_rewriting function
export symbolics_lookup, extract_symexprs, apply_rewrites, merge_equations, to_acset, symbolic_rewriting

const DECA_EQUALITY_SYMBOL = (==)

function symbolics_lookup(d::SummationDecapode)
  Dict{Symbol, BasicSymbolic}(map(parts(d, :Var)) do i
    (d[i, :name], decavar_to_symbolics(d, i))
  end)
end

function decavar_to_symbolics(d::SummationDecapode, idx::Int; space = :I)
  new_type = SymbolicUtils.symtype(Deca.DECQuantity, d[idx, :type], space)
  SymbolicUtils.Sym{new_type}(d[idx, :name])
end

function to_symbolics(d::SummationDecapode, symvar_lookup::Dict{Symbol, BasicSymbolic}, op_idx::Int, op_type::Symbol)
  input_syms = getindex.(Ref(symvar_lookup), d[edge_inputs(d,op_idx,Val(op_type)), :name])
  output_sym = getindex.(Ref(symvar_lookup), d[edge_output(d,op_idx,Val(op_type)), :name])
  op_sym = getfield(@__MODULE__, edge_function(d,op_idx,Val(op_type)))

  S = promote_symtype(op_sym, input_syms...)
  SymbolicEquation{Symbolic}(output_sym, SymbolicUtils.Term{S}(op_sym, input_syms))
end

function symbolic_rewriting(old_d::SummationDecapode, rewriter = nothing)
  d = infer_types!(deepcopy(old_d))
  eqns = merge_equations(d)
  to_acset(d, apply_rewrites(eqns, rewriter))
end

apply_rewrites(eqns, rewriter) = isnothing(rewriter) ? eqns : map(rewriter, eqns)

function extract_symexprs(d::SummationDecapode, symvar_lookup::Dict{Symbol, BasicSymbolic})
  map(topological_sort_edges(d)) do node
    to_symbolics(d, symvar_lookup, node.index, node.name)
  end
end

# XXX SymbolicUtils.substitute swaps the order of multiplication.
# e.g. @decapode begin
#   ∂ₜ(G) == κ*u
# end
# will have the κ*u term rewritten to u*κ
function merge_equations(d::SummationDecapode)
  symvar_lookup = symbolics_lookup(d)
  symexpr_list = extract_symexprs(d, symvar_lookup)

  eqn_lookup = Dict()

  terminal_vars = infer_terminal_names(d)
  terminal_eqns = SymbolicEquation{Symbolic}[]

  foreach(symexpr_list) do x
    push!(eqn_lookup, (x.lhs => SymbolicUtils.substitute(x.rhs, eqn_lookup)))
    if x.lhs.name in terminal_vars
      push!(terminal_eqns, SymbolicEquation{Symbolic}(x.lhs, eqn_lookup[x.lhs]))
    end
  end

  formed_deca_eqn.(terminal_eqns)
end

formed_deca_eqn(symeqn::SymbolicEquation{Symbolic}) = SymbolicUtils.Term{Number}(DECA_EQUALITY_SYMBOL, [symeqn.lhs, symeqn.rhs])

function to_acset(d::SummationDecapode, sym_exprs)
  outer_types = map([infer_states(d)..., infer_terminals(d)...]) do i
    :($(d[i, :name])::$(d[i, :type]))
  end

  #TODO: This step is breaking up summations
  final_exprs = SymbolicUtils.Code.toexpr.(sym_exprs)
  reify!(exprs) = foreach(exprs) do x
    if typeof(x)==Expr && x.head == :call
      x.args[1] = nameof(x.args[1])
      reify!(x.args[2:end])
    end
  end
  reify!(final_exprs)

  deca_block = quote
    $(outer_types...) 
    $(final_exprs...)
  end

  ∘(infer_types!, SummationDecapode, parse_decapode)(deca_block)
end

