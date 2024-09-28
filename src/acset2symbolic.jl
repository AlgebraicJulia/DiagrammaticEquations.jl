using DiagrammaticEquations
using ACSets
using SymbolicUtils
using SymbolicUtils: BasicSymbolic, Symbolic

export symbolic_rewriting

const EQUALITY = (==)
const SymEqSym = SymbolicEquation{Symbolic}

function symbolics_lookup(d::SummationDecapode)
  Dict{Symbol, BasicSymbolic}(map(d[:name],d[:type]) do name,type
    (name, decavar_to_symbolics(name, type))
  end)
end

function decavar_to_symbolics(var_name::Symbol, var_type::Symbol; space = :I)
  new_type = SymbolicUtils.symtype(Deca.DECQuantity, var_type, space)
  SymbolicUtils.Sym{new_type}(var_name)
end

function to_symbolics(d::SummationDecapode, symvar_lookup::Dict{Symbol, BasicSymbolic}, op_idx::Int, op_type::Symbol)
  input_syms = getindex.(Ref(symvar_lookup), d[edge_inputs(d,op_idx,Val(op_type)), :name])
  output_sym = getindex.(Ref(symvar_lookup), d[edge_output(d,op_idx,Val(op_type)), :name])
  op_sym = getfield(@__MODULE__, edge_function(d,op_idx,Val(op_type)))

  S = promote_symtype(op_sym, input_syms...)
  SymEqSym(output_sym, SymbolicUtils.Term{S}(op_sym, input_syms))
end

function to_symbolics(d::SummationDecapode)
  symvar_lookup = symbolics_lookup(d)
  map(e -> to_symbolics(d, symvar_lookup, e.index, e.name), topological_sort_edges(d))
end

function symbolic_rewriting(d::SummationDecapode, rewriter=identity)
  d′ = infer_types!(deepcopy(d))
  eqns = merge_equations(d′)
  to_acset(d′, map(rewriter, eqns))
end

# XXX SymbolicUtils.substitute swaps the order of multiplication.
# e.g. ∂ₜ(G) == κ*u becomes ∂ₜ(G) == u*κ
function merge_equations(d::SummationDecapode)
  eqn_lookup, terminal_eqns = Dict(), SymEqSym[]

  foreach(to_symbolics(d)) do x
    sub = SymbolicUtils.substitute(x.rhs, eqn_lookup)
    push!(eqn_lookup, (x.lhs => sub))
    if x.lhs.name in infer_terminal_names(d)
      push!(terminal_eqns, SymEqSym(x.lhs, sub))
    end
  end

  map(terminal_eqns) do eqn
    SymbolicUtils.Term{Number}(EQUALITY, [eqn.lhs, eqn.rhs])
  end
end

function to_acset(d::SummationDecapode, sym_exprs)
  outer_types = map([infer_states(d)..., infer_terminals(d)...]) do i
    :($(d[i, :name])::$(d[i, :type]))
  end

  #TODO: This step is breaking up summations
  final_exprs = SymbolicUtils.Code.toexpr.(sym_exprs)
  reify!(exprs) = foreach(exprs) do x
    if typeof(x) == Expr && x.head == :call
      x.args[1] = nameof(x.args[1])
      reify!(x.args[2:end])
    end
  end
  reify!(final_exprs)

  deca_block = quote end
  deca_block.args = [outer_types..., final_exprs...]
  ∘(infer_types!, SummationDecapode, parse_decapode)(deca_block)
end
