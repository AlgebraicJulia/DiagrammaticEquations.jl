using DiagrammaticEquations
using ACSets
using SymbolicUtils
using SymbolicUtils.Rewriters
using SymbolicUtils.Code
using MLStyle

export extract_symexprs, apply_rewrites, merge_equations, to_acset, symbolic_rewriting, symbolics_lookup

const DECA_EQUALITY_SYMBOL = (==)

to_symbolics(d::SummationDecapode, symvar_lookup::Dict{Symbol, SymbolicUtils.BasicSymbolic}, node::TraversalNode) = to_symbolics(d, symvar_lookup, node.index, Val(node.name))

function symbolics_lookup(d::SummationDecapode)
  lookup = Dict{Symbol, SymbolicUtils.BasicSymbolic}()
  for i in parts(d, :Var)
    push!(lookup, d[i, :name] => decavar_to_symbolics(d, i))
  end
  lookup
end

function decavar_to_symbolics(d::SummationDecapode, index::Int; space = :I)
  var = d[index, :name]
  new_type = SymbolicUtils.symtype(Deca.DECQuantity, d[index, :type], space)
  @info new_type
  SymbolicUtils.Sym{new_type}(var)
end

function to_symbolics(d::SummationDecapode, symvar_lookup::Dict{Symbol, SymbolicUtils.BasicSymbolic}, op_index::Int, ::Val{:Op1})
  input_sym = symvar_lookup[d[d[op_index, :src], :name]]
  output_sym = symvar_lookup[d[d[op_index, :tgt], :name]]
  # op_sym = SymbolicUtils.Sym{(SymbolicUtils.FnType){Tuple{Number}, Number}}(d[op_index, :op1])

  op_sym = getfield(@__MODULE__, d[op_index, :op1])

  @info typeof(op_sym)

  rhs = SymbolicUtils.Term{Number}(op_sym, [input_sym])
  SymbolicUtils.Term{Number}(DECA_EQUALITY_SYMBOL, [output_sym, rhs])
end

function to_symbolics(d::SummationDecapode, symvar_lookup::Dict{Symbol, SymbolicUtils.BasicSymbolic}, op_index::Int, ::Val{:Op2})
  input1_sym = symvar_lookup[d[d[op_index, :proj1], :name]]
  input2_sym = symvar_lookup[d[d[op_index, :proj2], :name]]
  output_sym = symvar_lookup[d[d[op_index, :res], :name]]
  # op_sym = SymbolicUtils.Sym{(SymbolicUtils.FnType){Tuple{Number, Number}, Number}}(d[op_index, :op2])

  op_sym = getfield(@__MODULE__, d[op_index, :op2])

  rhs = SymbolicUtils.Term{Number}(op_sym, [input1_sym, input2_sym])
  SymbolicUtils.Term{Number}(DECA_EQUALITY_SYMBOL, [output_sym, rhs])
end

#XXX: Always converting + -> .+ here since summation doesn't store the style of addition
function to_symbolics(d::SummationDecapode, symvar_lookup::Dict{Symbol, SymbolicUtils.BasicSymbolic}, op_index::Int, ::Val{:Σ})
  syms_array = [symvar_lookup[var] for var in d[d[incident(d, op_index, :summation), :summand], :name]]
  output_sym = symvar_lookup[d[d[op_index, :sum], :name]]

  rhs = SymbolicUtils.Term{Number}(+, syms_array)
  SymbolicUtils.Term{Number}(DECA_EQUALITY_SYMBOL, [output_sym, rhs])
end

function symbolic_rewriting(old_d::SummationDecapode)
  d = deepcopy(old_d)

  infer_types!(d)
  # resolve_overloads!(d)

  symvar_lookup = symbolics_lookup(d)
  merge_equations(d, symvar_lookup, extract_symexprs(d, symvar_lookup))
end

function extract_symexprs(d::SummationDecapode, symvar_lookup::Dict{Symbol, SymbolicUtils.BasicSymbolic})
  topo_list = topological_sort_edges(d)
  sym_list = []
  for node in topo_list
    retrieve_name(d, node) != DerivOp || continue
    push!(sym_list, to_symbolics(d, symvar_lookup, node))
  end
  sym_list
end

function apply_rewrites(symexprs, rewriter)

  rewritten_list = []
  for sym in symexprs
    res_sym = rewriter(sym)
    rewritten_sym = isnothing(res_sym) ? sym : res_sym
    push!(rewritten_list, rewritten_sym)
  end

  rewritten_list
end

function merge_equations(d::SummationDecapode, symvar_lookup::Dict{Symbol, SymbolicUtils.BasicSymbolic}, rewritten_syms)

  eqn_lookup = Dict()

  final_list = []

  for node in start_nodes(d)
    sym = symvar_lookup[d[node, :name]]
    push!(eqn_lookup, (sym => sym))
  end

  final_nodes = infer_terminal_names(d)

  for expr in rewritten_syms

    merged_eqn = SymbolicUtils.substitute(expr, eqn_lookup)
    lhs = merged_eqn.arguments[1]
    rhs = merged_eqn.arguments[2]

    push!(eqn_lookup, (lhs => rhs))

    if lhs.name in final_nodes
      push!(final_list, merged_eqn)
    end
  end

  final_list
end

function to_acset(og_d, sym_exprs)
  final_exprs = SymbolicUtils.Code.toexpr.(sym_exprs)

  recursive_descent = @λ begin
    e::Expr => begin
      if e.head == :call
        @show nameof(e.args[1])
        e.args[1] = nameof(e.args[1])
        map(recursive_descent, e.args[2:end])
      end
    end
    sym => nothing
  end
  map(recursive_descent, final_exprs)

  deca_block = quote end

  states = infer_states(og_d)
  terminals = infer_terminals(og_d)

  deca_type_gen = idx -> :($(og_d[idx, :name])::$(og_d[idx, :type]))

  append!(deca_block.args, map(deca_type_gen, vcat(states, terminals)))

  for op1 in parts(og_d, :Op1)
    if og_d[op1, :op1] == DerivOp
      push!(deca_block.args, :($(og_d[og_d[op1, :tgt], :name]) == $DerivOp($(og_d[og_d[op1, :src], :name]))))
    end
  end

  append!(deca_block.args, final_exprs)

  d = SummationDecapode(parse_decapode(deca_block))

  infer_types!(d)
  # resolve_overloads!(d)

  d
end
