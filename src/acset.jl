using Catlab
using Catlab.DenseACSets
using DataStructures
using ACSets.InterTypes

@intertypes "decapodeacset.it" module decapodeacset end

using .decapodeacset
# TODO: Move this export to main file
export Operator, same_type_rules_op, arthimetic_operators, infer_resolve!, type_check

# Transferring pointers
# --------------------

"""    function transfer_parents!(d::SummationDecapode, x, y)

Transfer the parents of x to y. Also transfer TVar status from x to y.
"""
function transfer_parents!(d::SummationDecapode, x, y)
  #set_subpart!(d, incident(d, x, [:tgt], copy=true), :tgt, y)
  #set_subpart!(d, incident(d, x, [:res], copy=true), :res, y)
  #set_subpart!(d, incident(d, x, [:sum], copy=true), :sum, y)
  #set_subpart!(d, incident(d, x, [:incl], copy=true), :incl, y)
  # XXX: This avoids allocations.
  for i in parts(d, :Op1)
    if d.subparts.tgt[i] == x
      d.subparts.tgt[i] = y
    end
  end
  for i in parts(d, :Op2)
    if d.subparts.res[i] == x
      d.subparts.res[i] = y
    end
  end
  for i in parts(d, :Σ)
    if d.subparts.sum[i] == x
      d.subparts.sum[i] = y
    end
  end
  for i in parts(d, :TVar)
    if d.subparts.incl[i] == x
      d.subparts.incl[i] = y
    end
  end
  d
end

"""    function transfer_children!(d::SummationDecapode, x, y)

Transfer the children of x to y.
"""
function transfer_children!(d::SummationDecapode, x, y)
  #set_subpart!(d, incident(d, x, [:src], copy=true), :src, y)
  #set_subpart!(d, incident(d, x, [:proj1], copy=true), :proj1, y)
  #set_subpart!(d, incident(d, x, [:proj2], copy=true), :proj2, y)
  #set_subpart!(d, incident(d, x, [:summand], copy=true), :summand, y)
  # XXX: This avoids allocations.
  for i in parts(d, :Op1)
    if d.subparts.src[i] == x
      d.subparts.src[i] = y
    end
  end
  for i in parts(d, :Op2)
    if d.subparts.proj1[i] == x
      d.subparts.proj1[i] = y
    end
  end
  for i in parts(d, :Op2)
    if d.subparts.proj2[i] == x
      d.subparts.proj2[i] = y
    end
  end
  for i in parts(d, :Summand)
    if d.subparts.summand[i] == x
      d.subparts.summand[i] = y
    end
  end
  d
end


"""    function fill_names!(d::AbstractNamedDecapode; lead_symbol::Symbol = Symbol("•"))

Provide a variable name to all the variables that don't have names.
"""
function fill_names!(d::AbstractNamedDecapode; lead_symbol::Symbol = Symbol("•"))
  bulletcount = 1
  for i in parts(d, :Var)
    if !isassigned(d[:,:name],i) || isnothing(d[i, :name])
      d[i,:name] = Symbol("$lead_symbol$bulletcount")
      bulletcount += 1
    end
  end
  for e in incident(d, :∂ₜ, :op1)
    s = d[e,:src]
    t = d[e, :tgt]
    String(d[t,:name])[1] != '•' && continue
    d[t, :name] = append_dot(d[s,:name])
  end
  d
end

"""    find_dep_and_order(d::AbstractNamedDecapode)

Find the order of each tangent variable in the Decapode, and the index of the variable that it is dependent on. Returns a tuple of (dep, order), both of which respecting the order in which incident(d, :∂ₜ, :op1) returns Vars.
"""
function find_dep_and_order(d::AbstractNamedDecapode)
  dep = d[incident(d, :∂ₜ, :op1), :src]
  order = ones(Int, nparts(d, :TVar))
  found = true
  while found
    found = false
    for i in parts(d, :TVar)
      deps = incident(d, :∂ₜ, :op1) ∩ incident(d, dep[i], :tgt)
      if !isempty(deps)
        dep[i] = d[first(deps), :src]
        order[i] += 1
        found = true
      end
    end
  end
  (dep, order)
end

"""    dot_rename!(d::AbstractNamedDecapode)

Rename tangent variables by their depending variable appended with a dot.
e.g. If D == ∂ₜ(C), then rename D to Ċ.

If a tangent variable updates multiple vars, choose one arbitrarily.
e.g. If D == ∂ₜ(C) and D == ∂ₜ(B), then rename D to either Ċ or B ̇.
"""
function dot_rename!(d::AbstractNamedDecapode)
  dep, order = find_dep_and_order(d)
  for (i,e) in enumerate(incident(d, :∂ₜ, :op1))
    t = d[e, :tgt]
    name = d[dep[i],:name]
    for _ in 1:order[i]
      name = append_dot(name)
    end
    d[t, :name] = name
  end
  d
end

function make_sum_mult_unique!(d::AbstractNamedDecapode)
  snum = 1
  mnum = 1
  for (i, name) in enumerate(d[:name])
    if(name == :sum)
      d[i, :name] = Symbol("sum_$(snum)")
      snum += 1
    elseif(name == :mult)
      d[i, :name] = Symbol("mult_$(mnum)")
      mnum += 1
    end
  end
end

# A collection of DecaType getters
# TODO: This should be replaced by using a type hierarchy
const ALL_TYPES = [:Form0, :Form1, :Form2, :DualForm0, :DualForm1, :DualForm2,
                   :PVF, :DVF,
                   :Literal, :Parameter, :Constant, :infer]

const FORM_TYPES = [:Form0, :Form1, :Form2, :DualForm0, :DualForm1, :DualForm2]
const PRIMALFORM_TYPES = [:Form0, :Form1, :Form2]
const DUALFORM_TYPES = [:DualForm0, :DualForm1, :DualForm2]

const VECTORFIELD_TYPES = [:PVF, :DVF]

const NON_EC_TYPES = [:Constant, :Parameter, :Literal, :infer]
const USER_TYPES = [:Constant, :Parameter]
const NUMBER_TYPES = [:Literal]
const INFER_TYPES = [:infer]

# Types that can not ever be inferred
const NONINFERABLE_TYPES = [:Constant, :Parameter, :Literal]

function get_unsupportedtypes(types)
  setdiff(types, ALL_TYPES)
end

# Note: This hard-bakes in Form0 through Form2, and higher Forms are not
# allowed.
function recognize_types(d::AbstractNamedDecapode)
  types = d[:type]
  unrecognized_types = get_unsupportedtypes(types)
  isempty(unrecognized_types) ||
  error("Types $unrecognized_types are not recognized. CHECK: $types")
end
export recognize_types

"""    is_expanded(d::AbstractNamedDecapode)

Check that no unary operator is a composition of unary operators.
"""
is_expanded(d::AbstractNamedDecapode) = !any(x -> x isa AbstractVector, d[:op1])

"""    function expand_operators(d::AbstractNamedDecapode)

If any unary operator is a composition, expand it out using intermediate variables.
"""
function expand_operators(d::AbstractNamedDecapode)
  #e = SummationDecapode{Symbol, Symbol, Symbol}()
  e = SummationDecapode{Any, Any, Symbol}()
  copy_parts!(e, d, (:Var, :TVar, :Op2))
  expand_operators!(e, d)
  return e
end

function expand_operators!(e::AbstractNamedDecapode, d::AbstractNamedDecapode)
  newvar = 0
  for op in parts(d, :Op1)
    if !isa(d[op,:op1], AbstractArray)
      add_part!(e, :Op1, op1=d[op,:op1], src=d[op, :src], tgt=d[op,:tgt])
    elseif length(d[op, :op1]) == 1
      add_part!(e, :Op1, op1=only(d[op,:op1]), src=d[op, :src], tgt=d[op,:tgt])
    else
      for (i, step) in enumerate(d[op, :op1])
        if i == 1
          newvar = add_part!(e, :Var, type=:infer, name=Symbol("•_$(op)_$(i)"))
          add_part!(e, :Op1, op1=step, src=d[op, :src], tgt=newvar)
        elseif i == length(d[op, :op1])
          add_part!(e, :Op1, op1=step, src=newvar, tgt=d[op,:tgt])
        else
          newvar′ = add_part!(e, :Var, type=:infer, name=Symbol("•_$(op)_$(i)"))
          add_part!(e, :Op1, op1=step, src=newvar, tgt=newvar′)
          newvar = newvar′
        end
      end
    end
  end
  return newvar
end

"""    function infer_states(d::SummationDecapode)

Find variables which have a time derivative or are not the source of a computation.
See also: [`infer_terminals`](@ref).
"""
function infer_states(d::SummationDecapode)
  parentless = filter(parts(d, :Var)) do v
    length(incident(d, v, :tgt)) == 0 &&
    length(incident(d, v, :res)) == 0 &&
    length(incident(d, v, :sum)) == 0 &&
    d[v, :type] != :Literal
  end
  parents_of_tvars =
    union(d[incident(d,:∂ₜ, :op1), :src],
          d[incident(d,:dt, :op1), :src])
  union(parentless, parents_of_tvars)
end

"""    function infer_state_names(d)

Find names of variables which have a time derivative or are not the source of a computation.
See also: [`infer_states`](@ref).
"""
infer_state_names(d) = d[infer_states(d), :name]

"""    function infer_terminals(d::SummationDecapode)
Find variables which have no children.
See also: [`infer_states`](@ref).
"""
function infer_terminals(d::SummationDecapode)
  filter(parts(d, :Var)) do v
    length(incident(d, v, :src)) == 0 &&
    length(incident(d, v, :proj1)) == 0 &&
    length(incident(d, v, :proj2)) == 0 &&
    length(incident(d, v, :summand)) == 0
  end
end

"""    function infer_terminal_names(d)

Find names of variables which have no children.
See also: [`infer_terminals`](@ref).
"""
infer_terminal_names(d) = d[infer_terminals(d), :name]

"""    function expand_operators(d::SummationDecapode)

Find operations that are compositions, and expand them with intermediate variables.
"""
function expand_operators(d::SummationDecapode)
  #e = SummationDecapode{Symbol, Symbol, Symbol}()
  e = SummationDecapode{Any, Any, Symbol}()
  copy_parts!(e, d, (:Var, :TVar, :Op2, :Σ, :Summand))
  expand_operators!(e, d)
  return e
end

"""    function contract_operators(d::SummationDecapode; white_list::Set{Symbol} = Set{Symbol}(), black_list::Set{Symbol} = Set{Symbol}())

Find chains of Op1s in the given Decapode, and replace them with
a single Op1 with a vector of function names. After this process,
all Vars that are not a part of any computation are removed. If a
white list is provided, only chain those operators. If a black list
is provided, do not chain those operators.
"""
function contract_operators(d::SummationDecapode;
  white_list::Set{Symbol} = Set{Symbol}(),
  black_list::Set{Symbol} = Set{Symbol}())
  e = expand_operators(d)
  contract_operators!(e, white_list=white_list, black_list=black_list)
  #return e
end

function contract_operators!(d::SummationDecapode;
    white_list::Set{Symbol} = Set{Symbol}(),
    black_list::Set{Symbol} = Set{Symbol}())
  chains = find_chains(d, white_list=white_list, black_list=black_list)
  filter!(x -> length(x) != 1, chains)
  for chain in chains
    add_part!(d, :Op1, src=d[:src][first(chain)], tgt=d[:tgt][last(chain)], op1=Vector{Symbol}(d[:op1][chain]))
  end
  rem_parts!(d, :Op1, sort!(vcat(chains...)))
  remove_neighborless_vars!(d)
end

"""    function remove_neighborless_vars!(d::SummationDecapode)

Remove all Vars from the given Decapode that are not part of any computation.
"""
function remove_neighborless_vars!(d::SummationDecapode)
  neighborless_vars = setdiff(parts(d,:Var),
                              union(d[:src],
                                    d[:tgt],
                                    d[:proj1],
                                    d[:proj2],
                                    d[:res],
                                    d[:sum],
                                    d[:summand],
                                    d[:incl]))
  #union(map(x -> t5_orig[x], [:src, :tgt])...) alternate syntax
  #rem_parts!(d, :Var, neighborless_vars)
  rem_parts!(d, :Var, sort!(neighborless_vars))
  d
end

"""    function find_chains(d::SummationDecapode; white_list::Set{Symbol} = Set{Symbol}(), black_list::Set{Symbol} = Set{Symbol}())

Find chains of Op1s in the given Decapode. A chain ends when the
target of the last Op1 is part of an Op2 or sum, or is a target
of multiple Op1s. If a white list is provided, only chain those
operators. If a black list is provided, do not chain those operators.
"""
function find_chains(d::SummationDecapode;
    white_list::Set{Symbol} = Set{Symbol}(),
    black_list::Set{Symbol} = Set{Symbol}())
  chains = []
  visited = falses(nparts(d, :Op1))
  # TODO: Re-write this without two reduce-vcats.
  rvrv(x) = reduce(vcat, reduce(vcat, x))
  chain_starts = unique(rvrv(
    [incident(d, Vector{Int64}(filter(i -> !isnothing(i), infer_states(d))), :src),
     incident(d, d[:res], :src),
     incident(d, d[:sum], :src),
     incident(d, d[collect(Iterators.flatten(incident(d, collect(black_list), :op1))), :tgt], :src)]))

  passes_white_list(x) = isempty(white_list) ? true : x ∈ white_list
  passes_black_list(x) = x ∉ black_list

  filter!(x -> passes_white_list(d[x, :op1]), chain_starts)
  filter!(x -> passes_black_list(d[x, :op1]), chain_starts)

  s = Stack{Int64}()
  foreach(x -> push!(s, x), chain_starts)
  while !isempty(s)
    # Start a new chain.
    op_to_visit = pop!(s)
    curr_chain = []
    while true
      visited[op_to_visit] = true
      append!(curr_chain, op_to_visit)

      tgt = d[op_to_visit, :tgt]
      next_op1s = incident(d, tgt, :src)
      next_op2s = vcat(incident(d, tgt, :proj1), incident(d, tgt, :proj2))

      if (length(next_op1s) != 1 ||
          length(next_op2s) != 0 ||
          is_tgt_of_many_ops(d, tgt) ||
          !isempty(incident(d, tgt, :sum)) ||
          !isempty(incident(d, tgt, :summand)) ||
          !passes_white_list(d[only(next_op1s), :op1]) ||
          !passes_black_list(d[only(next_op1s), :op1]))
        # Terminate chain.
        append!(chains, [curr_chain])
        for op1 in next_op1s
          if !visited[op1] && passes_white_list(d[op1, :op1]) && passes_black_list(d[op1, :op1])
            push!(s, op1)
          end
        end
        break
      end
      # Try to continue chain.
      op_to_visit = only(next_op1s)
    end
  end
  return chains
end

function add_constant!(d::AbstractNamedDecapode, k::Symbol)
    return add_part!(d, :Var, type=:Constant, name=k)
end

function add_parameter(d::AbstractNamedDecapode, k::Symbol)
    return add_part!(d, :Var, type=:Parameter, name=k)
end


"""
    safe_modifytype(org_type::Symbol, new_type::Symbol)

This function accepts an original type and a new type and determines if the original type
  can be safely overwritten by the new type.
"""
function safe_modifytype(org_type::Symbol, new_type::Symbol)
  modify = (org_type in INFER_TYPES && !(new_type in NONINFERABLE_TYPES))
  return (modify, modify ? new_type : org_type)
end

"""
    safe_modifytype!(d::SummationDecapode, var_idx::Int, new_type::Symbol)

This function calls `safe_modifytype` to safely modify a Decapode's variable type.
"""
function safe_modifytype!(d::SummationDecapode, var_idx::Int, new_type::Symbol)
  modify, d[var_idx, :type] = safe_modifytype(d[var_idx, :type], new_type)
  return modify
end

"""
    filterfor_ec_types(types::AbstractVector{Symbol})

Return any form or vector-field type symbols.
"""
function filterfor_ec_types(types::AbstractVector{Symbol})
  conditions = x -> !(x in NON_EC_TYPES)
  filter(conditions, types)
end

struct Operator{T}
  res_type::T
  src_types::AbstractVector{T}
  op_name::Symbol
  aliases::AbstractVector{Symbol}

  function Operator{T}(res_type::T, src_types::AbstractVector{T}, op_name, aliases = Symbol[]) where T
    new(res_type, src_types, op_name, aliases)
  end

  function Operator{T}(res_type::T, src_type::T, op_name, aliases = Symbol[]) where T
    new(res_type, T[src_type], op_name, aliases)
  end

  function Operator(res_type::Symbol, src_type::Union{Symbol, AbstractVector{Symbol}}, op_name, aliases = Symbol[])
    Operator{Symbol}(res_type, src_type, op_name, aliases)
  end
end

function same_type_rules_op(op_name::Symbol, types::AbstractVector{Symbol}, arity::Int, g_aliases::AbstractVector{Symbol} = Symbol[], sp_aliases::AbstractVector = Symbol[])
  @assert isempty(sp_aliases) || length(types) == length(sp_aliases)
  map(1:length(types)) do i
    aliases = isempty(sp_aliases) ? g_aliases : vcat(g_aliases, sp_aliases[i])
    Operator{Symbol}(types[i], repeat([types[i]], arity), op_name, aliases)
  end
end

function arthimetic_operators(op_name::Symbol, broadcasted::Bool, arity::Int = 2)
  @match (broadcasted, arity) begin
    (true, 2) => bin_broad_arith_ops(op_name)
    _ => error("This type of arthimetic operator is not yet supported or may not be valid.")
  end
end

function bin_broad_arith_ops(op_name)
  all_ops = map(t -> Operator{Symbol}(t, [t, t], op_name), FORM_TYPES)
  for type in vcat(USER_TYPES, NUMBER_TYPES)
    append!(all_ops, map(t -> Operator{Symbol}(t, [t, type], op_name), FORM_TYPES))
    append!(all_ops, map(t -> Operator{Symbol}(t, [type, t], op_name), FORM_TYPES))
  end

  all_ops
end

function infer_sum_types!(d::SummationDecapode, Σ_idx::Int)
  # Note that we are not doing any type checking here for users!
  # i.e. We are not checking the underlying types of Constant or Parameter
  applied = false

  summands = d[incident(d, Σ_idx, :summation), :summand]
  sum = d[Σ_idx, :sum]
  idxs = [summands; sum]
  types = d[idxs, :type]
  all(t != :infer for t in types) && return applied # We need not infer

  ec_types = unique(filterfor_ec_types(types))

  ec_type = @match length(ec_types) begin
    0 => return applied # We can not infer
    1 => only(ec_types)
    _ => error("Type mismatch in summation $Σ_idx, all the following forms appear: $ec_types")
  end

  for idx in idxs
    applied |= safe_modifytype!(d, idx, ec_type)
  end

  return applied
end

function check_operator(d::SummationDecapode, op_id, rule, edge_val; check_name::Bool = false, check_aliases::Bool = false, ignore_infers::Bool = false, ignore_usertypes::Bool = false)
  inputs = edge_inputs(d, op_id, edge_val)
  output = edge_output(d, op_id, edge_val)

  max_score = length(inputs) + length(output)

  rule_types = vcat(rule.src_types, rule.res_type)
  deca_types = vcat(d[inputs, :type], d[output, :type])

  score = sum(map(zip(rule_types, deca_types)) do (rule_t, deca_t)
    if ignore_infers && deca_t in INFER_TYPES; return 1; end
    if ignore_usertypes && deca_t in USER_TYPES; return 1; end
    return rule_t == deca_t
  end)

  dop_name = edge_function(d, op_id, edge_val)

  check_op = check_name ? dop_name == rule.op_name : false
  check_op = check_op || (check_aliases ? dop_name in rule.aliases : check_op)

  (check_op, max_score - score)
end

function apply_inference_rule!(d::SummationDecapode, op_id, rule, edge_val)

  name_present, type_diff = check_operator(d, op_id, rule, edge_val; check_name = true, check_aliases = true)

  if(name_present && type_diff == 1)
    mod_srcs = false
    for (in, type) in zip(edge_inputs(d, op_id, edge_val), rule.src_types)
      mod_srcs |= safe_modifytype!(d, in, type)
    end
    mod_tgt = safe_modifytype!(d, edge_output(d, op_id, edge_val), rule.res_type)
    return mod_srcs || mod_tgt
  end

  return false
end

function apply_overloading_rule!(d::SummationDecapode, op_id, rule, edge_val)

  name_present, type_diff = check_operator(d, op_id, rule, edge_val; check_aliases = true)

  if name_present && type_diff == 0
    set_edge_label(d, op_id, rule.op_name, edge_val)
    return true
  end

  return false
end

function apply_type_checking_rule(d::SummationDecapode, op_id, rule, edge_val)
  name_present, type_diff = check_operator(d, op_id, rule, edge_val; check_name = true, ignore_infers = true, ignore_usertypes = true)
  return name_present, type_diff == 0
end

# TODO: Although the big-O complexity is the same, it might be more efficent on
# average to iterate over edges then rules, instead of rules then edges. This
# might result in more un-maintainable code. If you implement this, you might
# also want to make the rules keys in a Dict.
# It also might be more efficient on average to instead iterate over variables.
"""    function infer_types!(d::SummationDecapode, op1_rules::Vector{NamedTuple{(:src_type, :tgt_type, :replacement_type, :op), NTuple{4, Symbol}}})

Infer types of Vars given rules wherein one type is known and the other not.
"""
function infer_types!(d::SummationDecapode, type_rules::AbstractVector{Operator{Symbol}})

  # This is an optimization so we do not "visit" a row which has no infer types.
  # It could be deleted if found to be not worth maintainability tradeoff.
  types_known_op1 = ones(Bool, nparts(d, :Op1))
  types_known_op1[incident(d, :infer, [:src, :type])] .= false
  types_known_op1[incident(d, :infer, [:tgt, :type])] .= false

  types_known_op2 = ones(Bool, nparts(d, :Op2))
  types_known_op2[incident(d, :infer, [:proj1, :type])] .= false
  types_known_op2[incident(d, :infer, [:proj2, :type])] .= false
  types_known_op2[incident(d, :infer, [:res, :type])] .= false

  types_known = Dict{Symbol, Vector{Bool}}(:Op1 => types_known_op1, :Op2 => types_known_op2)

  while true
    applied = false


    for table in [:Op1, :Op2]
      for op_idx in parts(d, table)
        types_known[table][op_idx] && continue

        for rule in type_rules
          this_applied = apply_inference_rule!(d, op_idx, rule, Val(table))

          types_known[table][op_idx] = this_applied
          applied |= this_applied
        end

      end
    end

    for Σ_idx in parts(d, :Σ)
      applied |= infer_sum_types!(d, Σ_idx)
    end

    applied || break # Break if no rules were applied.
  end

  d
end

"""    function resolve_overloads!(d::SummationDecapode, op1_rules::Vector{NamedTuple{(:src_type, :tgt_type, :resolved_name, :op), NTuple{4, Symbol}}})

Resolve function overloads based on types of src and tgt.
"""
function resolve_overloads!(d::SummationDecapode, resolve_rules::AbstractVector{Operator{Symbol}})
  for rule in resolve_rules
    for table in [:Op1, :Op2]
      for op_idx in parts(d, table)
        apply_overloading_rule!(d, op_idx, rule, Val(table))
      end
    end
  end

  d
end

function type_check(d::SummationDecapode, type_rules::AbstractVector{Operator{Symbol}})
  for table in [:Op1, :Op2]
    for op_idx in parts(d, table)

      check_passed = true
      for rule in type_rules
        rule_applies, rule_checked = apply_type_checking_rule(d, op_idx, rule, Val(table))

        rule_applies || continue
        check_passed = false

        rule_checked || continue
        check_passed = true
        break
      end

      # TODO: Change this to error
      if !check_passed
        error("$(edge_function(d, op_idx, Val(table))) at $op_idx in table $table is not typed properly")
        # println("$(rule)")
        return false
      end

    end
  end

  true
end

function infer_resolve!(d::SummationDecapode, operators::AbstractVector{Operator{Symbol}})
  infer_types!(d, operators)
  resolve_overloads!(d, operators)
  type_check(d, operators)

  d
end

function replace_names!(d::SummationDecapode, op1_repls::Vector{Pair{Symbol, Any}}, op2_repls::Vector{Pair{Symbol, Symbol}})
  for (orig,repl) in op1_repls
    for i in collect(incident(d, orig, :op1))
      d[i, :op1] = repl
    end
  end
  for (orig,repl) in op2_repls
    for i in collect(incident(d, orig, :op2))
      d[i, :op2] = repl
    end
  end
  d
end

# Return a dict of Literals and the indices of Vars equal to that Literal.
# XXX: Does not copy the result of incident.
# XXX: A generalization for given subpart must be type stable.
function group_lits_by_name(d::SummationDecapode{Any,Any,Symbol})
  dict = Dict{Symbol,Vector{Int}}()
  for v in incident(d, :Literal, :type)
    lit = d[v,:name]
    (lit in keys(dict)) ? (continue) : (setindex!(dict, incident(d,lit,:name), lit))
  end
  dict
end

"""    function unique_lits!(d::SummationDecapode)

Remove repeated Literals from a Decapode.
"""
function unique_lits!(d::SummationDecapode)
  lit_idxs = group_lits_by_name(d)
  filter!(((l,i),) -> 1 < length(i), lit_idxs)
  isempty(lit_idxs) && return d
  to_remove = map(keys(lit_idxs), values(lit_idxs)) do lit, idxs
    foreach(x -> transfer_children!(d, x, idxs[1]), idxs[2:end])
    idxs[2:end]
  end
  rem_parts!(d, :Var, sort!(reduce(vcat, to_remove)))
  d
end
