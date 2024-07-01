using Catlab
using Catlab.DenseACSets
using DataStructures
using ACSets.InterTypes

@intertypes "decapodeacset.it" module decapodeacset end

using .decapodeacset

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
  :Literal, :Parameter, :Constant, :infer]

const FORM_TYPES = [:Form0, :Form1, :Form2, :DualForm0, :DualForm1, :DualForm2]
const PRIMALFORM_TYPES = [:Form0, :Form1, :Form2]
const DUALFORM_TYPES = [:DualForm0, :DualForm1, :DualForm2]

const NONFORM_TYPES = [:Constant, :Parameter, :Literal, :infer]
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
    filterfor_forms(types::AbstractVector{Symbol})

Return any form type symbols.
"""
function filterfor_forms(types::AbstractVector{Symbol})
  conditions = x -> !(x in NONFORM_TYPES)
  filter(conditions, types)
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

  forms = unique(filterfor_forms(types))

  form = @match length(forms) begin
    0 => return applied # We can not infer
    1 => only(forms)
    _ => error("Type mismatch in summation $Σ_idx, all the following forms appear: $forms")
  end

  for idx in idxs
    applied |= safe_modifytype!(d, idx, form)
  end

  return applied
end

function apply_inference_rule_op1!(d::SummationDecapode, op1_id, rule)
  type_src = d[d[op1_id, :src], :type]
  type_tgt = d[d[op1_id, :tgt], :type]

  score_src = (rule.src_type == type_src)
  score_tgt = (rule.tgt_type == type_tgt)
  check_op = (d[op1_id, :op1] in rule.op_names)

  if(check_op && (score_src + score_tgt == 1))
    mod_src = safe_modifytype!(d, d[op1_id, :src], rule.src_type)
    mod_tgt = safe_modifytype!(d, d[op1_id, :tgt], rule.tgt_type)
    return mod_src || mod_tgt
  end

  return false
end

function apply_inference_rule_op2!(d::SummationDecapode, op2_id, rule)
  type_proj1 = d[d[op2_id, :proj1], :type]
  type_proj2 = d[d[op2_id, :proj2], :type]
  type_res = d[d[op2_id, :res], :type]

  score_proj1 = (rule.proj1_type == type_proj1)
  score_proj2 = (rule.proj2_type == type_proj2)
  score_res = (rule.res_type == type_res)
  check_op = (d[op2_id, :op2] in rule.op_names)

  if(check_op && (score_proj1 + score_proj2 + score_res == 2))
    mod_proj1 = safe_modifytype!(d, d[op2_id, :proj1], rule.proj1_type)
    mod_proj2 = safe_modifytype!(d, d[op2_id, :proj2], rule.proj2_type)
    mod_res =   safe_modifytype!(d, d[op2_id, :res], rule.res_type)
    return mod_proj1 || mod_proj2 || mod_res
  end

  return false
end


# TODO: Although the big-O complexity is the same, it might be more efficent on
# average to iterate over edges then rules, instead of rules then edges. This
# might result in more un-maintainable code. If you implement this, you might
# also want to make the rules keys in a Dict.
# It also might be more efficient on average to instead iterate over variables.
"""    function infer_types!(d::SummationDecapode, op1_rules::Vector{NamedTuple{(:src_type, :tgt_type, :replacement_type, :op), NTuple{4, Symbol}}})

Infer types of Vars given rules wherein one type is known and the other not.
"""
function infer_types!(d::SummationDecapode, op1_rules::Vector{NamedTuple{(:src_type, :tgt_type, :op_names), Tuple{Symbol, Symbol, Vector{Symbol}}}}, op2_rules::Vector{NamedTuple{(:proj1_type, :proj2_type, :res_type, :op_names), Tuple{Symbol, Symbol, Symbol, Vector{Symbol}}}})

  # This is an optimization so we do not "visit" a row which has no infer types.
  # It could be deleted if found to be not worth maintainability tradeoff.
  types_known_op1 = ones(Bool, nparts(d, :Op1))
  types_known_op1[incident(d, :infer, [:src, :type])] .= false
  types_known_op1[incident(d, :infer, [:tgt, :type])] .= false

  types_known_op2 = ones(Bool, nparts(d, :Op2))
  types_known_op2[incident(d, :infer, [:proj1, :type])] .= false
  types_known_op2[incident(d, :infer, [:proj2, :type])] .= false
  types_known_op2[incident(d, :infer, [:res, :type])] .= false

  while true
    applied = false

    for rule in op1_rules
      for op1_idx in parts(d, :Op1)
        types_known_op1[op1_idx] && continue

        this_applied = apply_inference_rule_op1!(d, op1_idx, rule)

        types_known_op1[op1_idx] = this_applied
        applied |= this_applied
      end
    end

    for rule in op2_rules
      for op2_idx in parts(d, :Op2)
        types_known_op2[op2_idx] && continue

        this_applied = apply_inference_rule_op2!(d, op2_idx, rule)

        types_known_op2[op2_idx] = this_applied
        applied |= this_applied
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
function resolve_overloads!(d::SummationDecapode, op1_rules::Vector{NamedTuple{(:src_type, :tgt_type, :resolved_name, :op), NTuple{4, Symbol}}}, op2_rules::Vector{NamedTuple{(:proj1_type, :proj2_type, :res_type, :resolved_name, :op), NTuple{5, Symbol}}})
  for op1_idx in parts(d, :Op1)
    src = d[:src][op1_idx]; tgt = d[:tgt][op1_idx]; op1 = d[:op1][op1_idx]
    src_type = d[:type][src]; tgt_type = d[:type][tgt]
    for rule in op1_rules
      if op1 == rule[:op] && src_type == rule[:src_type] && tgt_type == rule[:tgt_type]
        d[op1_idx, :op1] = rule[:resolved_name]
        break
      end
    end
  end

  for op2_idx in parts(d, :Op2)
    proj1 = d[:proj1][op2_idx]; proj2 = d[:proj2][op2_idx]; res = d[:res][op2_idx]; op2 = d[:op2][op2_idx]
    proj1_type = d[:type][proj1]; proj2_type = d[:type][proj2]; res_type = d[:type][res]
    for rule in op2_rules
      if op2 == rule[:op] && proj1_type == rule[:proj1_type] && proj2_type == rule[:proj2_type] && res_type == rule[:res_type]
        d[op2_idx, :op2] = rule[:resolved_name]
        break
      end
    end
  end

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
