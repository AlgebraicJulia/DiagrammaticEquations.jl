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

# Opening up Op1s
# --------------

# Validate whether LHS represents a valid op1.
function validate_op1_match(d::SummationDecapode, LHS::SummationDecapode)
  if nparts(LHS, :Op1) != 1
    error("Only single operator replacement is supported for now, but found Op1s: $(LHS[:op1])")
  end
end

# Validate whether RHS represents a valid replacement for an op1.
function validate_op1_replacement(d::SummationDecapode, LHS::Symbol, RHS::SummationDecapode)
  if length(infer_states(RHS)) != 1 || length(infer_terminals(RHS)) != 1
    error("The replacement for $(LHS) must have a single input and a single output, but found inputs: $(RHS[infer_states(RHS), :name]) and outputs $(RHS[infer_terminals(RHS), :name])")
  end
end

"""    function replace_op1!(d::SummationDecapode, LHS::Symbol, RHS::SummationDecapode)

Given a Decapode, d, replace at most one instance of the left-hand-side unary operator with those of the right-hand-side.

Return the index of the replaced operator, 0 if no match was found.

See also: [`replace_all_op1s!`](@ref)
"""
function replace_op1!(d::SummationDecapode, LHS::Symbol, RHS::SummationDecapode)
  validate_op1_replacement(d, LHS, RHS)
  isempty(incident(d, LHS, :op1)) && return 0
  # Identify the "matched" operation.
  LHS_op1 = first(incident(d, LHS, :op1))
  LHS_input = d[LHS_op1, :src]
  LHS_output = d[LHS_op1, :tgt]
  # Add in the "replace" operation(s).
  added_vars = copy_parts!(d, RHS).Var
  RHS_input = only(intersect(infer_states(d), added_vars))
  RHS_output = only(intersect(infer_terminals(d), added_vars))
  # Transfer LHS_input's pointers to RHS_input.
  transfer_parents!(d, LHS_input, RHS_input)
  transfer_children!(d, LHS_input, RHS_input)
  d[RHS_input, :name] = d[LHS_input, :name]
  d[RHS_input, :type] = d[LHS_input, :type]
  # Transfer LHS_output's pointers to RHS_output.
  transfer_parents!(d, LHS_output, RHS_output)
  transfer_children!(d, LHS_output, RHS_output)
  d[RHS_output, :name] = d[LHS_output, :name]
  d[RHS_output, :type] = d[LHS_output, :type]
  # Remove the replaced match and variables.
  rem_parts!(d, :Var, sort!([LHS_input, LHS_output]))
  rem_part!(d, :Op1, LHS_op1)
  LHS_op1
end

"""    function replace_op1!(d::SummationDecapode, LHS::SummationDecapode, RHS::SummationDecapode)

Given a Decapode, d, replace at most one instance of the left-hand-side unary operator with those of the right-hand-side.

Return the index of the replaced unary operator, 0 if no match was found.
See also: [`replace_op2!`](@ref), [`replace_all_op1s!`](@ref)
"""
function replace_op1!(d::SummationDecapode, LHS::SummationDecapode, RHS::SummationDecapode)
  validate_op1_match(d, LHS)
  replace_op1!(d, only(LHS[:op1]), RHS)
end

"""    function replace_op1!(d::SummationDecapode, LHS::Symbol, RHS::Symbol)

Given a Decapode, d, replace at most one instance of the left-hand-side unary operator with that of the right-hand-side.

Return the index of the replaced unary operator, 0 if no match was found.
See also: [`replace_op2!`](@ref), [`replace_all_op1s!`](@ref)
"""
function replace_op1!(d::SummationDecapode, LHS::Symbol, RHS::Symbol)
  isempty(incident(d, LHS, :op1)) && return 0
  LHS_op1 = first(incident(d, LHS, :op1))
  d[LHS_op1, :op1] = RHS
  LHS_op1
end

"""    function replace_all_op1s!(d::SummationDecapode, LHS::Union{Symbol, SummationDecapode}, RHS::Union{Symbol, SummationDecapode})

Given a Decapode, d, replace all instances of the left-hand-side unary operator with those of the right-hand-side.

Return true if any replacements were made, otherwise false.

See also: [`replace_op1!`](@ref), [`replace_all_op2s!`](@ref)
"""
function replace_all_op1s!(d::SummationDecapode, LHS::Union{Symbol, SummationDecapode}, RHS::Union{Symbol, SummationDecapode})
  any_replaced = false
  while replace_op1!(d,LHS,RHS) != 0
    any_replaced = true
  end
  any_replaced
end

# Opening up Op2s
# --------------

# Validate whether LHS represents a valid op2.
function validate_op2_match(d::SummationDecapode, LHS::SummationDecapode)
  if nparts(LHS, :Op2) != 1
    error("Only single operator replacement is supported for now, but found Op2s: $(LHS[:op2])")
  end
end

# Validate whether RHS represents a valid replacement for an op2.
function validate_op2_replacement(d::SummationDecapode, LHS::Symbol, RHS::SummationDecapode, proj1, proj2)
  if length(infer_states(RHS)) != 2 || length(infer_terminals(RHS)) != 1
    error("The replacement for $(LHS) must have two inputs and a single output, but found inputs: $(RHS[infer_states(RHS), :name]) and outputs $(RHS[infer_terminals(RHS), :name])")
  end
  if !issetequal(infer_states(RHS), [proj1, proj2])
    error("The projections of the RHS of this replacement are not state variables. The projections are $(RHS[[proj1,proj2], :op2]) but the state variables are $(RHS[infer_states(RHS), :op2]).")
  end
end

"""    function replace_op2!(d::SummationDecapode, LHS::Symbol, RHS::SummationDecapode, proj1, proj2)

Given a Decapode, d, replace at most one instance of the left-hand-side binary operator with those of the right-hand-side.

proj1 and proj2 are the indices of the intended proj1 and proj2 in RHS.

Return the index of the replaced operator, 0 if no match was found.

See also: [`replace_op1!`](@ref), [`replace_all_op2s!`](@ref)
"""
function replace_op2!(d::SummationDecapode, LHS::Symbol, RHS::SummationDecapode, proj1, proj2)
  validate_op2_replacement(d, LHS, RHS, proj1, proj2)
  isempty(incident(d, LHS, :op2)) && return 0
  # Identify the "matched" operation.
  LHS_op2 = first(incident(d, LHS, :op2))
  LHS_proj1, LHS_proj2 = d[LHS_op2, :proj1], d[LHS_op2, :proj2]
  LHS_output = d[LHS_op2, :res]
  # Add in the "replace" operation(s).
  added_vars = copy_parts!(d, RHS).Var
  RHS_proj1, RHS_proj2 = intersect(infer_states(d), added_vars)
  # Preserve the order of proj1 and proj2.
  if d[RHS_proj1, :name] != RHS[proj1, :name]
    RHS_proj1, RHS_proj2 = RHS_proj2, RHS_proj1
  end
  RHS_output = only(intersect(infer_terminals(d), added_vars))
  # Transfer LHS_proj1's pointers to RHS_proj1.
  transfer_parents!(d, LHS_proj1, RHS_proj1)
  transfer_children!(d, LHS_proj1, RHS_proj1)
  d[RHS_proj1, :name] = d[LHS_proj1, :name]
  d[RHS_proj1, :type] = d[LHS_proj1, :type]
  # Transfer LHS_proj2's pointers to RHS_proj2.
  transfer_parents!(d, LHS_proj2, RHS_proj2)
  transfer_children!(d, LHS_proj2, RHS_proj2)
  d[RHS_proj2, :name] = d[LHS_proj2, :name]
  d[RHS_proj2, :type] = d[LHS_proj2, :type]
  # Transfer LHS_output's pointers to RHS_output.
  transfer_parents!(d, LHS_output, RHS_output)
  transfer_children!(d, LHS_output, RHS_output)
  d[RHS_output, :name] = d[LHS_output, :name]
  d[RHS_output, :type] = d[LHS_output, :type]
  # Remove the replaced match and variables.
  rem_parts!(d, :Var, sort!([LHS_proj1, LHS_proj2, LHS_output]))
  rem_part!(d, :Op2, LHS_op2)
  LHS_op2
end

"""    function replace_op2!(d::SummationDecapode, LHS::SummationDecapode, RHS::SummationDecapode, proj1, proj2)

Given a Decapode, d, replace at most one instance of the left-hand-side binary operator with those of the right-hand-side.

proj1 and proj2 are the indices of the intended proj1 and proj2 in RHS.

Return the index of the replaced binary operator, 0 if no match was found.
See also: [`replace_op1!`](@ref), [`replace_all_op2s!`](@ref)
"""
function replace_op2!(d::SummationDecapode, LHS::SummationDecapode, RHS::SummationDecapode, proj1, proj2)
  validate_op2_match(d, LHS)
  replace_op2!(d, only(LHS[:op2]), RHS, proj1, proj2)
end

"""    function replace_op2!(d::SummationDecapode, LHS::Symbol, RHS::Symbol)

Given a Decapode, d, replace at most one instance of the left-hand-side binary operator with that of the right-hand-side.

Return the index of the replaced binary operator, 0 if no match was found.
See also: [`replace_op1!`](@ref), [`replace_all_op2s!`](@ref)
"""
function replace_op2!(d::SummationDecapode, LHS::Symbol, RHS::Symbol)
  isempty(incident(d, LHS, :op2)) && return 0
  LHS_op2 = first(incident(d, LHS, :op2))
  d[LHS_op2, :op2] = RHS
  LHS_op2
end

# Ignoring proj1 and proj2 keeps replace_all_op2s! simple.
replace_op2!(d::SummationDecapode, LHS::Symbol, RHS::Symbol, proj1, proj2) =
  replace_op2!(d, LHS, RHS)

"""    function replace_all_op2s!(d::SummationDecapode, LHS::Union{Symbol, SummationDecapode}, RHS::Union{Symbol, SummationDecapode}, proj1, proj2)

Given a Decapode, d, replace all instances of the left-hand-side binary operator with those of the right-hand-side.

proj1 and proj2 are the indices of the intended proj1 and proj2 in RHS.

Return true if any replacements were made, otherwise false.

See also: [`replace_op2!`](@ref), [`replace_all_op1s!`](@ref)
"""
function replace_all_op2s!(d::SummationDecapode, LHS::Union{Symbol, SummationDecapode}, RHS::Union{Symbol, SummationDecapode}, proj1, proj2)
  any_replaced = false
  while replace_op2!(d,LHS,RHS, proj1, proj2) != 0
    any_replaced = true
  end
  any_replaced
end

"""    function replace_all_op2s!(d::SummationDecapode, LHS::Union{Symbol, SummationDecapode}, RHS::Union{Symbol, SummationDecapode})

Given a Decapode, d, replace all instances of the left-hand-side binary operator with those of the right-hand-side.

Search for distinguished variables "p1" and "p2" to serve as the proj1 and proj2 from RHS.

Return true if any replacements were made, otherwise false.

See also: [`replace_op2!`](@ref), [`replace_all_op1s!`](@ref)
"""
function replace_all_op2s!(d::SummationDecapode, LHS::Union{Symbol, SummationDecapode}, RHS::Union{Symbol, SummationDecapode})
  p1s = incident(RHS, :p1, :name)
  p2s = incident(RHS, :p2, :name)
  if length(p1s) != 1 || length(p2s) != 1
    error("proj1 and proj2 to use were not given, but unique distinguished variables p1 and p2 were not found. Found p1: $(p1s) and p2: $(p2s).")
  end
  proj1 = only(p1s)
  proj2 = only(p2s)
  any_replaced = false
  while replace_op2!(d,LHS,RHS, proj1, proj2) != 0
    any_replaced = true
  end
  any_replaced
end

