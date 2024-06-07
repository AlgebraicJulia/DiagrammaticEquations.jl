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

"""    function replace_operators!(d::SummationDecapode, LHS::SummationDecapode, RHS::SummationDecapode)

Given a Decapode, d, replace all instances of the left-hand-side operator with those of the right-hand-side.
"""
function replace_operators!(d::SummationDecapode, LHS::SummationDecapode, RHS::SummationDecapode)
  # TODO: You could use `isomorphisms`.
  nparts(LHS, :Op1) == 1 || error("Only single operator replacement is supported for now")
  length(infer_states(RHS)) == 1 || error("Only single operator replacement is supported for now")
  while true
    isempty(incident(d, only(LHS[:op1]), :op1)) && break
    # Identify the "matched" operation.
    LHS_op1 = only(incident(d, only(LHS[:op1]), :op1))
    LHS_input = d[LHS_op1, :src]
    LHS_output = d[LHS_op1, :tgt]
    # Add in the "replace" operation(s).
    added_vars = copy_parts!(d, RHS).Var
    RHS_input = only(intersect(infer_states(d), added_vars))
    RHS_output = only(intersect(infer_terminals(d), added_vars))
    # Transfer LHS_input's children to RHS_input.
    transfer_children!(d, LHS_input, RHS_input)
    d[RHS_input, :name] = d[LHS_input, :name]
    d[RHS_input, :type] = d[LHS_input, :type]
    # Transfer LHS_output's parents to RHS_output.
    transfer_parents!(d, LHS_output, RHS_output)
    d[RHS_output, :name] = d[LHS_output, :name]
    d[RHS_output, :type] = d[LHS_output, :type]
    # Remove the replaced match and variables.
    rem_parts!(d, :Var, sort!([LHS_input, LHS_output]))
    rem_part!(d, :Op1, LHS_op1)
  end
  d
end

