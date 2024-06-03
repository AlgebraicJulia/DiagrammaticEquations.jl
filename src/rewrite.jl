using Catlab

"""    function get_valid_op1s(deca_source::SummationDecapode, varID)

Searches SummationDecapode, deca_source, at the request varID
and returns all op1s which are allowed to be averaged. Returns
an array of indices of valid op1 sources.

Namely this is meant to exclude ∂ₜ from being included in an average.
"""
function get_valid_op1s(deca_source::SummationDecapode, varID)
    indices = incident(deca_source, varID, :tgt)
    filter!(x -> deca_source[x, :op1] ∉ [:∂ₜ,:dt], indices)
end
  
"""    function is_tgt_of_many_ops(d::SummationDecapode, var)

Return true if there are two or more distinct operations leading
into Var var (not counting ∂ₜ).
"""
function is_tgt_of_many_ops(d::SummationDecapode, var)
  op1Count = length(get_valid_op1s(d, var))
  op2Count = length(incident(d, var, :res))
  sumCount = length(incident(d, var, :sum))

  op1Count + op2Count + sumCount >= 2
end
  
"""    function find_tgts_of_many_ops(d::SummationDecapode)

Searches SummationDecapode, d, for all Vars which have two or
more distinct operations leading into the same variable.
"""
find_tgts_of_many_ops(d::SummationDecapode) =
  filter(var -> is_tgt_of_many_ops(d, var), parts(d, :Var))

function average_rewrite!(d::SummationDecapode)
  function add_var_and_inc(id_num,v)
    inter = add_part!(d, :Var,
      name=Symbol("oc$(id_num)_"*string(d[v,:name])),
      type=d[v,:type])
    inter, id_num+1
  end
  while !isempty(find_tgts_of_many_ops(d))
    id_num = 0
    v = first(find_tgts_of_many_ops(d))
    # 1. Add a new intermediate Var for each operation.
    int_op1s = map(get_valid_op1s(d, v)) do op1
      inter, id_num = add_var_and_inc(id_num,v)
      add_part!(d, :Op1, src=d[op1, :src], tgt=inter, op1=d[op1, :op1])
      inter
    end
    int_op2s = map(incident(d, v, :res)) do op2
      inter, id_num = add_var_and_inc(id_num,v)
      add_part!(d, :Op2, proj1=d[op2, :proj1], proj2=d[op2, :proj2], res=inter, op2=d[op2, :op2])
      inter
    end
    int_sums = map(incident(d, v, :sum)) do sum
      inter, id_num = add_var_and_inc(id_num,v)
      Sigma = add_part!(d, :Σ, sum=inter)
      d[incident(d, sum, :summation), :summation] = Sigma
      inter
    end
    # 2. Remove the old operations pointing into this Var.
    rem_parts!(d, :Op1, get_valid_op1s(d, v))
    rem_parts!(d, :Op2, incident(d, v, :res))
    rem_parts!(d, :Σ, incident(d, v, :sum))
    # 3. Compute this Var by taking the average of the intermediates.
    summed_inters, id_num = add_var_and_inc(id_num,v)
    Sigma = add_part!(d, :Σ, sum=summed_inters)
    inters = reduce(vcat, [int_op1s, int_op2s, int_sums])
    add_parts!(d, :Summand, summand=inters, summation=fill(Sigma,length(inters)), length(inters))
    lit = add_part!(d, :Var, name=Symbol(string(id_num-1)), type=:Literal)
    add_part!(d, :Op2, proj1=summed_inters, proj2=lit, res=v, op2=Symbol("/"))
  end
  d
end

"""    function average_rewrite(d::SummationDecapode)
 
Compute each quantitity in the given Decapode by the average of all computation paths leading to that node.
"""
function average_rewrite(d::SummationDecapode)
  e = SummationDecapode{Any, Any, Symbol}()
  copy_parts!(e, d, (:Var, :TVar, :Op1, :Op2, :Σ, :Summand))
  average_rewrite!(e)
  e
end

