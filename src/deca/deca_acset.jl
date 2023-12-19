using ..DiagrammaticEquations

# TODO: You could write a method which auto-generates these rules given degree N.
"""
These are the default rules used to do type inference in the 1D exterior calculus.
"""
op1_inf_rules_1D = [
  # Rules for ∂ₜ 
  (src_type = :Form0, tgt_type = :Form0, op_names = [:∂ₜ,:dt]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [:∂ₜ,:dt]),

  # Rules for d
  (src_type = :Form0, tgt_type = :Form1, op_names = [:d, :d₀]),
  (src_type = :DualForm0, tgt_type = :DualForm1, op_names = [:d, :dual_d₀, :d̃₀]),

  # Rules for ⋆
  (src_type = :Form0, tgt_type = :DualForm1, op_names = [:⋆, :⋆₀, :star]),
  (src_type = :Form1, tgt_type = :DualForm0, op_names = [:⋆, :⋆₁, :star]),
  (src_type = :DualForm1, tgt_type = :Form0, op_names = [:⋆, :⋆₀⁻¹, :star_inv]),
  (src_type = :DualForm0, tgt_type = :Form1, op_names = [:⋆, :⋆₁⁻¹, :star_inv]),

  # Rules for Δ
  (src_type = :Form0, tgt_type = :Form0, op_names = [:Δ, :Δ₀, :lapl]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [:Δ, :Δ₁, :lapl]),

  # Rules for δ
  (src_type = :Form1, tgt_type = :Form0, op_names = [:δ, :δ₁, :codif]),

  # Rules for negation
  (src_type = :Form0, tgt_type = :Form0, op_names = [:neg, :(-)]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [:neg, :(-)])]

op2_inf_rules_1D = [
  # Rules for ∧₀₀, ∧₁₀, ∧₀₁
  (proj1_type = :Form0, proj2_type = :Form0, res_type = :Form0, op_names = [:∧, :∧₀₀, :wedge]),
  (proj1_type = :Form1, proj2_type = :Form0, res_type = :Form1, op_names = [:∧, :∧₁₀, :wedge]),
  (proj1_type = :Form0, proj2_type = :Form1, res_type = :Form1, op_names = [:∧, :∧₀₁, :wedge]),

  # Rules for L₀, L₁
  (proj1_type = :Form1, proj2_type = :Form0, res_type = :Form0, op_names = [:L, :L₀]),
  (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form1, op_names = [:L, :L₁]),    

  # Rules for i₁
  (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form0, op_names = [:i, :i₁]),

  # Rules for divison and multiplication
  (proj1_type = :Form0, proj2_type = :Form0, res_type = :Form0, op_names = [:./, :.*]),
  (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form1, op_names = [:./, :.*]),

  # WARNING: This parameter type inference might be wrong, depending on what the user gives as a parameter
  #= (proj1_type = :Parameter, proj2_type = :Form0, res_type = :Form0, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Parameter, proj2_type = :Form1, res_type = :Form1, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Parameter, proj2_type = :Form2, res_type = :Form2, op_names = [:/, :./, :*, :.*]),

  (proj1_type = :Form0, proj2_type = :Parameter, res_type = :Form0, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Form1, proj2_type = :Parameter, res_type = :Form1, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Form2, proj2_type = :Parameter, res_type = :Form2, op_names = [:/, :./, :*, :.*]),=#
  
  (proj1_type = :Literal, proj2_type = :Form0, res_type = :Form0, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Literal, proj2_type = :Form1, res_type = :Form1, op_names = [:/, :./, :*, :.*]),
  
  (proj1_type = :Form0, proj2_type = :Literal, res_type = :Form0, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Form1, proj2_type = :Literal, res_type = :Form1, op_names = [:/, :./, :*, :.*]),
  
  (proj1_type = :Constant, proj2_type = :Form0, res_type = :Form0, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Constant, proj2_type = :Form1, res_type = :Form1, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Form0, proj2_type = :Constant, res_type = :Form0, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Form1, proj2_type = :Constant, res_type = :Form1, op_names = [:/, :./, :*, :.*]),
  
  (proj1_type = :Constant, proj2_type = :DualForm0, res_type = :DualForm0, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Constant, proj2_type = :DualForm1, res_type = :DualForm1, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :DualForm0, proj2_type = :Constant, res_type = :DualForm0, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :DualForm1, proj2_type = :Constant, res_type = :DualForm1, op_names = [:/, :./, :*, :.*])]

"""
These are the default rules used to do type inference in the 2D exterior calculus.
"""
op1_inf_rules_2D = [
  # Rules for ∂ₜ
  (src_type = :Form0, tgt_type = :Form0, op_names = [:∂ₜ, :dt]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [:∂ₜ, :dt]),
  (src_type = :Form2, tgt_type = :Form2, op_names = [:∂ₜ, :dt]),

  # Rules for d
  (src_type = :Form0, tgt_type = :Form1, op_names = [:d, :d₀]),
  (src_type = :Form1, tgt_type = :Form2, op_names = [:d, :d₁]),
  (src_type = :DualForm0, tgt_type = :DualForm1, op_names = [:d, :dual_d₀, :d̃₀]),
  (src_type = :DualForm1, tgt_type = :DualForm2, op_names = [:d, :dual_d₁, :d̃₁]),

  # Rules for ⋆
  (src_type = :Form0, tgt_type = :DualForm2, op_names = [:⋆, :⋆₀, :star]),
  (src_type = :Form1, tgt_type = :DualForm1, op_names = [:⋆, :⋆₁, :star]),
  (src_type = :Form2, tgt_type = :DualForm0, op_names = [:⋆, :⋆₂, :star]),

  (src_type = :DualForm2, tgt_type = :Form0, op_names = [:⋆, :⋆₀⁻¹, :star_inv]),
  (src_type = :DualForm1, tgt_type = :Form1, op_names = [:⋆, :⋆₁⁻¹, :star_inv]),
  (src_type = :DualForm0, tgt_type = :Form2, op_names = [:⋆, :⋆₂⁻¹, :star_inv]),

  # Rules for Δ
  (src_type = :Form0, tgt_type = :Form0, op_names = [:Δ, :Δ₀, :lapl]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [:Δ, :Δ₁, :lapl]),
  (src_type = :Form2, tgt_type = :Form2, op_names = [:Δ, :Δ₂, :lapl]),

  # Rules for δ
  (src_type = :Form1, tgt_type = :Form0, op_names = [:δ, :δ₁, :codif]),
  (src_type = :Form2, tgt_type = :Form1, op_names = [:δ, :δ₂, :codif]),

  # Rules for negation
  (src_type = :Form0, tgt_type = :Form0, op_names = [:neg, :(-)]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [:neg, :(-)]),
  (src_type = :Form2, tgt_type = :Form2, op_names = [:neg, :(-)])]
    
op2_inf_rules_2D = vcat(op2_inf_rules_1D, [
  # Rules for ∧₁₁, ∧₂₀, ∧₀₂
  (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form2, op_names = [:∧, :∧₁₁, :wedge]),
  (proj1_type = :Form2, proj2_type = :Form0, res_type = :Form2, op_names = [:∧, :∧₂₀, :wedge]),
  (proj1_type = :Form0, proj2_type = :Form2, res_type = :Form2, op_names = [:∧, :∧₀₂, :wedge]),

  (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm2, op_names = [:L]),    

  # Rules for L₂
  (proj1_type = :Form1, proj2_type = :Form2, res_type = :Form2, op_names = [:L, :L₂]),    

  # Rules for i₁
  (proj1_type = :Form1, proj2_type = :Form2, res_type = :Form1, op_names = [:i, :i₁]),
  
  # Rules for subtraction
  (proj1_type = :Form0, proj2_type = :Form0, res_type = :Form0, op_names = [:-, :.-]),
  (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form1, op_names = [:-, :.-]),
  (proj1_type = :Form2, proj2_type = :Form2, res_type = :Form2, op_names = [:-, :.-]),
  (proj1_type = :DualForm0, proj2_type = :DualForm0, res_type = :DualForm0, op_names = [:-, :.-]),
  (proj1_type = :DualForm1, proj2_type = :DualForm1, res_type = :DualForm1, op_names = [:-, :.-]),
  (proj1_type = :DualForm2, proj2_type = :DualForm2, res_type = :DualForm2, op_names = [:-, :.-]),

  # Rules for divison and multiplication
  (proj1_type = :Form2, proj2_type = :Form2, res_type = :Form2, op_names = [:./, :.*]),
  (proj1_type = :Literal, proj2_type = :Form2, res_type = :Form2, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Form2, proj2_type = :Literal, res_type = :Form2, op_names = [:/, :./, :*, :.*])])
  
function apply_inference_rule_op1!(d::SummationDecapode, op1_id, rule)
  type_src = d[d[op1_id, :src], :type]
  type_tgt = d[d[op1_id, :tgt], :type]

  if(type_src != :infer && type_tgt != :infer)
    return false
  end

  score_src = (rule.src_type == type_src)
  score_tgt = (rule.tgt_type == type_tgt)
  check_op = (d[op1_id, :op1] in rule.op_names)

  if(check_op && (score_src + score_tgt == 1))
    d[d[op1_id, :src], :type] = rule.src_type
    d[d[op1_id, :tgt], :type] = rule.tgt_type
    return true
  end

  return false
end

function apply_inference_rule_op2!(d::SummationDecapode, op2_id, rule)
  type_proj1 = d[d[op2_id, :proj1], :type]
  type_proj2 = d[d[op2_id, :proj2], :type]
  type_res = d[d[op2_id, :res], :type]

  if(type_proj1 != :infer && type_proj2 != :infer && type_res != :infer)
    return false
  end

  score_proj1 = (rule.proj1_type == type_proj1)
  score_proj2 = (rule.proj2_type == type_proj2)
  score_res = (rule.res_type == type_res)
  check_op = (d[op2_id, :op2] in rule.op_names)

  if(check_op && (score_proj1 + score_proj2 + score_res == 2))
    d[d[op2_id, :proj1], :type] = rule.proj1_type
    d[d[op2_id, :proj2], :type] = rule.proj2_type
    d[d[op2_id, :res], :type] = rule.res_type
    return true
  end

  return false
end

ascii_to_unicode_op1 = Pair{Symbol, Any}[
                        (:dt       => :∂ₜ),
                        (:star     => :⋆),
                        (:lapl     => :Δ),
                        (:codif    => :δ),
                        (:star_inv => :⋆⁻¹)]

ascii_to_unicode_op2 = [
                        (:wedge    => :∧)]

"""    function unicode!(d::SummationDecapode)

Replace ASCII operators with their Unicode equivalents.
"""
unicode!(d::SummationDecapode) = replace_names!(d, ascii_to_unicode_op1, ascii_to_unicode_op2)

vec_to_dec_op1 = [
                  (:grad        => :d),
                  (:div         => [:⋆,:d,:⋆]),
                  (:curl        => [:d,:⋆]),
                  (:∇           => :d),
                  (Symbol("∇ᵈ") => [:⋆,:d,:⋆]),
                  # Note: This is x, not \times.
                  (Symbol("∇x") => [:d,:⋆])]

vec_to_dec_op2 = Pair{Symbol, Symbol}[]

"""    function vec_to_dec!(d::SummationDecapode)

Replace Vector Calculus operators with Discrete Exterior Calculus equivalents.
"""
function vec_to_dec!(d::SummationDecapode)
  # Perform simple substitutions.
  replace_names!(d, vec_to_dec_op1, vec_to_dec_op2)

  # Replace `adv` with divergence of ∧.
  advs = incident(d, :adv, :op2)
  adv_tgts = d[advs, :res]

  # Intermediate wedges.
  wedge_tgts = add_parts!(d, :Var, length(adv_tgts), name=map(i -> Symbol("•_adv_$i"), eachindex(advs)), type=:infer)
  # Divergences.
  add_parts!(d, :Op1, length(adv_tgts), src=wedge_tgts, tgt=adv_tgts, op1=fill([:⋆,:d,:⋆],length(advs)))
  # Point advs to the intermediates.
  d[collect(advs), :res] = wedge_tgts

  # Replace adv with ∧.
  d[collect(advs), :op2] = fill(:∧,length(advs))

  d
end

# TODO: When SummationDecapodes are annotated with the degree of their space,
# use dispatch to choose the correct set of rules.
"""    function resolve_overloads!(d::SummationDecapode)

Resolve function overloads based on types of src and tgt.
"""
resolve_overloads!(d::SummationDecapode) =
  resolve_overloads!(d, op1_res_rules_2D, op2_res_rules_2D)

