using ..DiagrammaticEquations

# HOW TO ADD NEW INFERENCE RULES
# 1. Add proper types
# 2. Set up proper consts for operator names
# 3. Set op_names to contain the generic name as well as the canon name
# TODO: You could write a method which auto-generates these rules given degree N.
"""
These are the default rules used to do type inference in the 1D exterior calculus.
"""
op1_inf_rules_1D = [
  # Rules for ∂ₜ
  (src_type = :Form0, tgt_type = :Form0, op_names = [PARTIAL_T]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [PARTIAL_T]),

  # Rules for d
  (src_type = :Form0, tgt_type = :Form1, op_names = [NOFORM_EXTDERIV, EXTDERIV_0]),
  (src_type = :DualForm0, tgt_type = :DualForm1, op_names = [NOFORM_DUALDERIV, DUALDERIV_1]),

  # Rules for ⋆
  (src_type = :Form0, tgt_type = :DualForm1, op_names = [NOFORM_HODGE, HODGE_0]),
  (src_type = :Form1, tgt_type = :DualForm0, op_names = [NOFORM_HODGE, HODGE_1]),
  (src_type = :DualForm1, tgt_type = :Form0, op_names = [NOFORM_INVHODGE, INVHODGE_0]),
  (src_type = :DualForm0, tgt_type = :Form1, op_names = [NOFORM_INVHODGE, INVHODGE_1]),

  # Rules for Δ
  (src_type = :Form0, tgt_type = :Form0, op_names = [NOFORM_LAPLACE, LAPLACE_0]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [NOFORM_LAPLACE, LAPLACE_1]),

  # Rules for δ
  (src_type = :Form1, tgt_type = :Form0, op_names = [NOFORM_CODIF, CODIF_1]),

  # Rules for negation
  (src_type = :Form0, tgt_type = :Form0, op_names = [NEG]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [NEG]),

  # Rules for the averaging operator
  (src_type = :Form0, tgt_type = :Form1, op_names = [NOFORM_AVG, AVG_01]),

  # Rules for magnitude/ norm
  (src_type = :Form0, tgt_type = :Form0, op_names = [MAG]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [MAG])]

op2_inf_rules_1D = [
  # Rules for ∧₀₀, ∧₁₀, ∧₀₁
  (proj1_type = :Form0, proj2_type = :Form0, res_type = :Form0, op_names = [NOFORM_WEDGE, WEDGE_00]),
  (proj1_type = :Form0, proj2_type = :Form1, res_type = :Form1, op_names = [NOFORM_WEDGE, WEDGE_01]),
  (proj1_type = :Form1, proj2_type = :Form0, res_type = :Form1, op_names = [NOFORM_WEDGE, WEDGE_10]),

  # Rules for L₀, L₁
  (proj1_type = :Form1, proj2_type = :DualForm0, res_type = :DualForm0, op_names = [NOFORM_LIE, LIE_0]),
  (proj1_type = :Form1, proj2_type = :DualForm1, res_type = :DualForm1, op_names = [NOFORM_LIE, LIE_1]),

  # Rules for i₁
  (proj1_type = :Form1, proj2_type = :DualForm1, res_type = :DualForm0, op_names = [NOFORM_INNERPROD, INNERPROD_1]),

  # Rules for divison and multiplication
  (proj1_type = :Form0, proj2_type = :Form0, res_type = :Form0, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form1, op_names = [:/, :./, :*, :.*, :^, :.^]),

  # WARNING: This parameter type inference might be wrong, depending on what the user gives as a parameter
  #= (proj1_type = :Parameter, proj2_type = :Form0, res_type = :Form0, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Parameter, proj2_type = :Form1, res_type = :Form1, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Parameter, proj2_type = :Form2, res_type = :Form2, op_names = [:/, :./, :*, :.*]),

  (proj1_type = :Form0, proj2_type = :Parameter, res_type = :Form0, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Form1, proj2_type = :Parameter, res_type = :Form1, op_names = [:/, :./, :*, :.*]),
  (proj1_type = :Form2, proj2_type = :Parameter, res_type = :Form2, op_names = [:/, :./, :*, :.*]),=#

  (proj1_type = :Form0, proj2_type = :Literal, res_type = :Form0, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :Form1, proj2_type = :Literal, res_type = :Form1, op_names = [:/, :./, :*, :.*, :^, :.^]),

  (proj1_type = :DualForm0, proj2_type = :Literal, res_type = :DualForm0, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :DualForm1, proj2_type = :Literal, res_type = :DualForm1, op_names = [:/, :./, :*, :.*, :^, :.^]),

  (proj1_type = :Literal, proj2_type = :Form0, res_type = :Form0, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :Literal, proj2_type = :Form1, res_type = :Form1, op_names = [:/, :./, :*, :.*, :^, :.^]),

  (proj1_type = :Literal, proj2_type = :DualForm0, res_type = :DualForm0, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :Literal, proj2_type = :DualForm1, res_type = :DualForm1, op_names = [:/, :./, :*, :.*, :^, :.^]),

  (proj1_type = :Constant, proj2_type = :Form0, res_type = :Form0, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :Constant, proj2_type = :Form1, res_type = :Form1, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :Form0, proj2_type = :Constant, res_type = :Form0, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :Form1, proj2_type = :Constant, res_type = :Form1, op_names = [:/, :./, :*, :.*, :^, :.^]),

  (proj1_type = :Constant, proj2_type = :DualForm0, res_type = :DualForm0, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :Constant, proj2_type = :DualForm1, res_type = :DualForm1, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :DualForm0, proj2_type = :Constant, res_type = :DualForm0, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :DualForm1, proj2_type = :Constant, res_type = :DualForm1, op_names = [:/, :./, :*, :.*, :^, :.^])]

"""
These are the default rules used to do type inference in the 2D exterior calculus.
"""
op1_inf_rules_2D = [
  # Rules for ∂ₜ
  (src_type = :Form0, tgt_type = :Form0, op_names = [PARTIAL_T]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [PARTIAL_T]),
  (src_type = :Form2, tgt_type = :Form2, op_names = [PARTIAL_T]),

  # Rules for d
  (src_type = :Form0, tgt_type = :Form1, op_names = [NOFORM_EXTDERIV, EXTDERIV_0]),
  (src_type = :Form1, tgt_type = :Form2, op_names = [NOFORM_EXTDERIV, EXTDERIV_1]),
  (src_type = :DualForm0, tgt_type = :DualForm1, op_names = [NOFORM_DUALDERIV, DUALDERIV_0]),
  (src_type = :DualForm1, tgt_type = :DualForm2, op_names = [NOFORM_DUALDERIV, DUALDERIV_1]),

  # Rules for ⋆
  (src_type = :Form0, tgt_type = :DualForm2, op_names = [NOFORM_HODGE, HODGE_0]),
  (src_type = :Form1, tgt_type = :DualForm1, op_names = [NOFORM_HODGE, HODGE_1]),
  (src_type = :Form2, tgt_type = :DualForm0, op_names = [NOFORM_HODGE, HODGE_2]),

  (src_type = :DualForm2, tgt_type = :Form0, op_names = [NOFORM_INVHODGE, INVHODGE_0]),
  (src_type = :DualForm1, tgt_type = :Form1, op_names = [NOFORM_INVHODGE, INVHODGE_1]),
  (src_type = :DualForm0, tgt_type = :Form2, op_names = [NOFORM_INVHODGE, INVHODGE_2]),

  # Rules for Δ
  (src_type = :Form0, tgt_type = :Form0, op_names = [NOFORM_LAPLACE, LAPLACE_0]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [NOFORM_LAPLACE, LAPLACE_1]),
  (src_type = :Form2, tgt_type = :Form2, op_names = [NOFORM_LAPLACE, LAPLACE_2]),

  # Rules for Δᵈ
  # TODO: Support this operator properly
  # TODO: Overloaded name is inconsistent with inv hodge
  (src_type = :DualForm0, tgt_type = :DualForm0, op_names = [:Δᵈ₀]),
  (src_type = :DualForm1, tgt_type = :DualForm1, op_names = [:Δᵈ₁]),

  # Rules for δ
  (src_type = :Form1, tgt_type = :Form0, op_names = [NOFORM_CODIF, CODIF_1]),
  (src_type = :Form2, tgt_type = :Form1, op_names = [NOFORM_CODIF, CODIF_2]),

  # Rules for the averaging operator
  (src_type = :Form0, tgt_type = :Form1, op_names = [NOFORM_AVG, AVG_01]),

  # Rules for negation and magnitude
  (src_type = :Form0, tgt_type = :Form0, op_names = [NEG, MAG]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [NEG, MAG]),
  (src_type = :Form2, tgt_type = :Form2, op_names = [NEG, MAG]),
  (src_type = :DualForm0, tgt_type = :DualForm0, op_names = [NEG, MAG]),
  (src_type = :DualForm1, tgt_type = :DualForm1, op_names = [NEG, MAG]),
  (src_type = :DualForm2, tgt_type = :DualForm2, op_names = [NEG, MAG])]

op2_inf_rules_2D = vcat(op2_inf_rules_1D, [
  # Rules for ∧₁₁, ∧₂₀, ∧₀₂
  (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form2, op_names = [NOFORM_WEDGE, WEDGE_11]),
  (proj1_type = :Form0, proj2_type = :Form2, res_type = :Form2, op_names = [NOFORM_WEDGE, WEDGE_02]),
  (proj1_type = :Form2, proj2_type = :Form0, res_type = :Form2, op_names = [NOFORM_WEDGE, WEDGE_20]),

  # Rules for L₂
  (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm2, op_names = [NOFORM_LIE, LIE_2]),

  # Rules for i₁
  (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm1, op_names = [NOFORM_INNERPROD, INNERPROD_2]),

  # Rules for ℒ
  (proj1_type = :DualForm1, proj2_type = :DualForm1, res_type = :DualForm1, op_names = [:ℒ₁]),

  # Rules for ι
  (proj1_type = :DualForm1, proj2_type = :DualForm1, res_type = :DualForm0, op_names = [:ι₁₁]),
  (proj1_type = :DualForm1, proj2_type = :DualForm2, res_type = :DualForm1, op_names = [:ι₁₂]),

  # Rules for subtraction
  (proj1_type = :Form0, proj2_type = :Form0, res_type = :Form0, op_names = [:-, :.-]),
  (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form1, op_names = [:-, :.-]),
  (proj1_type = :Form2, proj2_type = :Form2, res_type = :Form2, op_names = [:-, :.-]),
  (proj1_type = :DualForm0, proj2_type = :DualForm0, res_type = :DualForm0, op_names = [:-, :.-]),
  (proj1_type = :DualForm1, proj2_type = :DualForm1, res_type = :DualForm1, op_names = [:-, :.-]),
  (proj1_type = :DualForm2, proj2_type = :DualForm2, res_type = :DualForm2, op_names = [:-, :.-]),

  # Rules for divison, multiplication, and exponentiation.
  (proj1_type = :Form2, proj2_type = :Form2, res_type = :Form2, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :Literal, proj2_type = :Form2, res_type = :Form2, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :Form2, proj2_type = :Literal, res_type = :Form2, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :DualForm2, proj2_type = :DualForm2, res_type = :DualForm2, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :Literal, proj2_type = :DualForm2, res_type = :DualForm2, op_names = [:/, :./, :*, :.*, :^, :.^]),
  (proj1_type = :DualForm2, proj2_type = :Literal, res_type = :DualForm2, op_names = [:/, :./, :*, :.*, :^, :.^])])

  # HOW TO ADD NEW RESOLUTION RULES
  # 1. Add proper types
  # 2. Set up resolved name to be canon ascii name
  # 3. Set up op to be the generic ascii name
  # 4. For unicode operators, create another rule using the unicode variants of the above instead
  """
  These are the default rules used to do function resolution in the 1D exterior calculus.
  """
  op1_res_rules_1D = [
    # Rules for d.
    # TODO: Change resolved to EXTDERIV_0
    (src_type = :Form0, tgt_type = :Form1, resolved_name = :d₀, op = NOFORM_EXTDERIV),

    # TODO: Change resolved to DUALDERIV_0
    (src_type = :DualForm0, tgt_type = :DualForm1, resolved_name = :dual_d₀, op = NOFORM_DUALDERIV),
    (src_type = :DualForm0, tgt_type = :DualForm1, resolved_name = UNICODE_DUALDERIV_0, op = NOFORM_UNICODE_DUALDERIV),

    # Rules for ⋆.
    (src_type = :Form0, tgt_type = :DualForm1, resolved_name = HODGE_0, op = NOFORM_HODGE),
    (src_type = :Form0, tgt_type = :DualForm1, resolved_name = UNICODE_HODGE_0, op = NOFORM_UNICODE_HODGE),

    (src_type = :Form1, tgt_type = :DualForm0, resolved_name = HODGE_1, op = NOFORM_HODGE),
    (src_type = :Form1, tgt_type = :DualForm0, resolved_name = UNICODE_HODGE_1, op = NOFORM_UNICODE_HODGE),

    (src_type = :DualForm1, tgt_type = :Form0, resolved_name = INVHODGE_0, op = NOFORM_INVHODGE),
    (src_type = :DualForm1, tgt_type = :Form0, resolved_name = UNICODE_INVHODGE_0, op = NOFORM_UNICODE_INVHODGE),

    (src_type = :DualForm0, tgt_type = :Form1, resolved_name = INVHODGE_1, op = NOFORM_INVHODGE),
    (src_type = :DualForm0, tgt_type = :Form1, resolved_name = UNICODE_INVHODGE_1, op = NOFORM_UNICODE_INVHODGE),

    # Rules for δ.
    (src_type = :Form1, tgt_type = :Form0, resolved_name = CODIF_1, op = NOFORM_CODIF),
    (src_type = :Form1, tgt_type = :Form0, resolved_name = UNICODE_CODIF_1, op = NOFORM_UNICODE_CODIF),

    # Rules for Δ
    (src_type = :Form0, tgt_type = :Form0, resolved_name = LAPLACE_0, op = NOFORM_LAPLACE),
    (src_type = :Form0, tgt_type = :Form0, resolved_name = UNICODE_LAPLACE_0, op = NOFORM_UNICODE_LAPLACE),

    (src_type = :Form1, tgt_type = :Form1, resolved_name = LAPLACE_1, op = NOFORM_LAPLACE),
    (src_type = :Form1, tgt_type = :Form1, resolved_name = UNICODE_LAPLACE_1, op = NOFORM_UNICODE_LAPLACE),

    # TODO: Change resolved to AVG_01
    (src_type = :Form0, tgt_type = :Form1, resolved_name = :avg₀₁, op = NOFORM_AVG)]

  # We merge 1D and 2D rules since it seems op2 rules are metric-free. If
  # this assumption is false, this needs to change.
  op2_res_rules_1D = [
    # Rules for ∧.
    (proj1_type = :Form0, proj2_type = :Form0, res_type = :Form0, resolved_name = WEDGE_00, op = NOFORM_WEDGE),
    (proj1_type = :Form0, proj2_type = :Form0, res_type = :Form0, resolved_name = UNICODE_WEDGE_00, op = NOFORM_UNICODE_WEDGE),

    (proj1_type = :Form1, proj2_type = :Form0, res_type = :Form1, resolved_name = WEDGE_10, op = NOFORM_WEDGE),
    (proj1_type = :Form1, proj2_type = :Form0, res_type = :Form1, resolved_name = UNICODE_WEDGE_10, op = NOFORM_UNICODE_WEDGE),

    (proj1_type = :Form0, proj2_type = :Form1, res_type = :Form1, resolved_name = WEDGE_01, op = NOFORM_WEDGE),
    (proj1_type = :Form0, proj2_type = :Form1, res_type = :Form1, resolved_name = UNICODE_WEDGE_01, op = NOFORM_UNICODE_WEDGE),

    # Rules for L.
    # TODO: Change first resolved to LIE_0
    (proj1_type = :Form1, proj2_type = :DualForm0, res_type = :DualForm0, resolved_name = :L₀, op = NOFORM_LIE),
    (proj1_type = :Form1, proj2_type = :DualForm0, res_type = :DualForm0, resolved_name = UNICODE_LIE_0, op = NOFORM_UNICODE_LIE),

    # TODO: Change first resolved to LIE_1
    (proj1_type = :Form1, proj2_type = :DualForm1, res_type = :DualForm1, resolved_name = :L₁, op = NOFORM_LIE),
    (proj1_type = :Form1, proj2_type = :DualForm1, res_type = :DualForm1, resolved_name = UNICODE_LIE_1, op = NOFORM_UNICODE_LIE),

    # Rules for i.
    # TODO: Change first resolved to INNERPROD_1
    (proj1_type = :Form1, proj2_type = :DualForm1, res_type = :DualForm0, resolved_name = :i₁, op = NOFORM_INNERPROD),
    (proj1_type = :Form1, proj2_type = :DualForm1, res_type = :DualForm0, resolved_name = UNICODE_INNERPROD_1, op = NOFORM_UNICODE_INNERPROD)]


  """
  These are the default rules used to do function resolution in the 2D exterior calculus.
  """
  op1_res_rules_2D = [
    # Rules for d.
    # TODO: Change resolved to EXTDERIV_0
    (src_type = :Form0, tgt_type = :Form1, resolved_name = :d₀, op = NOFORM_EXTDERIV),

    # TODO: Change resolved to EXTDERIV_1
    (src_type = :Form1, tgt_type = :Form2, resolved_name = :d₁, op = NOFORM_EXTDERIV),

    # TODO: Change first resolved to DUALDERIV_0
    (src_type = :DualForm0, tgt_type = :DualForm1, resolved_name = :dual_d₀, op = NOFORM_DUALDERIV),
    (src_type = :DualForm0, tgt_type = :DualForm1, resolved_name = UNICODE_DUALDERIV_0, op = NOFORM_UNICODE_DUALDERIV),

    # TODO: Change first resolved to DUALDERIV_1
    (src_type = :DualForm1, tgt_type = :DualForm2, resolved_name = :dual_d₁, op = NOFORM_DUALDERIV),
    (src_type = :DualForm1, tgt_type = :DualForm2, resolved_name = UNICODE_DUALDERIV_1, op = NOFORM_UNICODE_DUALDERIV),

    # Rules for ⋆.
    (src_type = :Form0, tgt_type = :DualForm2, resolved_name = HODGE_0, op = NOFORM_HODGE),
    (src_type = :Form1, tgt_type = :DualForm1, resolved_name = HODGE_1, op = NOFORM_HODGE),
    (src_type = :Form2, tgt_type = :DualForm0, resolved_name = HODGE_2, op = NOFORM_HODGE),
    (src_type = :Form0, tgt_type = :DualForm2, resolved_name = UNICODE_HODGE_0, op = NOFORM_UNICODE_HODGE),
    (src_type = :Form1, tgt_type = :DualForm1, resolved_name = UNICODE_HODGE_1, op = NOFORM_UNICODE_HODGE),
    (src_type = :Form2, tgt_type = :DualForm0, resolved_name = UNICODE_HODGE_2, op = NOFORM_UNICODE_HODGE),

    (src_type = :DualForm2, tgt_type = :Form0, resolved_name = INVHODGE_0, op = NOFORM_INVHODGE),
    (src_type = :DualForm1, tgt_type = :Form1, resolved_name = INVHODGE_1, op = NOFORM_INVHODGE),
    (src_type = :DualForm0, tgt_type = :Form2, resolved_name = INVHODGE_2, op = NOFORM_INVHODGE),
    (src_type = :DualForm2, tgt_type = :Form0, resolved_name = UNICODE_INVHODGE_0, op = NOFORM_UNICODE_INVHODGE),
    (src_type = :DualForm1, tgt_type = :Form1, resolved_name = UNICODE_INVHODGE_1, op = NOFORM_UNICODE_INVHODGE),
    (src_type = :DualForm0, tgt_type = :Form2, resolved_name = UNICODE_INVHODGE_2, op = NOFORM_UNICODE_INVHODGE),

    # Rules for Δ.
    (src_type = :Form0, tgt_type = :Form0, resolved_name = LAPLACE_0, op = NOFORM_LAPLACE),
    (src_type = :Form0, tgt_type = :Form0, resolved_name = UNICODE_LAPLACE_0, op = NOFORM_UNICODE_LAPLACE),

    (src_type = :Form1, tgt_type = :Form1, resolved_name = LAPLACE_1, op = NOFORM_LAPLACE),
    (src_type = :Form1, tgt_type = :Form1, resolved_name = UNICODE_LAPLACE_1, op = NOFORM_UNICODE_LAPLACE),

    (src_type = :Form2, tgt_type = :Form2, resolved_name = LAPLACE_2, op = NOFORM_LAPLACE),
    (src_type = :Form2, tgt_type = :Form2, resolved_name = UNICODE_LAPLACE_2, op = NOFORM_UNICODE_LAPLACE),

    # Rules for δ.
    (src_type = :Form1, tgt_type = :Form0, resolved_name = CODIF_1, op = NOFORM_CODIF),
    (src_type = :Form1, tgt_type = :Form0, resolved_name = UNICODE_CODIF_1, op = NOFORM_UNICODE_CODIF),

    (src_type = :Form2, tgt_type = :Form1, resolved_name = CODIF_2, op = NOFORM_CODIF),
    (src_type = :Form2, tgt_type = :Form1, resolved_name = UNICODE_CODIF_2, op = NOFORM_UNICODE_CODIF),

    # Rules for ∇².
    # TODO: Call this :nabla2 in ASCII?
    # TODO: Do we support this operator anywhere?
    # TODO: Overloaded name of this operator is inconsistent with something like inv hodge
    (src_type = :Form0, tgt_type = :Form0, resolved_name = :∇²₀, op = :∇²),
    (src_type = :Form1, tgt_type = :Form1, resolved_name = :∇²₁, op = :∇²),
    (src_type = :Form2, tgt_type = :Form2, resolved_name = :∇²₂, op = :∇²),

    # TODO: Change resolved to AVG_01
    (src_type = :Form0, tgt_type = :Form1, resolved_name = :avg₀₁, op = NOFORM_AVG)]

  # We merge 1D and 2D rules directly here since it seems op2 rules
  # are metric-free. If this assumption is false, this needs to change.
  op2_res_rules_2D = vcat(op2_res_rules_1D, [
    # Rules for ∧.
    (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form2, resolved_name = WEDGE_11, op = NOFORM_WEDGE),
    (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form2, resolved_name = UNICODE_WEDGE_11, op = NOFORM_UNICODE_WEDGE),

    (proj1_type = :Form2, proj2_type = :Form0, res_type = :Form2, resolved_name = WEDGE_20, op = NOFORM_WEDGE),
    (proj1_type = :Form2, proj2_type = :Form0, res_type = :Form2, resolved_name = UNICODE_WEDGE_20, op = NOFORM_UNICODE_WEDGE),

    (proj1_type = :Form0, proj2_type = :Form2, res_type = :Form2, resolved_name = WEDGE_02, op = NOFORM_WEDGE),
    (proj1_type = :Form0, proj2_type = :Form2, res_type = :Form2, resolved_name = UNICODE_WEDGE_02, op = NOFORM_UNICODE_WEDGE),

    # Rules for L.
     # TODO: Change first resolved to LIE_2
    (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm2, resolved_name = :L₂, op = NOFORM_LIE),
    (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm2, resolved_name = UNICODE_LIE_2, op = NOFORM_UNICODE_LIE),
    # (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm2, resolved_name = :L₂ᵈ, op = :L),
    # Rules for i.

    # TODO: Change first resolved to INNERPROD_2
    (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm1, resolved_name = :i₂, op = NOFORM_INNERPROD),
    (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm1, resolved_name = UNICODE_INNERPROD_2, op = NOFORM_UNICODE_INNERPROD)])

# TODO: When SummationDecapodes are annotated with the degree of their space,
# use dispatch to choose the correct set of rules.
infer_types!(d::SummationDecapode) =
  infer_types!(d, op1_inf_rules_2D, op2_inf_rules_2D)

ascii_to_unicode_op1 = Pair{Symbol, Any}[
                        (:dt       => :∂ₜ),
                        (:star     => :⋆),
                        (:lapl     => :Δ),
                        (:codif    => :δ),
                        (:avg_01    => :avg₀₁),
                        (:star_inv => :⋆⁻¹)]

ascii_to_unicode_op2 = [
                        (:wedge    => :∧)]

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
