using ..DiagrammaticEquations

# Canon names for DEC operations
# These should be ascii only and have their form inputs
# seperated from the name by an underscore afterwards (only :name_form#s).

# Op 1 names
const PARTIAL_T = :dt

const EXTDERIV_0 = :d_0
const EXTDERIV_1 = :d_1

const DUALDERIV_0 = :duald_0
const DUALDERIV_1 = :duald_1

const HODGE_0 = :hdg_0
const HODGE_1 = :hdg_1
const HODGE_2 = :hdg_2

const INVHODGE_0 = :invhdg_0
const INVHODGE_1 = :invhdg_1
const INVHODGE_2 = :invhdg_2

const LAPLACE_0 = :lapl_0
const LAPLACE_1 = :lapl_1
const LAPLACE_2 = :lapl_2

const CODIF_1 = :codif_1
const CODIF_2 = :codif_2

const AVG_01 = :avg_01

const NEG = :(-)
const MAG = :mag

# Op 2 names

const WEDGE_00 = :wdg_00
const WEDGE_01 = :wdg_01
const WEDGE_10 = :wdg_10
const WEDGE_11 = :wdg_11
const WEDGE_02 = :wdg_02
const WEDGE_20 = :wdg_20

const LIE_0 = :L_0
const LIE_1 = :L_1
const LIE_2 = :L_2

const INNERPROD_1 = :i_1
const INNERPROD_2 = :i_2

# Canon names to use when form is not known, useful for type-inference

# Op1 names
const NOFORM_EXTDERIV = :d
const NOFORM_DUALDERIV = NOFORM_EXTDERIV

const NOFORM_HODGE = :hdg
const NOFORM_INVHODGE = NOFORM_HODGE

const NOFORM_LAPLACE = :lapl
const NOFORM_CODIF = :codif

const NOFORM_AVG = :avg

# Op2 names
const NOFORM_WEDGE = :wdg

const NOFORM_LIE = :L

const NOFORM_INNERPROD = :i

# ! Names should have no more than 2 or 3 alternatives
# TODO: Make this const
# We currently support the following groups of typed names
# dt, ∂ₜ
# d₀, d_0 | d₁, d_1 | d
# d̃₀, dual_d₀, duald_0 | d̃₁, dual_d₁, duald_1 | d
# ⋆₀, hdg_0 | ⋆₁, hdg_1 | ⋆₂, hdg_2 | hdg
# ⋆₀⁻¹, invhdg_0 | ⋆₁⁻¹, invhdg_1 | ⋆₂⁻¹, invhdg_2 | hdg
# Δ₀, lapl_0 | Δ₁, lapl_1 | Δ₂, lapl_2 | lapl
# δ₁, codif_1 | δ₂, codif_2 | codif
# avg₀₁, avg_01 | avg
# -, neg | mag, norm

CANON_NAMES = Dict{Symbol, Symbol}(
  # Partial time derivative
  :∂ₜ => PARTIAL_T,

  # Exterior derivatives
  :d₀ => EXTDERIV_0,
  :d₁ => EXTDERIV_1,

  # Dual derivatives
  :d̃₀ => DUALDERIV_0,
  :dual_d₀ => DUALDERIV_0,

  :d̃₁ => DUALDERIV_1,
  :dual_d₁ => DUALDERIV_1,

  :d̃ => NOFORM_EXTDERIV,

  # Hodge stars
  :⋆₀ => HODGE_0,
  :⋆₁ => HODGE_1,
  :⋆₂ => HODGE_2,

  :⋆ => NOFORM_HODGE,

  # Inverse Hodge stars
  :⋆₀⁻¹ => INVHODGE_0,
  :⋆₁⁻¹ => INVHODGE_1,
  :⋆₂⁻¹ => INVHODGE_2,

  :⋆ => NOFORM_HODGE,

  # Laplacian
  :Δ₀ => LAPLACE_0,
  :Δ₁ => LAPLACE_1,
  :Δ₂ => LAPLACE_2,

  :Δ => NOFORM_LAPLACE,

  # Co-differential
  :δ₁ => CODIF_1,
  :δ₂ => CODIF_2,
  :δ => NOFORM_CODIF,

  # Averaging
  :avg₀₁ => AVG_01,

  # Math
  :neg => NEG,
  :norm => MAG,

  # Wedge
  :∧₀₀ => WEDGE_00,
  :∧₀₁ => WEDGE_01,
  :∧₁₀ => WEDGE_10,
  :∧₁₁ => WEDGE_11,
  :∧₀₂ => WEDGE_02,
  :∧₂₀ => WEDGE_20,

  :∧ => NOFORM_WEDGE,

  # Lie Derivative

  :ℒ₀ => LIE_0,
  :L₀ => LIE_0,
  
  :ℒ₁ => LIE_1,
  :L₁ => LIE_1,

  :ℒ₂ => LIE_2,
  :L₂ => LIE_2,

  :ℒ => NOFORM_LIE,

  # Inner Product

  :ι₁ => INNERPROD_1,
  :i₁ => INNERPROD_1,

  :ι₂ => INNERPROD_2,
  :i₂ => INNERPROD_2,

  :ι => NOFORM_INNERPROD
)

get_canon_name(var::Symbol) = get(CANON_NAMES, var, var)
get_canon_name(var::AbstractVector{Symbol}) = return get_canon_name.(var)

# TODO: Name liable to change, needs to be clear but concise
deca_canon_op1(d::SummationDecapode, idx::Int) = get_canon_name(d[idx, :op1])
deca_canon_op1(d::SummationDecapode, idx::AbstractVector{Int}) = get_canon_name.(d[idx, :op1])

# TODO: Name liable to change, needs to be clear but concise
deca_canon_op2(d::SummationDecapode, idx::Int) = get_canon_name(d[idx, :op2])
deca_canon_op2(d::SummationDecapode, idx::AbstractVector{Int}) = get_canon_name.(d[idx, :op2])

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

  """
  These are the default rules used to do function resolution in the 1D exterior calculus.
  """
  op1_res_rules_1D = [
    # Rules for d.
    (src_type = :Form0, tgt_type = :Form1, resolved_name = :d₀, op = NOFORM_EXTDERIV),
    (src_type = :DualForm0, tgt_type = :DualForm1, resolved_name = :dual_d₀, op = NOFORM_DUALDERIV),
    # Rules for ⋆.
    (src_type = :Form0, tgt_type = :DualForm1, resolved_name = :⋆₀, op = NOFORM_HODGE),
    (src_type = :Form1, tgt_type = :DualForm0, resolved_name = :⋆₁, op = NOFORM_HODGE),
    (src_type = :DualForm1, tgt_type = :Form0, resolved_name = :⋆₀⁻¹, op = NOFORM_INVHODGE),
    (src_type = :DualForm0, tgt_type = :Form1, resolved_name = :⋆₁⁻¹, op = NOFORM_INVHODGE),
    # Rules for δ.
    (src_type = :Form1, tgt_type = :Form0, resolved_name = :δ₁, op = NOFORM_CODIF),
    # Rules for Δ
    (src_type = :Form0, tgt_type = :Form0, resolved_name = :Δ₀, op = NOFORM_LAPLACE),
    (src_type = :Form1, tgt_type = :Form1, resolved_name = :Δ₁, op = NOFORM_LAPLACE)]

  # We merge 1D and 2D rules since it seems op2 rules are metric-free. If
  # this assumption is false, this needs to change.
  op2_res_rules_1D = [
    # Rules for ∧.
    (proj1_type = :Form0, proj2_type = :Form0, res_type = :Form0, resolved_name = :∧₀₀, op = NOFORM_WEDGE),
    (proj1_type = :Form1, proj2_type = :Form0, res_type = :Form1, resolved_name = :∧₁₀, op = NOFORM_WEDGE),
    (proj1_type = :Form0, proj2_type = :Form1, res_type = :Form1, resolved_name = :∧₀₁, op = NOFORM_WEDGE),
    # (proj1_type = :Form0, proj2_type = :Form0, res_type = :Form0, resolved_name = :∧₀₀, op = :wedge),
    # (proj1_type = :Form1, proj2_type = :Form0, res_type = :Form1, resolved_name = :∧₁₀, op = :wedge),
    # (proj1_type = :Form0, proj2_type = :Form1, res_type = :Form1, resolved_name = :∧₀₁, op = :wedge),
    # Rules for L.
    (proj1_type = :Form1, proj2_type = :DualForm0, res_type = :DualForm0, resolved_name = :L₀, op = NOFORM_LIE),
    (proj1_type = :Form1, proj2_type = :DualForm1, res_type = :DualForm1, resolved_name = :L₁, op = NOFORM_LIE),
    # Rules for i.
    (proj1_type = :Form1, proj2_type = :DualForm1, res_type = :DualForm0, resolved_name = :i₁, op = NOFORM_INNERPROD)]


  """
  These are the default rules used to do function resolution in the 2D exterior calculus.
  """
  op1_res_rules_2D = [
    # Rules for d.
    (src_type = :Form0, tgt_type = :Form1, resolved_name = :d₀, op = NOFORM_EXTDERIV),
    (src_type = :Form1, tgt_type = :Form2, resolved_name = :d₁, op = NOFORM_EXTDERIV),
    (src_type = :DualForm0, tgt_type = :DualForm1, resolved_name = :dual_d₀, op = NOFORM_DUALDERIV),
    (src_type = :DualForm1, tgt_type = :DualForm2, resolved_name = :dual_d₁, op = NOFORM_DUALDERIV),
    # Rules for ⋆.
    (src_type = :Form0, tgt_type = :DualForm2, resolved_name = :⋆₀, op = NOFORM_HODGE),
    (src_type = :Form1, tgt_type = :DualForm1, resolved_name = :⋆₁, op = NOFORM_HODGE),
    (src_type = :Form2, tgt_type = :DualForm0, resolved_name = :⋆₂, op = NOFORM_HODGE),
    (src_type = :DualForm2, tgt_type = :Form0, resolved_name = :⋆₀⁻¹, op = NOFORM_INVHODGE),
    (src_type = :DualForm1, tgt_type = :Form1, resolved_name = :⋆₁⁻¹, op = NOFORM_INVHODGE),
    (src_type = :DualForm0, tgt_type = :Form2, resolved_name = :⋆₂⁻¹, op = NOFORM_INVHODGE),
    # (src_type = :Form0, tgt_type = :DualForm2, resolved_name = :⋆₀, op = :star),
    # (src_type = :Form1, tgt_type = :DualForm1, resolved_name = :⋆₁, op = :star),
    # (src_type = :Form2, tgt_type = :DualForm0, resolved_name = :⋆₂, op = :star),
    # (src_type = :DualForm2, tgt_type = :Form0, resolved_name = :⋆₀⁻¹, op = :star),
    # (src_type = :DualForm1, tgt_type = :Form1, resolved_name = :⋆₁⁻¹, op = :star),
    # (src_type = :DualForm0, tgt_type = :Form2, resolved_name = :⋆₂⁻¹, op = :star),
    # Rules for δ.
    (src_type = :Form2, tgt_type = :Form1, resolved_name = :δ₂, op = NOFORM_CODIF),
    (src_type = :Form1, tgt_type = :Form0, resolved_name = :δ₁, op = NOFORM_CODIF),
    # (src_type = :Form2, tgt_type = :Form1, resolved_name = :δ₂, op = :codif),
    # (src_type = :Form1, tgt_type = :Form0, resolved_name = :δ₁, op = :codif),
    # Rules for ∇².
    # TODO: Call this :nabla2 in ASCII?
    # TODO: Do we support this operator anywhere?
    # TODO: Overloaded name of this operator is inconsistent with something like inv hodge
    (src_type = :Form0, tgt_type = :Form0, resolved_name = :∇²₀, op = :∇²),
    (src_type = :Form1, tgt_type = :Form1, resolved_name = :∇²₁, op = :∇²),
    (src_type = :Form2, tgt_type = :Form2, resolved_name = :∇²₂, op = :∇²),
    # Rules for Δ.
    (src_type = :Form0, tgt_type = :Form0, resolved_name = :Δ₀, op = NOFORM_LAPLACE),
    (src_type = :Form1, tgt_type = :Form1, resolved_name = :Δ₁, op = NOFORM_LAPLACE),
    (src_type = :Form2, tgt_type = :Form2, resolved_name = :Δ₂, op = NOFORM_LAPLACE),
    # (src_type = :Form0, tgt_type = :Form0, resolved_name = :Δ₀, op = :lapl),
    # (src_type = :Form1, tgt_type = :Form1, resolved_name = :Δ₁, op = :lapl),
    # (src_type = :Form1, tgt_type = :Form1, resolved_name = :Δ₂, op = :lapl)]

    (src_type = :Form0, tgt_type = :Form1, resolved_name = :avg₀₁, op = NOFORM_AVG)]

  # We merge 1D and 2D rules directly here since it seems op2 rules
  # are metric-free. If this assumption is false, this needs to change.
  op2_res_rules_2D = vcat(op2_res_rules_1D, [
    # Rules for ∧.
    (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form2, resolved_name = :∧₁₁, op = NOFORM_WEDGE),
    (proj1_type = :Form2, proj2_type = :Form0, res_type = :Form2, resolved_name = :∧₂₀, op = NOFORM_WEDGE),
    (proj1_type = :Form0, proj2_type = :Form2, res_type = :Form2, resolved_name = :∧₀₂, op = NOFORM_WEDGE),
    # (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form2, resolved_name = :∧₁₁, op = :wedge),
    # (proj1_type = :Form2, proj2_type = :Form0, res_type = :Form2, resolved_name = :∧₂₀, op = :wedge),
    # (proj1_type = :Form0, proj2_type = :Form2, res_type = :Form2, resolved_name = :∧₀₂, op = :wedge),
    # Rules for L.
    (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm2, resolved_name = :L₂, op = NOFORM_LIE),
    # (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm2, resolved_name = :L₂ᵈ, op = :L),
    # Rules for i.
    (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm1, resolved_name = :i₂, op = NOFORM_INNERPROD)])

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
