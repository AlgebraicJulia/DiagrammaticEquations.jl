using ..DiagrammaticEquations

# Canon names for DEC operations
# These should be ascii only, no symbols like :(-) or :(+) and have their form inputs
# seperated from the name by an underscore afterwards (only :name_form#s).

const PARTIAL_T = :dt

const EXTDERIV_0 = :d_0
const EXTDERIV_1 = :d_1
const EXTDERIV_2 = :d_2

const DUALDERIV_0 = :duald_0
const DUALDERIV_1 = :duald_1
const DUALDERIV_2 = :duald_2

const HODGE_0 = :hdg_0
const HODGE_1 = :hdg_1
const HODGE_2 = :hdg_2

const INVHODGE_0 = :invhdg_0
const INVHODGE_1 = :invhdg_1
const INVHODGE_2 = :invhdg_2

const LAPLACE_0 = :lapl_0
const LAPLACE_1 = :lapl_1
const LAPLACE_2 = :lapl_2

const CODIF_0 = :codif_0
const CODIF_1 = :codif_1
const CODIF_2 = :codif_2

const AVG_01 = :avg_01

const NEG = :neg
const MAG = :mag

# Canon names to use when form is not known, useful for type-inference

const NOFORM_EXTDERIV = :d
const NOFORM_DUALDERIV = NOFORM_EXTDERIV

const NOFORM_HODGE = :hdg
const NOFORM_INVHODGE = NOFORM_HODGE

const NOFORM_LAPLACE = :lapl
const NOFORM_CODIF = :codif

const NOFORM_AVG = :avg

# ! Names should have no more than 2 or 3 alternatives
# TODO: Make this const
CANON_OP1_NAMES = Dict{Symbol, Symbol}(
  # Partial time derivative
  :∂ₜ => PARTIAL_T,
  PARTIAL_T => PARTIAL_T,

  # Exterior derivatives
  :d₀ => EXTDERIV_0,
  EXTDERIV_0 => EXTDERIV_0,

  :d₁ => EXTDERIV_1,
  EXTDERIV_1 => EXTDERIV_1,

  :d₂ => EXTDERIV_2,
  EXTDERIV_2 => EXTDERIV_2,

  # Dual derivatives
  :d̃₀ => DUALDERIV_0,
  :dual_d₀ => DUALDERIV_0,
  :dual_d_0 => DUALDERIV_0,
  DUALDERIV_0 => DUALDERIV_0,

  :d̃₁ => DUALDERIV_1,
  :dual_d₁ => DUALDERIV_1,
  :dual_d_1 => DUALDERIV_1,
  DUALDERIV_1 => DUALDERIV_1,

  :d̃₂ => DUALDERIV_2,
  :dual_d₂ => DUALDERIV_2,
  :dual_d_2 => DUALDERIV_2,
  DUALDERIV_2 => DUALDERIV_2,

  :d̃ => NOFORM_EXTDERIV,

  # Hodge stars
  :⋆₀ => HODGE_0,
  :star_0 => HODGE_0,
  HODGE_0 => HODGE_0,

  :⋆₁ => HODGE_1,
  :star_1 => HODGE_1,
  HODGE_1 => HODGE_1,

  :⋆₂ => HODGE_2,
  :star_2 => HODGE_2,
  HODGE_2 => HODGE_2,

  :⋆ => NOFORM_HODGE,

  # Inverse Hodge stars
  :⋆₀⁻¹ => INVHODGE_0,
  :invstar_0 => INVHODGE_0,
  INVHODGE_0 => INVHODGE_0,

  :⋆₁⁻¹ => INVHODGE_1,
  :invstar_1 => INVHODGE_1,
  INVHODGE_1 => INVHODGE_1,

  :⋆₂⁻¹ => INVHODGE_2,
  :invstar_2 => INVHODGE_2,
  INVHODGE_2 => INVHODGE_2,

  :⋆ => NOFORM_HODGE,

  # Laplacian
  :Δ₀ => LAPLACE_0,
  LAPLACE_0 => LAPLACE_0,

  :Δ₁ => LAPLACE_1,
  LAPLACE_1 => LAPLACE_1,

  :Δ₂ => LAPLACE_2,
  LAPLACE_2 => LAPLACE_2,

  :Δ => NOFORM_LAPLACE,

  # Co-differential
  :δ₀ => CODIF_0,
  CODIF_0 => CODIF_0,

  :δ₁ => CODIF_1,
  CODIF_1 => CODIF_1,

  :δ₂ => CODIF_2,
  CODIF_2 => CODIF_2,

  :δ => NOFORM_CODIF,

  # Averaging
  :avg₀₁ => AVG_01,
  AVG_01 => AVG_01,

  # Math
  :(-) => NEG,
  NEG => NEG,

  :norm => MAG,
  MAG => MAG
)

# This should only be called with canon names
# TODO: Make this const
# TODO: Decide if we should do this explicity like below or programmatically using an
# agreed on canon naming convention
REMOVEFORM_OP1 = Dict{Symbol, Symbol}(
  EXTDERIV_0 => NOFORM_EXTDERIV,
  EXTDERIV_1 => NOFORM_EXTDERIV,
  EXTDERIV_2 => NOFORM_EXTDERIV,

  DUALDERIV_0 => NOFORM_EXTDERIV,
  DUALDERIV_1 => NOFORM_EXTDERIV,
  DUALDERIV_2 => NOFORM_EXTDERIV,

  HODGE_0 => NOFORM_HODGE,
  HODGE_1 => NOFORM_HODGE,
  HODGE_2 => NOFORM_HODGE,

  INVHODGE_0 => NOFORM_HODGE,
  INVHODGE_1 => NOFORM_HODGE,
  INVHODGE_2 => NOFORM_HODGE,

  LAPLACE_0 => NOFORM_LAPLACE,
  LAPLACE_1 => NOFORM_LAPLACE,
  LAPLACE_2 => NOFORM_LAPLACE,

  CODIF_0 => NOFORM_CODIF,
  CODIF_1 => NOFORM_CODIF,
  CODIF_2 => NOFORM_CODIF,

  AVG_01 => NOFORM_AVG
)

get_canon_name(var::Symbol) = get(CANON_OP1_NAMES, var, var)
get_canon_name(var::AbstractVector{Symbol}) = return get_formless_canon_name.(var)

function get_formless_canon_name(var::Symbol)
  canon_var = get_canon_name(var)
  get(REMOVEFORM_OP1, canon_var, canon_var)
end
get_formless_canon_name(var::AbstractVector{Symbol}) = return get_formless_canon_name.(var)

# TODO: Name liable to change, needs to be clear but concise
deca_canon_op1(d::SummationDecapode, idx::Int) = get_canon_name(d[idx, :op1])
deca_canon_op1(d::SummationDecapode, idx::AbstractVector{Int}) = get_canon_name.(d[idx, :op1])

# TODO: Name liable to change, needs to be clear but concise
deca_formless_op1(d::SummationDecapode, idx::Int) = get_formless_canon_name(d[idx, :op1])
deca_formless_op1(d::SummationDecapode, idx::AbstractVector{Int}) = get_formless_canon_name.(d[idx, :op1])

# TODO: You could write a method which auto-generates these rules given degree N.
"""
These are the default rules used to do type inference in the 1D exterior calculus.
"""
op1_inf_rules_1D = [
  # Rules for ∂ₜ
  (src_type = :Form0, tgt_type = :Form0, op_names = [PARTIAL_T]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [PARTIAL_T]),

  # Rules for d
  (src_type = :Form0, tgt_type = :Form1, op_names = [NOFORM_EXTDERIV]),
  (src_type = :DualForm0, tgt_type = :DualForm1, op_names = [NOFORM_DUALDERIV]),

  # Rules for ⋆
  (src_type = :Form0, tgt_type = :DualForm1, op_names = [NOFORM_HODGE]),
  (src_type = :Form1, tgt_type = :DualForm0, op_names = [NOFORM_HODGE]),
  (src_type = :DualForm1, tgt_type = :Form0, op_names = [NOFORM_INVHODGE]),
  (src_type = :DualForm0, tgt_type = :Form1, op_names = [NOFORM_INVHODGE]),

  # Rules for Δ
  (src_type = :Form0, tgt_type = :Form0, op_names = [NOFORM_LAPLACE]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [NOFORM_LAPLACE]),

  # Rules for δ
  (src_type = :Form1, tgt_type = :Form0, op_names = [NOFORM_CODIF]),

  # Rules for negation
  (src_type = :Form0, tgt_type = :Form0, op_names = [NEG]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [NEG]),

  # Rules for the averaging operator
  (src_type = :Form0, tgt_type = :Form1, op_names = [NOFORM_AVG]),

  # Rules for magnitude/ norm
  (src_type = :Form0, tgt_type = :Form0, op_names = [MAG]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [MAG])]

op2_inf_rules_1D = [
  # Rules for ∧₀₀, ∧₁₀, ∧₀₁
  (proj1_type = :Form0, proj2_type = :Form0, res_type = :Form0, op_names = [:∧, :∧₀₀, :wedge]),
  (proj1_type = :Form1, proj2_type = :Form0, res_type = :Form1, op_names = [:∧, :∧₁₀, :wedge]),
  (proj1_type = :Form0, proj2_type = :Form1, res_type = :Form1, op_names = [:∧, :∧₀₁, :wedge]),

  # Rules for L₀, L₁
  (proj1_type = :Form1, proj2_type = :DualForm0, res_type = :DualForm0, op_names = [:L, :L₀]),
  (proj1_type = :Form1, proj2_type = :DualForm1, res_type = :DualForm1, op_names = [:L, :L₁]),    

  # Rules for i₁
  (proj1_type = :Form1, proj2_type = :DualForm1, res_type = :DualForm0, op_names = [:i, :i₁]),

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
  (src_type = :Form0, tgt_type = :Form1, op_names = [NOFORM_EXTDERIV]),
  (src_type = :Form1, tgt_type = :Form2, op_names = [NOFORM_EXTDERIV]),
  (src_type = :DualForm0, tgt_type = :DualForm1, op_names = [NOFORM_DUALDERIV]),
  (src_type = :DualForm1, tgt_type = :DualForm2, op_names = [NOFORM_DUALDERIV]),

  # Rules for ⋆
  (src_type = :Form0, tgt_type = :DualForm2, op_names = [NOFORM_HODGE]),
  (src_type = :Form1, tgt_type = :DualForm1, op_names = [NOFORM_HODGE]),
  (src_type = :Form2, tgt_type = :DualForm0, op_names = [NOFORM_HODGE]),

  (src_type = :DualForm2, tgt_type = :Form0, op_names = [NOFORM_INVHODGE]),
  (src_type = :DualForm1, tgt_type = :Form1, op_names = [NOFORM_INVHODGE]),
  (src_type = :DualForm0, tgt_type = :Form2, op_names = [NOFORM_INVHODGE]),

  # Rules for Δ
  (src_type = :Form0, tgt_type = :Form0, op_names = [NOFORM_LAPLACE]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [NOFORM_LAPLACE]),
  (src_type = :Form2, tgt_type = :Form2, op_names = [NOFORM_LAPLACE]),

  # Rules for Δᵈ
  (src_type = :DualForm0, tgt_type = :DualForm0, op_names = [:Δᵈ₀]),
  (src_type = :DualForm1, tgt_type = :DualForm1, op_names = [:Δᵈ₁]),

  # Rules for δ
  (src_type = :Form1, tgt_type = :Form0, op_names = [NOFORM_CODIF]),
  (src_type = :Form2, tgt_type = :Form1, op_names = [NOFORM_CODIF]),

  # Rules for the averaging operator
  (src_type = :Form0, tgt_type = :Form1, op_names = [NOFORM_AVG]),

  # Rules for negation
  (src_type = :Form0, tgt_type = :Form0, op_names = [NEG]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [NEG]),
  (src_type = :Form2, tgt_type = :Form2, op_names = [NEG]),
  (src_type = :DualForm0, tgt_type = :DualForm0, op_names = [NEG]),
  (src_type = :DualForm1, tgt_type = :DualForm1, op_names = [NEG]),
  (src_type = :DualForm2, tgt_type = :DualForm2, op_names = [NEG]),

  # Rules for magnitude/ norm
  (src_type = :Form0, tgt_type = :Form0, op_names = [MAG]),
  (src_type = :Form1, tgt_type = :Form1, op_names = [MAG]),
  (src_type = :Form2, tgt_type = :Form2, op_names = [MAG]),
  (src_type = :DualForm0, tgt_type = :DualForm0, op_names = [MAG]),
  (src_type = :DualForm1, tgt_type = :DualForm1, op_names = [MAG]),
  (src_type = :DualForm2, tgt_type = :DualForm2, op_names = [MAG])]

op2_inf_rules_2D = vcat(op2_inf_rules_1D, [
  # Rules for ∧₁₁, ∧₂₀, ∧₀₂
  (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form2, op_names = [:∧, :∧₁₁, :wedge]),
  (proj1_type = :Form2, proj2_type = :Form0, res_type = :Form2, op_names = [:∧, :∧₂₀, :wedge]),
  (proj1_type = :Form0, proj2_type = :Form2, res_type = :Form2, op_names = [:∧, :∧₀₂, :wedge]),

  # Rules for L₂
  (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm2, op_names = [:L, :L₂]),    

  # Rules for i₁
  (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm1, op_names = [:i, :i₂]),

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
    # (src_type = :Form0, tgt_type = :DualForm1, resolved_name = :⋆₀, op = :star),
    # (src_type = :Form1, tgt_type = :DualForm0, resolved_name = :⋆₁, op = :star),
    # (src_type = :DualForm1, tgt_type = :Form0, resolved_name = :⋆₀⁻¹, op = :star),
    # (src_type = :DualForm0, tgt_type = :Form1, resolved_name = :⋆₁⁻¹, op = :star),
    # Rules for δ.
    (src_type = :Form1, tgt_type = :Form0, resolved_name = :δ₁, op = NOFORM_CODIF),
    # (src_type = :Form1, tgt_type = :Form0, resolved_name = :δ₁, op = :codif),
     # Rules for Δ
    (src_type = :Form0, tgt_type = :Form0, resolved_name = :Δ₀, op = NOFORM_LAPLACE),
    (src_type = :Form1, tgt_type = :Form1, resolved_name = :Δ₁, op = NOFORM_LAPLACE)]

  # We merge 1D and 2D rules since it seems op2 rules are metric-free. If
  # this assumption is false, this needs to change.
  op2_res_rules_1D = [
    # Rules for ∧.
    (proj1_type = :Form0, proj2_type = :Form0, res_type = :Form0, resolved_name = :∧₀₀, op = :∧),
    (proj1_type = :Form1, proj2_type = :Form0, res_type = :Form1, resolved_name = :∧₁₀, op = :∧),
    (proj1_type = :Form0, proj2_type = :Form1, res_type = :Form1, resolved_name = :∧₀₁, op = :∧),
    (proj1_type = :Form0, proj2_type = :Form0, res_type = :Form0, resolved_name = :∧₀₀, op = :wedge),
    (proj1_type = :Form1, proj2_type = :Form0, res_type = :Form1, resolved_name = :∧₁₀, op = :wedge),
    (proj1_type = :Form0, proj2_type = :Form1, res_type = :Form1, resolved_name = :∧₀₁, op = :wedge),
    # Rules for L.
    (proj1_type = :Form1, proj2_type = :DualForm0, res_type = :DualForm0, resolved_name = :L₀, op = :L),
    (proj1_type = :Form1, proj2_type = :DualForm1, res_type = :DualForm1, resolved_name = :L₁, op = :L),
    # Rules for i.
    (proj1_type = :Form1, proj2_type = :DualForm1, res_type = :DualForm0, resolved_name = :i₁, op = :i)]


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
    (src_type = :Form1, tgt_type = :Form1, resolved_name = :Δ₂, op = NOFORM_LAPLACE)]
    # (src_type = :Form0, tgt_type = :Form0, resolved_name = :Δ₀, op = :lapl),
    # (src_type = :Form1, tgt_type = :Form1, resolved_name = :Δ₁, op = :lapl),
    # (src_type = :Form1, tgt_type = :Form1, resolved_name = :Δ₂, op = :lapl)]

  # We merge 1D and 2D rules directly here since it seems op2 rules
  # are metric-free. If this assumption is false, this needs to change.
  op2_res_rules_2D = vcat(op2_res_rules_1D, [
    # Rules for ∧.
    (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form2, resolved_name = :∧₁₁, op = :∧),
    (proj1_type = :Form2, proj2_type = :Form0, res_type = :Form2, resolved_name = :∧₂₀, op = :∧),
    (proj1_type = :Form0, proj2_type = :Form2, res_type = :Form2, resolved_name = :∧₀₂, op = :∧),
    (proj1_type = :Form1, proj2_type = :Form1, res_type = :Form2, resolved_name = :∧₁₁, op = :wedge),
    (proj1_type = :Form2, proj2_type = :Form0, res_type = :Form2, resolved_name = :∧₂₀, op = :wedge),
    (proj1_type = :Form0, proj2_type = :Form2, res_type = :Form2, resolved_name = :∧₀₂, op = :wedge),
    # Rules for L.
    (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm2, resolved_name = :L₂, op = :L),
    # (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm2, resolved_name = :L₂ᵈ, op = :L),
    # Rules for i.
    (proj1_type = :Form1, proj2_type = :DualForm2, res_type = :DualForm1, resolved_name = :i₂, op = :i)])
    
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
