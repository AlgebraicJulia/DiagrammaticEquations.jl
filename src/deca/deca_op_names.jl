# HOW TO ADD NEW OPERATOR NAMES (Do steps 1-2 minimum)
# 1. Add a canon name, as `const NAME_FORMTYPES`
# 2. Add a ascii generic name, as `const NOFORM_NAME`
#
# 3. Add a unicode name, as const `UNICODE_NAME_FORMTYPES`
# 4. Add a unicode generic name, as const `NOFORM_UNICODE_NAME`
# 5. Add to CANON_NAMES
#     unicode name => canon name
#     unicode generic name => ascii generic name
#
# 6. Add aliases to CANON_NAMES
#     alias => canon name
#     generic alias => ascii generic name

# Canon names for DEC operations
# These should be ascii only and have their form inputs
# seperated from the name by an underscore afterwards (only :name_form#s).

# Op 1 names
const PARTIAL_T = :dt

const EXTDERIV_0 = :d_0
const EXTDERIV_1 = :d_1

const DUALDERIV_0 = :dual_d_0
const DUALDERIV_1 = :dual_d_1

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

# Canon names to use when form is not known, useful for type-inference, ascii only

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

# Unicode names with types, for use with overloading
# So if op is ∧ => ∧₀₁ but if is wdg => wdg_01

# Op1 names
const UNICODE_EXTDERIV_0 = :d₀
const UNICODE_EXTDERIV_1 = :d₁

const UNICODE_DUALDERIV_0 = :d̃₀
const UNICODE_DUALDERIV_1 = :d̃₁

const UNICODE_HODGE_0 = :⋆₀
const UNICODE_HODGE_1 = :⋆₁
const UNICODE_HODGE_2 = :⋆₂

const UNICODE_INVHODGE_0 = :⋆₀⁻¹
const UNICODE_INVHODGE_1 = :⋆₁⁻¹
const UNICODE_INVHODGE_2 = :⋆₂⁻¹

const UNICODE_LAPLACE_0 = :Δ₀
const UNICODE_LAPLACE_1 = :Δ₁
const UNICODE_LAPLACE_2 = :Δ₂

const UNICODE_CODIF_1 = :δ₁
const UNICODE_CODIF_2 = :δ₂

const UNICODE_AVG_01 = :avg₀₁

# Op2 names
const UNICODE_WEDGE_00 = :∧₀₀
const UNICODE_WEDGE_01 = :∧₀₁
const UNICODE_WEDGE_10 = :∧₁₀
const UNICODE_WEDGE_11 = :∧₁₁
const UNICODE_WEDGE_02 = :∧₀₂
const UNICODE_WEDGE_20 = :∧₂₀

const UNICODE_LIE_0 = :ℒ₀
const UNICODE_LIE_1 = :ℒ₁
const UNICODE_LIE_2 = :ℒ₂

const UNICODE_INNERPROD_1 = :ι₁
const UNICODE_INNERPROD_2 = :ι₂

# Generic unicode names for use with overloading
# So if op is ∧ => ∧₀₁ but if is wdg => wdg_01

# Op1 names
const NOFORM_UNICODE_EXTDERIV = NOFORM_EXTDERIV
const NOFORM_UNICODE_DUALDERIV = :d̃

const NOFORM_UNICODE_HODGE = :⋆
const NOFORM_UNICODE_INVHODGE = NOFORM_UNICODE_HODGE

const NOFORM_UNICODE_LAPLACE = :Δ
const NOFORM_UNICODE_CODIF = :δ

const NOFORM_UNICODE_AVG = NOFORM_AVG

# Op2 names
const NOFORM_UNICODE_WEDGE = :∧
const NOFORM_UNICODE_LIE = :ℒ
const NOFORM_UNICODE_INNERPROD = :ι

# ! Names should have no more than 2 or 3 alternatives
# TODO: Make this const
# We currently support the following groups of typed names
# dt, ∂ₜ
# d₀, d_0 | d₁, d_1 | d
# d̃₀, dual_d₀, dual_d_0 | d̃₁, dual_d₁, dual_d_1 | d
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
  UNICODE_EXTDERIV_0 => EXTDERIV_0,
  UNICODE_EXTDERIV_1 => EXTDERIV_1,

  NOFORM_UNICODE_EXTDERIV => NOFORM_EXTDERIV,

  # Dual derivatives
  UNICODE_DUALDERIV_0 => DUALDERIV_0,
  :dual_d₀ => DUALDERIV_0,

  UNICODE_DUALDERIV_1 => DUALDERIV_1,
  :dual_d₁ => DUALDERIV_1,

  NOFORM_UNICODE_DUALDERIV => NOFORM_DUALDERIV,

  # Hodge stars
  UNICODE_HODGE_0 => HODGE_0,
  UNICODE_HODGE_1 => HODGE_1,
  UNICODE_HODGE_2 => HODGE_2,

  NOFORM_UNICODE_HODGE => NOFORM_HODGE,

  # Inverse Hodge stars
  UNICODE_INVHODGE_0 => INVHODGE_0,
  UNICODE_INVHODGE_1 => INVHODGE_1,
  UNICODE_INVHODGE_2 => INVHODGE_2,

  NOFORM_UNICODE_INVHODGE => NOFORM_INVHODGE,

  # Laplacian
  UNICODE_LAPLACE_0 => LAPLACE_0,
  UNICODE_LAPLACE_1 => LAPLACE_1,
  UNICODE_LAPLACE_2 => LAPLACE_2,

  NOFORM_UNICODE_LAPLACE => NOFORM_LAPLACE,

  # Co-differential
  UNICODE_CODIF_1 => CODIF_1,
  UNICODE_CODIF_2 => CODIF_2,

  NOFORM_UNICODE_CODIF => NOFORM_CODIF,

  # Averaging
  UNICODE_AVG_01 => AVG_01,

  # Math
  :neg => NEG,
  :norm => MAG,

  # Wedge
  UNICODE_WEDGE_00 => WEDGE_00,
  UNICODE_WEDGE_01 => WEDGE_01,
  UNICODE_WEDGE_10 => WEDGE_10,
  UNICODE_WEDGE_11 => WEDGE_11,
  UNICODE_WEDGE_02 => WEDGE_02,
  UNICODE_WEDGE_20 => WEDGE_20,

  NOFORM_UNICODE_WEDGE => NOFORM_WEDGE,

  # Lie Derivative

  UNICODE_LIE_0 => LIE_0,
  :L₀ => LIE_0,

  UNICODE_LIE_1 => LIE_1,
  :L₁ => LIE_1,

  UNICODE_LIE_2 => LIE_2,
  :L₂ => LIE_2,

  NOFORM_UNICODE_LIE => NOFORM_LIE,

  # Inner Product

  UNICODE_INNERPROD_1 => INNERPROD_1,
  :i₁ => INNERPROD_1,

  UNICODE_INNERPROD_2 => INNERPROD_2,
  :i₂ => INNERPROD_2,

  NOFORM_UNICODE_INNERPROD => NOFORM_INNERPROD
)

get_canon_name(var::Symbol) = get(CANON_NAMES, var, var)
get_canon_name(var::AbstractVector{Symbol}) = return get_canon_name.(var)

# TODO: Name liable to change, needs to be clear but concise
deca_canon_op1(d::SummationDecapode, idx::Int) = get_canon_name(d[idx, :op1])
deca_canon_op1(d::SummationDecapode, idx::AbstractVector{Int}) = get_canon_name.(d[idx, :op1])

# TODO: Name liable to change, needs to be clear but concise
deca_canon_op2(d::SummationDecapode, idx::Int) = get_canon_name(d[idx, :op2])
deca_canon_op2(d::SummationDecapode, idx::AbstractVector{Int}) = get_canon_name.(d[idx, :op2])
