using DiagrammaticEquations
using SymbolicUtils
using MLStyle

test_space = Space(:X, 2)

test_type = Form(0, false, test_space)

Heat = @decapode begin
  C::Form0
  D::Constant
  ∂ₜ(C) == (D+2)*Δ(C)
end

infer_types!(Heat)
resolve_overloads!(Heat)

function oldtype_to_new(old::Symbol, space::Space = Space(:I, 2))::Sort
  @match old begin
    :Form0 => PrimalForm(0, space)
    :Form1 => PrimalForm(1, space)
    :Form2 => PrimalForm(2, space)

    :DualForm0 => DualForm(0, space)
    :DualForm1 => DualForm(1, space)
    :DualForm2 => DualForm(2, space)

    :Constant => Scalar()
    :Parameter => Scalar()
  end
end

function isform_zero(x)
  getmetadata(x, Sort).dim == 0
end

function isform_two(x)
  getmetadata(x, Sort).dim == 2
end

@syms Δ(x) d(x) ⋆(x)

@syms C D Ċ sum_1

C = setmetadata(C, Sort, oldtype_to_new(Heat[1, :type]))

lap_0_rule = @rule Δ(~x::(isform_zero)) => ⋆(d(⋆(d(~x))))
lap_2_rule = @rule Δ(~x::(isform_two)) => d(⋆(d(⋆(~x))))

test_eq = Δ(C)
lap_0_rule(test_eq)
lap_2_rule(test_eq) === nothing

rewriter = SymbolicUtils.Postwalk(SymbolicUtils.Chain([lap_0_rule]))
test_eq_long = (D + 2) * Δ(C)
rewriter(test_eq_long)

# Δ₀(x) = ⋆₀⁻¹(dual_d₁(⋆₁(d₀(x))))
# @syms Δ₀(x) d₀(x) ⋆₁(x) dual_d₁(x) ⋆₀⁻¹(x)
# lap_0_rule = @rule Δ₀(~x::(isform_zero)) => ⋆₀⁻¹(dual_d₁(⋆₁(d₀(~x))))
