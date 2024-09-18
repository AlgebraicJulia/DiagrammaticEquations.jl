using DiagrammaticEquations
using SymbolicUtils
using MLStyle

Heat = @decapode begin
  C::Form0
  G::Form1
  D::Constant
  ∂ₜ(G) == D*Δ(d(C))
end

infer_types!(Heat)

Brusselator = @decapode begin
  (U, V)::Form0
  U2V::Form0
  (U̇, V̇)::Form0

  (α)::Constant
  F::Parameter

  U2V == (U .* U) .* V

  U̇ == 1 + U2V - (4.4 * U) + (α * Δ(U)) + F
  V̇ == (3.4 * U) - U2V + (α * Δ(V))
  ∂ₜ(U) == U̇
  ∂ₜ(V) == V̇
end
infer_types!(Brusselator)

Phytodynamics = @decapode begin
  (n,w)::Form0
  m::Constant
  ∂ₜ(n) == w + m*n + Δ(n)
end
infer_types!(Phytodynamics)
test = to_acset(Phytodynamics, symbolic_rewriting(Phytodynamics))

# resolve_overloads!(Heat)

# lap_0_convert = @rule Δ₀(~x) => Δ(~x)
# lap_1_convert = @rule Δ₁(~x) => Δ(~x)
# lap_2_convert = @rule Δ₂(~x) => Δ(~x)

# d_0_convert = @rule d₀(~x) => d(~x)

# overloaders = [lap_0_convert, lap_1_convert, lap_2_convert, d_0_convert]

# lap_0_rule = @rule Δ(~x) => ⋆(d(⋆(d(~x))))
# lap_1_rule = @rule Δ(~x) => d(⋆(d(⋆(~x)))) + ⋆(d(⋆(d(~x))))
# lap_2_rule = @rule Δ(~x) => d(⋆(d(⋆(~x))))

# openers = [lap_0_rule, lap_1_rule, lap_2_rule]

r = rules(Δ, Val(1))

heat_exprs = symbolic_rewriting(Heat)

rewriter = SymbolicUtils.Fixpoint(SymbolicUtils.Prewalk(test_rule))

res_exprs =  apply_rewrites(heat_exprs, rewriter)

optm_dd_0 = @rule d(d(~x)) => 0
star_0 = @rule ★(0) => 0
d_0 = @rule d(0) => 0

optm_rewriter = SymbolicUtils.Postwalk(
  SymbolicUtils.Fixpoint(SymbolicUtils.Chain([optm_dd_0, star_0, d_0])))

res_merge_exprs = map(optm_rewriter, res_exprs)

deca_test = to_acset(Heat, res_exprs)
infer_types!(deca_test)
resolve_overloads!(deca_test)
