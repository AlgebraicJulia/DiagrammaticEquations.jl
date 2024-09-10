using DiagrammaticEquations
using DiagrammaticEquations.SymbolicUtilsInterop
#
using Test
using MLStyle
using SymbolicUtils
using SymbolicUtils: BasicSymbolic, symtype

# See Klausmeier Equation 2.a
Hydrodynamics = @decapode begin
  (n,w)::DualForm0
  dX::Form1
  (a,ν)::Constant
  #
  ∂ₜ(w) == a - w - w * n^2 + ν * L(dX, w)
end

# See Klausmeier Equation 2.b
Phytodynamics = @decapode begin
  (n,w)::DualForm0
  m::Constant
  #
  ∂ₜ(n) == w * n^2 - m*n + Δ(n)
end

Hydrodynamics = parse_decapode(quote
  (n,w)::DualForm0
  dX::Form1
  (a,ν)::Constant
  #
  ∂ₜ(w) == a - w - w  + ν * L(dX, w)
end)

# See Klausmeier Equation 2.b
Phytodynamics = parse_decapode(quote
  (n,w)::Form0
  m::Constant
  ∂ₜ(n) == w - m*n + Δ(n)
end)

symbmodel = SymbolicContext(Phytodynamics)
dexpr = DecaExpr(symbmodel)
symbmodel′ = SymbolicContext(dexpr)
# TODO variables are the same but the equations don't match
