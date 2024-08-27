using DiagrammaticEquations
using DiagrammaticEquations.SymbolicUtilsInterop

using Test
using MLStyle
using SymbolicUtils
using SymbolicUtils: BasicSymbolic

# See Klausmeier Equation 2.a
Hydrodynamics = @decapode begin
  (n,w)::DualForm0
  dX::Form1
  (a,ν)::Constant

  ∂ₜ(w) == a - w - w * n^2 + ν * L(dX, w)
end

# See Klausmeier Equation 2.b
Phytodynamics = @decapode begin
  (n,w)::DualForm0
  m::Constant

  ∂ₜ(n) == w * n^2 - m*n + Δ(n)
end

Hydrodynamics = parse_decapode(quote
  (n,w)::DualForm0
  dX::Form1
  (a,ν)::Constant

  ∂ₜ(w) == a - w - w  + ν * L(dX, w)
end)

X = Space(:X, 2)
lookup = SpaceLookup(X)
# DecaSymbolic(lookup, Hydrodynamics)

# See Klausmeier Equation 2.b
Phytodynamics = parse_decapode(quote
  (n,w)::Form0
  m::Constant
  ∂ₜ(n) == w - m*n + Δ(n)
end)

import .ThDEC: d, ⋆, SortError

@register Δ(s::Sort) begin
    @match s begin
        ::Scalar => throw(SortError("Scalar"))
        ::VField => throw(SortError("Nay!"))
        ::Form => ⋆(d(⋆(d(s))))
    end
end

ω, = @syms ω::PrimalFormT{1, :X, 2}

@test Δ(PrimalForm(1, X)) == PrimalForm(1, X)
@test Δ(ω) |> typeof == BasicSymbolic{PrimalFormT{1, :X, 2}}

DecaSymbolic(lookup, Phytodynamics)
