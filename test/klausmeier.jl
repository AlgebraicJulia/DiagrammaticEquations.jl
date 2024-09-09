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

@test_broken DecaSymbolic(lookup, Phytodynamics)

import .ThDEC: d, ⋆, SortError

ω, = @syms ω::PrimalFormT{1, :X, 2}

@test Δ(PrimalForm(1, X)) == PrimalForm(1, X)
@test symtype(Δ(ω)) == PrimalFormT{1, :X, 2}

# TODO propagating module information is suited for a macro
symbmodel = ps = DecaSymbolic(lookup, Phytodynamics, Main)

DecaExpr(symbmodel)



n = ps.vars[1]
SymbolicUtils.symtype(n)
Δ(n)

r = @rule Δ(~n) => ⋆(d(⋆(d(~n))))

t2 = r(Δ(n))
t2 |> dump


using SymbolicUtils.Rewriters
using SymbolicUtils: promote_symtype
r = @rule ⋆(⋆(~n)) => ~n
nested_star_cancel = Postwalk(Chain([r]))
nested_star_cancel(d(⋆(⋆(n))))
nsc = nested_star_cancel

isequal(nsc(⋆(⋆(d(n)))), d(n))
dump(nsc(⋆(⋆(d(n))))) 
dump(d(n))
⋆(⋆(d(⋆(⋆(n)))))
nsc(⋆(⋆(d(⋆(⋆(n))))))
nsc(nsc(⋆(⋆(d(⋆(⋆(n)))))))